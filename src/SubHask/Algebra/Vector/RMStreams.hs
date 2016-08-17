{-# LANGUAGE AllowAmbiguousTypes #-}
module SubHask.Algebra.Vector.RMStreams where

-- This module is based on "Recycle your Arrays" from Roman Leshchinskiy
--
--
-- Don't forget to inline your instances of these classes ;)

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Primitive
import Data.Functor ()
import Prelude ((.), ($), Int)

import SubHask.Algebra

-- | Primitive for Streaming operations on index i with value a
data Step i a = Yield a i -- ^ replace index with a
              | Skip i    -- ^ skip this index in this step
              | Done      -- ^ final value was the last one.

instance Functor (Step i) where
    fmap f (Yield a i) = Yield (f a) i
    fmap _ (Skip i)    = Skip i
    fmap _ Done        = Done

-- | A Stream can be thought of repeated application of the given function
--   from the given Seed (i) to form a List of Yield/Skip finishing with
--   Done.
--
--   i.e. in a Vector i would be the index and the Stream the Element/Index-Stream
--        on Lists it would be the normal recursion
--        on Trees it might be Breadth-First or Depth-First-Streaming
--
--   in short: everything you can serialize into and out of lists.

data Stream i a = Stream (i -> Step i a) !i {-# UNPACK #-} !Int

class Streamable s i a where
        stream :: s -> Stream i a
        unstream :: Stream i a -> s

{-# RULES
"stream/unstream" forall (t :: forall i a. Stream i a). stream (unstream t) = t
  #-}

-- | The MStream composed of a next action in the monad m, the index it works on and the size of the structure
data MStream m i a = MStream (i -> m (Step i a)) !i {-# UNPACK #-} !Int


-- | streamM . runIdentity . unstream gets fused away, chaining the MStream m s i a -> MStream m s i a functions.
class (Monad m) => MStreamable m s i a where
        streamM :: s -> MStream m i a
        unstreamM :: MStream m i a -> m s

-- | if we choose m == Identity then we recover Stream, but we have to tell the compiler about the identity.

-- instance MStreamable Identity s i => Streamable s i where
--         stream s = let (MStream nextM i n) = streamM s in Stream (runIdentity . nextM) i n
--         unstream (Stream next i n) = runIdentity $ unstreamM (MStream (return . next) i n)

-- both - above and below work. comment both in -> compiler-loop.
instance Streamable s i a => MStreamable Identity s i a where
        streamM s = let (Stream next i n) = stream s in MStream (return . next) i n
        unstreamM (MStream next i n) = return . unstream $ Stream (runIdentity . next) i n


-- | manual function to convert Streams to MStream Identity.
--
-- Automatic instancing of STreamable for MStream Identity not possible becaus of undicidability
-- of choosing which one to apply and thus looping endlessly in the type-checker/optimizer.
{-# INLINE_FUSION liftMStream #-}
liftMStream :: Monad m => Stream i a -> MStream m i a
liftMStream (Stream next i n) = MStream (return . next) i n

-- | and the way back
{-# INLINE_FUSION lowerIdentity #-}
lowerIdentity :: MStream Identity i a -> Stream i a
lowerIdentity (MStream next i n) = Stream (runIdentity . next) i n

-- | And this rule recaptures the stream . unstream-Identity from above.

{-# RULES
"streamM/unstreamM"[~0] forall (t :: forall i a. MStream Identity i a). streamM (runIdentity (unstreamM t)) = t
"lift/lower-Stream"[~1] forall s . lowerIdentity (liftMStream s) = s
  #-}


-- | Wrapper for signaling a creation of a value
newtype (PrimBase m) => New m mutable = New (m mutable)

-- | Recycleable is used for collection of inplace-updates on indexed-based structures
--   (read: Arrays) without exposing the mutability and performing safe optimisations.
--
--   Concatenation of these functions works similar to a Stream - but in every step the
--   whole structure can get updated.
--
--   clone . new gets fused away chaining New ds -> New ds functions.
class (PrimBase m) => Recycleable m ds r where
        new :: New m ds -> r
        clone :: r -> New m ds


{-# RULES
"clone/new"[~0] forall p. clone (new p) = p
  #-}

{-# INLINE[1] newOp #-}
newOp :: (PrimBase m) => (ds -> m ds) -> New m ds -> New m ds
newOp f (New init) = New $ do
                            v <- init
                            f v


-- | combining interface of Streams and Recycles yielding more optimisations.
--   
--   Recycleabe and monadic Streams are incompatible, but if we lift a un-monadic
--   Stream to any monad we can recover the original Stream-fusion (by using the
--   Identity-Monad) while also allowing for Usage of the ST-Monad for recycling.
--
--   Defining fill is sufficient and GHC-Rules replace all occurences of
--   unstream and clone with the fill-based definition which then gets
--   optimised away by further rules.
class Fillable m ds i a where
        fill :: Stream i a -> New m ds

class (PrimMonad m, Streamable ds i a, Streamable s i a, Recycleable m ds s, Fillable m ds i a) => RMStreams m s ds i a

{-# INLINE[1] unstream_ #-}
unstream_ :: forall m s ds i a. RMStreams m s ds i a => Stream i a -> s
unstream_ s = (new :: Recycleable m ds s => New m ds -> s) ((fill :: Fillable m ds i a => Stream i a -> New m ds) s)

{-# INLINE[1] clone_ #-}
clone_ :: forall m s ds i a. RMStreams m s ds i a => s -> New m ds
clone_ s = (fill :: Fillable m ds i a => Stream i a -> New m ds) ((stream :: Streamable s i a => s -> Stream i a) s)

{-# INLINE[1] transform #-}
transform :: (IxContainer ds, RMStreams m s ds i a) => (Stream i a -> Stream i a) -> New m ds -> New m ds
transform f (New init) = New $ do
                       v <- init
                       return $ unstream (f (stream v))

-- | functions that do not change the type can be done inplace. Definition is id but used in GHC-Rules.
{-# INLINE[1] inplace #-}
inplace :: (forall m. Monad m => MStream m i a -> MStream m i a) -> Stream i a -> Stream i a
inplace f = lowerIdentity . f . liftMStream
-- not sure if the above works.

{-# RULES
-- can't get this specialisation typechecked ...
-- "unstream/fill_unstream" forall (t :: RMStreams m s ds i a => Stream i a). unstream t = unstream_ t
-- "clone/fill_clone" forall (t :: (Recycleable s i a, RMStreams ds s i a) => s). clone t = clone_ t :: (Recycleable s i a, RMStreams ds s i a) => New ds
"fusion"[1] forall s. stream (new (fill s)) = s
"recycling"[1] forall p. fill (stream (new p)) = p
-- "embed"[1] forall (f :: Stream i a -> Stream i a) (p :: (IxContainer ds, RMStreams m s ds i a) => New m ds). fill (f (stream (new p))) = transform f p
-- "unembed"[1] forall (f :: Stream i a -> Stream i a) (p :: (IxContainer ds, RMStreams m ds s i a) => New m ds). stream (new (transform f p)) = f (stream (new p))
-- "tfusion"[1] forall f g p. transform f (transform g p) = transform (f . g) p
  #-}
