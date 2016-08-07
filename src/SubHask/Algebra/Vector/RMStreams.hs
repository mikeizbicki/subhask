{-# LANGUAGE AllowAmbiguousTypes #-}
module SubHask.Algebra.Vector.RMStreams where

-- This module is based on "Recycle your Arrays" from Roman Leshchinskiy
--
--
-- Don't forget to inline your instances of these classes ;)

import Control.Monad
import Control.Monad.Identity
import Control.Monad.ST
import Data.Functor ()
import Prelude ((.), ($), Int)

--import SubHask.Internal.Prelude

-- | Primitive for Streaming operations on index i with value a
data Step i a = Yield a i -- ^ replace index with a
              | Skip i    -- ^ skip this index in this step
              | Done      -- ^ final value was the last one.

instance Functor (Step i) where
    fmap f (Yield a i) = Yield (f a) i
    fmap _ (Skip i)    = Skip i
    fmap _ Done        = Done

data Stream (s :: * -> * -> *) i a = Stream (i -> Step i a) !i {-# UNPACK #-} !Int

class Streamable s i where
        stream :: s i a -> Stream s i a
        unstream :: Stream s i a -> s i a

{-# RULES
"stream/unstream" forall (t :: forall s i a. Stream s i a). stream (unstream t) = t
  #-}

-- | The MStream composed of a next action in the monad m, the index it works on and the size of the structure
data MStream m (s :: * -> * -> *) i a = MStream (i -> m (Step i a)) !i {-# UNPACK #-} !Int


-- | streamM . runIdentity . unstream gets fused away, chaining the MStream m s i a -> MStream m s i a functions.
class (Monad m) => MStreamable m s i where
        streamM :: s i a -> MStream m s i a
        unstreamM :: MStream m s i a -> m (s i a)

-- | if we choose m == Identity then we recover Stream, but we have to tell the compiler about the identity.

-- instance MStreamable Identity s i => Streamable s i where
--         stream s = let (MStream nextM i n) = streamM s in Stream (runIdentity . nextM) i n
--         unstream (Stream next i n) = runIdentity $ unstreamM (MStream (return . next) i n)

-- both - above and below work. comment both in -> compiler-loop.
instance Streamable s i => MStreamable Identity s i where
        streamM s = let (Stream next i n) = stream s in MStream (return . next) i n
        unstreamM (MStream next i n) = return . unstream $ Stream (runIdentity . next) i n


-- | manual function to convert Streams to MStream Identity.
--
-- Automatic instancing of STreamable for MStream Identity not possible becaus of undicidability
-- of choosing which one to apply and thus looping endlessly in the type-checker/optimizer.
{-# INLINE[1] liftIdentity #-}
liftIdentity :: Stream s i a -> MStream Identity s i a
liftIdentity (Stream next i n) = MStream (return . next) i n

-- | and the way back
{-# INLINE[1] lowerIdentity #-}
lowerIdentity :: MStream Identity s i a -> Stream s i a
lowerIdentity (MStream next i n) = Stream (runIdentity . next) i n

-- | And this rule recaptures the stream . unstream-Identity from above.

{-# RULES
"streamM/unstreamM" forall (t :: forall s i a. MStream Identity s i a). streamM (runIdentity (unstreamM t)) = t
"lift/lower-Stream[1]"[~1] forall s . liftIdentity (lowerIdentity s) = s
"lift/lower-Stream[2]"[~1] forall s . lowerIdentity (liftIdentity s) = s
  #-}


-- | Wrapper for signaling a creation of a value
newtype New mutable i a = New (ST i (mutable i a))

-- | clone . new gets fused away chaining New r a -> New r a functions.
class Recycleable r where
        new :: New r i a -> r i a
        clone :: r i a -> New r i a


{-# RULES
"clone/new" forall p. clone (new p) = p
  #-}

-- | combining interface of Streams and Recycles yielding more optimisations.
--   
--   Defining fill is sufficient and GHC-Rules replace all occurences of
--   unstream and clone with the fill-based definition which then gets
--   optimised away by further rules.
class (Monad m, MStreamable m s i, Recycleable s, m ~ ST i) => RMStreams m s i where
        fill :: Stream s i a -> New s i a

        {-# INLINE unstream_ #-}
        unstream_ :: Stream s i a-> s i a
        unstream_ s = new (fill s)

        {-# INLINE clone_ #-}
        clone_ :: Streamable s i => s i a -> New s i a
        clone_ s = fill (stream s)

        {-# INLINE transform #-}
        transform :: (forall n. Monad n => MStream n s i a -> MStream n s i a) -> New s i a -> New s i a
        transform f (New init) = New $ do 
                                       v <- init
                                       unstreamM (f (streamM v))

        -- | functions that do not change the type can be done inplace. Definition is id but used in GHC-Rules.
        {-# INLINE inplace #-}
        inplace :: (forall n. Monad n => MStream n s i a -> MStream n s i a) -> MStream Identity s i a -> MStream Identity s i a
        inplace f = f

{- # RULES
"unstream/fill_unstream" forall (t :: (Streamable m s i, RMStreams m s i) => MStream m s i a). unstream t = unstream_ t :: (Streamable m s i, RMStreams m s i) => m (s i a)
"clone/fill_clone" forall (t :: (Recycleable s, RMStreams m s i) => s i a). clone t = clone_ t :: (Recycleable s, RMStreams m s i) => New s i a
"fusion" forall s. stream (new (fill s)) = s
"recycling" forall p. fill (stream (new p)) = p
"inplace" forall (f :: forall m. Monad m => MStream m s i a -> MStream m s i a) p. fill (inplace f (stream (new p))) = transform (f :: forall m. Monad m => MStream m s i a -> MStream m s i a) p
"uninplace" forall (f :: forall m. Monad m => MStream m s i a -> MStream m s i a) (p :: RMStreams m s i => New s i a). stream (new (transform f p)) = inplace f (stream (new p)) :: RMStreams m s i => MStream m s i a
"inplace2" forall (f :: forall m. MStream m s i a -> MStream m s i a) (g :: forall m. MStream m s i a -> MStream m s i a) (s :: RMStreams m r i => MStream m s i a). inplace f (inplace g s) = inplace ((f . g) :: RMStreams m r i => forall m . MStream m s i a -> MStream m s i a) s :: MStream m s i a
"mfusion" forall (f :: forall m. MStream m s i a -> MStream m s i a) (g :: forall m. MStream m s i a -> MStream m s i a) (p :: New s i a). transform f (transform g p) = transform (f . g :: forall m. MStream m s i a -> MStream m s i a) p :: New s i a
  # -}
