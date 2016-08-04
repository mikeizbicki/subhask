{-# LANGUAGE AllowAmbiguousTypes #-}
module SubHask.Algebra.Vector.RMStreams where

-- This module is based on "Recycle your Arrays" from Roman Leshchinskiy
--
--
-- Don't forget to inline your instances of these classes ;)

import Control.Monad
import Control.Monad.Identity
import Data.Functor
import Prelude ((.), ($))

import SubHask.Internal.Prelude

-- | Primitive for Streaming operations on index i with value a
data Step i a = Yield a i -- ^ replace index with a
              | Skip i    -- ^ skip this index in this step
              | Done      -- ^ final value was the last one.

instance Functor (Step i) where
    fmap f (Yield a i) = Yield (f a) i
    fmap _ (Skip i)    = Skip i
    fmap _ Done        = Done

-- | The MStream composed of a next action in the monad m, the index it works on and the size of the structure
data MStream m i a = MStream (i -> m (Step i a)) {-# UNBOX #-} !i {-# UNBOX #-} !Int


-- | stream . unstream gets fused away, chaining the MStream i a -> MStream i a functions.
class (Monad m) => Streamable m s i where
        stream :: s i a -> MStream m i a
        unstream :: MStream m i a -> m (s i a)

{-# RULES
"stream/unstream" forall (t :: forall i a. MStream Identity i a). stream (unstream t) = t
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
class (Monad m, Streamable m s i, Recycleable s, m ~ ST i) => RMStreams m s i where
        fill :: MStream m i a -> New s i a

        {-# INLINE unstream_ #-}
        unstream_ :: MStream m i a-> m (s i a)
        unstream_ s = return $ new (fill s)

        {-# INLINE clone_ #-}
        clone_ :: s i a -> New s i a
        clone_ s = fill ((stream :: s i a -> MStream m i a) s)

        {-# INLINE transform #-}
        transform :: (forall m. Monad m => MStream m i a -> MStream m i a) -> New s i a -> New s i a
        transform f (New init) = New $ do 
                                       v <- init
                                       (unstream_ :: MStream (ST i) i a -> ST i (s i a)) (f ((stream :: s i a -> MStream (ST i) i a) v))

        -- | functions that do not change the type can be done inplace. Definition is id but used in GHC-Rules.
        {-# INLINE inplace #-}
        inplace :: (forall m. Monad m => MStream m i a -> MStream m i a) -> MStream m i a -> MStream m i a
        inplace f = f

{-# RULES
"unstream/fill_unstream" forall (t :: (Streamable m s i, RMStreams m s i) => MStream m i a). unstream t = unstream_ t :: (Streamable m s i, RMStreams m s i) => m (s i a)
"clone/fill_clone" forall (t :: (Recycleable s, RMStreams m s i) => s i a). clone t = clone_ t :: (Recycleable s, RMStreams m s i) => New s i a
"fusion" forall s. stream (new (fill s)) = s
"recycling" forall p. fill (stream (new p)) = p
"inplace" forall (f :: forall m. Monad m => MStream m i a -> MStream m i a) p. fill (inplace f (stream (new p))) = transform (f :: forall m. Monad m => MStream m i a -> MStream m i a) p
"uninplace" forall (f :: forall m. MStream m i a -> MStream m i a) p. stream (new (transform f p)) = inplace f (stream (new p))
"inplace2" forall (f :: forall m. MStream m i a -> MStream m i a) (g :: forall m. MStream m i a -> MStream m i a) (s :: RMStreams m r i => MStream m i a). inplace f (inplace g s) = inplace (f . g) s :: RMStreams m r i => MStream m i a
"mfusion" forall (f :: forall m. MStream m i a -> MStream m i a) (g :: forall m. MStream m i a -> MStream m i a) (p :: New s i a). transform f (transform g p) = transform (f . g :: forall m. MStream m i a -> MStream m i a) p :: New s i a
  #-}
