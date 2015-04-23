{-# LANGUAGE NoAutoDeriveTypeable #-}
-- | In the SubHask library, every type has both a mutable and immutable version.
-- Normally we work with the immutable version;
-- however, certain algorithms require the mutable version for efficiency.
-- This module defines the interface to the mutable types.
module SubHask.Mutable
    ( Mutable
    , IsMutable (..)
    , immutable2mutable
    , unsafeRunMutableProperty

    , mkMutable

    -- ** Primitive types
    , PrimBase
    , PrimState

    -- ** Internal
    -- | These exports should never be used directly.
    -- They are required by the "mkMutable" TH function.
    , PrimRef
    , readPrimRef
    , writePrimRef
    , newPrimRef
    , helper_liftM
    )
    where

import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving
import SubHask.TemplateHaskell.Mutable

import Prelude (($),(.))
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive
import Data.PrimRef
import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | The mutable version of an immutable data type.
-- This is equivalent to the "PrimRef" type, which generalizes "STRef" and "IORef".
--
-- Unlike "PrimRef", "Mutable" is implemented using a data family.
-- This means that data types can provide more efficient implementations.
-- The canonical example is "Vector".
-- Vectors in standard Haskell use a different interface than the standard "PrimRef".
-- This requires the programmer learn multiple interfaces, and prevents the programmer from reusing code.
-- Very un-Haskelly.
-- This implementation of mutability gives a consistent interface for all data types.
data family Mutable (m :: * -> *) a

instance (Show a, IsMutable a, PrimBase m) => Show (Mutable m a) where
    show mx = unsafePerformIO $ unsafePrimToIO $ do
        x <- freeze mx
        return $ "Mutable ("++show x++")"

instance (IsMutable a, PrimBase m, Arbitrary a) => Arbitrary (Mutable m a) where
    arbitrary = do
        a <- arbitrary
        return $ unsafePerformIO $ unsafePrimToIO $ thaw a

-- | A Simple default implementation for mutable operations.
immutable2mutable :: IsMutable a => (a -> b -> a) -> (PrimBase m => Mutable m a -> b -> m ())
immutable2mutable f ma b = do
    a <- freeze ma
    write ma (f a b)

-- | This function should only be used from within quickcheck properties.
-- All other uses are unsafe.
unsafeRunMutableProperty :: PrimBase m => m a -> a
unsafeRunMutableProperty = unsafePerformIO . unsafePrimToIO


-- | This class implements conversion between mutable and immutable data types.
-- It is the equivalent of the functions provided in "Contol.Monad.Primitive",
-- but we use the names of from the "Data.Vector" interface because they are simpler and more intuitive.
--
-- Every data type is an instance of this class using a default implementation based on "PrimRef"s.
-- We use OverlappingInstances to allow some instances to provide more efficient implementations.
-- We require that any overlapping instance be semantically equivalent to prevent unsafe behavior.
-- The use of OverlappingInstances should only affect you if your creating your own specialized instances of the class.
-- You shouldn't have to do this unless you are very concerned about performance on a complex type.
--
-- FIXME:
-- It's disappointing that we still require this class, the "Primitive" class, and the "Storable" class.
-- Can these all be unified?
class IsMutable a where
    -- | Convert a mutable object into an immutable one.
    -- The implementation is guaranteed to copy the object within memory.
    -- The overhead is linear with the size of the object.
    freeze :: PrimBase m => Mutable m a -> m a

    -- | Convert an immutable object into a mutable one
    -- The implementation is guaranteed to copy the object within memory.
    -- The overhead is linear with the size of the object.
    thaw :: PrimBase m => a -> m (Mutable m a)

    -- | Assigns the value of the mutable variable to the immutable one.
    write :: PrimBase m => Mutable m a -> a -> m ()

    -- | Return a copy of the mutable object.
    -- Changes to the copy do not update in the original, and vice-versa.
    copy :: PrimBase m => Mutable m a -> m (Mutable m a)
    copy ma = do
        a <- unsafeFreeze ma
        thaw a

    -- | Like "freeze", but much faster on some types
    -- because the implementation is not required to perform a memory copy.
    --
    -- WARNING:
    -- You must not modify the mutable variable after calling unsafeFreeze.
    -- This might change the value of the immutable variable.
    -- This breaks referential transparency and is very bad.
    unsafeFreeze :: PrimBase m => Mutable m a -> m a
    unsafeFreeze = freeze

    -- | Like "thaw", but much faster on some types
    -- because the implementation is not required to perform a memory copy.
    --
    -- WARNING:
    -- You must not access the immutable variable after calling unsafeThaw.
    -- The contents of this variable might have changed arbitrarily.
    -- This breaks referential transparency and is very bad.
    unsafeThaw :: PrimBase m => a -> m (Mutable m a)
    unsafeThaw = thaw

--------------------------------------------------------------------------------

mkMutable [t| Int |]
mkMutable [t| Integer |]
mkMutable [t| Rational |]
mkMutable [t| Float |]
mkMutable [t| Double |]
mkMutable [t| Bool |]

mkMutable [t| forall a. [a] |]
mkMutable [t| () |]
mkMutable [t| forall a b. (a,b) |]
mkMutable [t| forall a b c. (a,b,c) |]
mkMutable [t| forall a b. a -> b |]
