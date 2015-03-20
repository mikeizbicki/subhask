{-# LANGUAGE NoAutoDeriveTypeable #-}
{-# LANGUAGE OverlappingInstances #-}
-- | In the SubHask library, every type has both a mutable and immutable version.
-- Normally we work with the immutable version;
-- however, certain algorithms require the mutable version for efficiency.
-- This module defines the interface to the mutable types.
module SubHask.Mutable
    ( Mutable
    , HasMutable (..)
    , immutable2mutable
    , unsafeRunMutableProperty
    -- ** Primitive types
    , PrimMonad
    , PrimState
    )
    where

import SubHask.Internal.Prelude
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

instance (Show a, HasMutable a, PrimMonad m) => Show (Mutable m a) where
    show mx = unsafePerformIO $ unsafePrimToIO $ do
        x <- freeze mx
        return $ "Mutable ("++show x++")"

-- This is the generic Mutable instance for all types that don't specifically declare their own.
newtype instance Mutable m a = Mutable (PrimRef m a)

instance HasMutable a where
    freeze (Mutable mx) = readPrimRef mx
    thaw x = liftM Mutable $ newPrimRef x
    write (Mutable mx) x = writePrimRef mx x

instance (PrimMonad m, Arbitrary a) => Arbitrary (Mutable m a) where
    arbitrary = do
        a <- arbitrary
        return $ unsafePerformIO $ unsafePrimToIO $ thaw a

-- | A Simple default implementation for mutable operations.
immutable2mutable :: (a -> b -> a) -> (PrimMonad m => Mutable m a -> b -> m ())
immutable2mutable f ma b = do
    a <- freeze ma
    write ma (f a b)

-- | This function should only be used from within quickcheck properties.
-- All other uses are unsafe.
unsafeRunMutableProperty :: PrimMonad m => m a -> a
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
class HasMutable a where
    -- | Convert a mutable object into an immutable one.
    -- The implementation is guaranteed to copy the object within memory.
    -- The overhead is linear with the size of the object.
    freeze :: PrimMonad m => Mutable m a -> m a

    -- | Convert an immutable object into a mutable one
    -- The implementation is guaranteed to copy the object within memory.
    -- The overhead is linear with the size of the object.
    thaw :: PrimMonad m => a -> m (Mutable m a)

    -- | Assigns the value of the mutable variable to the immutable one.
    write :: PrimMonad m => Mutable m a -> a -> m ()

    -- | Return a copy of the mutable object.
    -- Changes to the copy do not update in the original, and vice-versa.
    copy :: PrimMonad m => Mutable m a -> m (Mutable m a)
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
    unsafeFreeze :: PrimMonad m => Mutable m a -> m a
    unsafeFreeze = freeze

    -- | Like "thaw", but much faster on some types
    -- because the implementation is not required to perform a memory copy.
    --
    -- WARNING:
    -- You must not access the immutable variable after calling unsafeThaw.
    -- The contents of this variable might have changed arbitrarily.
    -- This breaks referential transparency and is very bad.
    unsafeThaw :: PrimMonad m => a -> m (Mutable m a)
    unsafeThaw = thaw

{-
class (Semigroup g, HasMutable g) => SemigroupM g where
    infixl 1 +=
    (+=) :: PrimMonad m => Mutable m g -> g -> m ()

--------------------------------------------------------------------------------
-- Vector

newtype instance Mutable m (V.Vector a) = Mutable_Vector (VM.MVector (PrimState m) a)

instance HasMutable (V.Vector a) where
    freeze (Mutable_Vector mx) = do
        x <- VG.freeze mx
        return x

    thaw x = do
        mx <- VG.thaw x
        return $ Mutable_Vector mx

--------------------------------------------------------------------------------
-- Int

newtype instance Mutable (m:: * -> *) Int = Mutable_Int (PrimRef m Int)

instance HasMutable Int where
    freeze (Mutable_Int mx) = readPrimRef mx
    thaw x = liftM Mutable_Int $ newPrimRef x

instance SemigroupM Int where
    (Mutable_Int mx) += y = do
        x <- readPrimRef mx
        writePrimRef mx $ x+y
--         return $ (Mutable_Int mx)


--------------------------------------------------------------------------------

-- class Mutable m aa | a -> ma, ma -> a where
--     freeze :: PrimMonad m => ma (PrimState m) -> m a
--     thaw :: PrimMonad m => a -> m (ma (PrimState m))
--
--     unsafeFreeze :: PrimMonad m => ma (PrimState m) -> m a
--     unsafeFreeze = freeze
--
--     unsafeThaw :: PrimMonad m => a -> m (ma (PrimState m))
--     unsafeThaw = thaw


-- class (Semigroup g, Mutable g mg) => SemigroupM g mg where
--     infixl 6 +=
--     (+=) :: PrimMonad m => mg (PrimState m)-> mg (PrimState m) -> m (mg (PrimState m))

{-
type family MutableVersion a :: * -> *
type family ImmutableVersion (a :: * -> *) :: *

newtype MBoxedVector a s = MBoxedVector (V.MVector s a)

instance Mutable (V.Vector a) (MBoxedVector a) where
    unsafeThaw v = liftM MBoxedVector $ VG.unsafeThaw v
    unsafeFreeze (MBoxedVector mv) = VG.unsafeFreeze mv

    thaw v = liftM MBoxedVector $ VG.thaw v
    freeze (MBoxedVector mv) = VG.freeze mv

instance Semigroup g => SemigroupM (BoxedVector g) (MBoxedVector g) where
    (+=) (MBoxedVector mv1) (MBoxedVector mv2)
        | VGM.length mv1 == 0 = return $ MBoxedVector mv2
        | VGM.length mv2 == 0 = return $ MBoxedVector mv1
        | VGM.length mv2 /= VGM.length mv2 = error "BoxedVector.SemigroupM: vectors have unequal length"
        | otherwise = do
            go 0
            return (MBoxedVector mv1)
        where
            go i = if i == VGM.length mv1
                then return ()
                else do
                    g1 <- VGM.unsafeRead mv1 i
                    g2 <- VGM.unsafeRead mv2 i
                    VGM.unsafeWrite mv1 i (g1 + g2)
                    go (i+1)
-}
-}
