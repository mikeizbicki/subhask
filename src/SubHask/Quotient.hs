{-# LANGUAGE RebindableSyntax #-}

module SubHask.Quotient
    where

import qualified Prelude as P
import Control.Monad

import SubHask.Algebra
import SubHask.Category
import SubHask.Internal.Prelude

-- | Maps members of an equivalence class into the "canonical" element.  This
-- element is often simpler for performing computations.
--
-- Instances must obey the law that:
--
-- > x == y     iff     canonicalEq x == canonicalEq y
-- > x /= y     iff     canonicalEq x /= canonicalEq y
--
class Eq a => CanonicalEq a where
    canonicalize :: a -> a

quotient :: CanonicalEq (a/b) => a -> a/b
quotient = canonicalize . Mod

newtype (/) (a :: *) (b :: k) = Mod a
    deriving (Eq,POrd,Ord,Read,Show,  P.Ord)

-- instance (Semigroup a, CanonicalEq (a/b)) => Semigroup (a/b) where
--     (Mod a)+(Mod b) = canonicalEq $ Mod $ a + b
--
-- instance (Monoid a, CanonicalEq (a/b)) => Monoid (a/b) where
--     zero = canonicalEq $ Mod $ zero
--
-- instance (Abelian a, CanonicalEq  (a/b)) => Abelian (a/b)
--
-- instance (Group a, CanonicalEq (a/b)) => Group (a/b) where
--     negate (Mod a) = canonicalEq $ Mod $ negate a
--
-- instance (Ring a, CanonicalEq (a/b)) => Rng (a/b) where
--     (Mod a)*(Mod b) = canonicalEq $ Mod $ a*b
--
-- instance (Ring a, CanonicalEq (a/b)) => Ring (a/b) where
--     one = canonicalEq $ Mod $ one
--
-- instance (Module a, CanonicalEq (a/b), HasScalar (a/b)) => Module (a/b) where
--     x .* (Mod a) = canonicalEq $ Mod $ x .* a
--     (Mod a) *. x = canonicalEq $ Mod $ a *. x

-- instance
--     ( IsScalar a
--     , CanonicalEq (a/b)
--     , Scalar (a/b) ~ (a/b)
--     ) => IsScalar (a/b)
--         where
--     fromInteger = canonicalEq . Mod . fromInteger
--
-- instance KnownNat n => CanonicalEq (Int / n) where
--     canonicalEq (Mod a) = Mod $ a `P.mod` (fromIntegral $ natVal (Proxy::Proxy n))

-- type instance Scalar (Int/ (n::Nat)) = Int / n

-- x :: Int/4
-- x = 3
