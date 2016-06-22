module SubHask.Category.Polynomial
    where

import Data.List (intersperse,filter,reverse)
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra
import SubHask.SubType

-- | The type of polynomials over an arbitrary ring.
--
-- See <https://en.wikipedia.org/wiki/Polynomial__ring wikipedia> for more detail.
type Polynomial a = Polynomial_ a a

-- |
-- FIXME:
-- "Polynomial_" takes two type parameters in order to be compatible with the "Category" hierarchy of classes.
-- But currently, both types must match each other.
-- Can/Should we generalize this to allow polynomials between types?
--
data Polynomial_ a b where
    Polynomial_ :: (Eq a, Ring a, a~b) => ![a] -> Polynomial_ a b

mkMutable [t| forall a b. Polynomial_ a b |]

instance (Eq r, ClassicalLogic r, Show r) => Show (Polynomial_ r r) where
    show (Polynomial_ xs) = concat $ intersperse " + " $ filter (/=[]) $ reverse $ P.map go $ P.zip [0..] xs
        where
            -- FIXME:
            -- The code below results in prettier output but incurs an "Eq" constraint that confuses ghci
            go :: (Int,r) -> String
            go (0,x) = when (zero/=x) $ show x
            go (1,x) = when (zero/=x) $ when (one/=x) (show x) ++ "x"
            go (i,x) = when (zero/=x) $ when (one/=x) (show x) ++ "x^" ++ show i

            when :: Monoid a => Bool -> a -> a
            when cond x = if cond then x else zero


-------------------------------------------------------------------------------

newtype instance ProofOf Polynomial_ a = ProofOf { unProofOf :: Polynomial_ a a }

mkMutable [t| forall a. ProofOf Polynomial_ a |]

instance Ring a => Semigroup (ProofOf Polynomial_ a) where
    (ProofOf p1)+(ProofOf p2) = ProofOf $ p1+p2

instance (Eq a, Ring a) => Cancellative (ProofOf Polynomial_ a) where
    (ProofOf p1)-(ProofOf p2) = ProofOf $ p1-p2

instance (Eq a, Ring a) => Monoid (ProofOf Polynomial_ a) where
    zero = ProofOf zero

instance (Ring a, Abelian a) => Abelian (ProofOf Polynomial_ a)

instance (Eq a, Ring a) => Group (ProofOf Polynomial_ a) where
    negate (ProofOf p) = ProofOf $ negate p

instance (Eq a, Ring a) => Rg (ProofOf Polynomial_ a) where
    (ProofOf p1)*(ProofOf p2) = ProofOf $ p1*p2

instance (Eq a, Ring a) => Rig (ProofOf Polynomial_ a) where
    one = ProofOf one

instance (Eq a, Ring a) => Ring (ProofOf Polynomial_ a) where
    fromInteger i = ProofOf $ fromInteger i

provePolynomial :: (Eq a, Ring a) => (ProofOf Polynomial_ a -> ProofOf Polynomial_ a) -> Polynomial_ a a
provePolynomial f = unProofOf $ f $ ProofOf $ Polynomial_ [0,1]

type instance Scalar (Polynomial_ a b) = Scalar b
type instance Logic (Polynomial_ a b) = Logic b

instance Eq b => Eq (Polynomial_ a b) where
    (Polynomial_ xs)==(Polynomial_ ys) = xs==ys

instance Ring r => Semigroup (Polynomial_ r r) where
    (Polynomial_ p1)+(Polynomial_ p2) = Polynomial_ $ sumList (+) p1 p2

instance (Eq r, Ring r) => Monoid (Polynomial_ r r) where
    zero = Polynomial_ []

instance (Eq r, Ring r) => Cancellative (Polynomial_ r r) where
    (Polynomial_ p1)-(Polynomial_ p2) = Polynomial_ $ sumList (-) p1 p2

instance (Eq r, Ring r) => Group (Polynomial_ r r) where
    negate (Polynomial_ p) = Polynomial_ $ P.map negate p

instance (Ring r, Abelian r) => Abelian (Polynomial_ r r)

instance (Eq r, Ring r) => Rg (Polynomial_ r r) where
    (Polynomial_ p1)*(Polynomial_ p2) = Polynomial_ $ P.foldl (sumList (+)) [] $ go p1 zero
        where
            go []     _ = []
            go (x:xs) i = (P.replicate i zero ++ P.map (*x) p2):go xs (i+one)

instance (Eq r, Ring r) => Rig (Polynomial_ r r) where
    one = Polynomial_ [one]

instance (Eq r, Ring r) => Ring (Polynomial_ r r) where
    fromInteger i = Polynomial_ [fromInteger i]

instance ValidScalar r => Module (Polynomial_ r r) where
    (Polynomial_ xs) .*  r               = Polynomial_ $ P.map (*r) xs

instance ValidScalar r => FreeModule (Polynomial_ r r) where
    (Polynomial_ xs) .*. (Polynomial_ ys) = Polynomial_ $ P.zipWith (*) xs ys
    ones = Polynomial_ $ P.repeat one

sumList :: (t -> t -> t) -> [t] -> [t] -> [t]
sumList _ [] ys = ys
sumList _ xs [] = xs
sumList f (x:xs) (y:ys) = f x y:sumList f xs ys

instance Category Polynomial_ where
    type ValidCategory Polynomial_ a = (Eq a, Ring a)
    id = Polynomial_ [zero, one]
    (Polynomial_ xs) . p2@(Polynomial_ _) = Polynomial_ (map (\x -> Polynomial_ [x]) xs) $ p2

instance Sup Polynomial_ Hask Hask
instance Sup Hask Polynomial_ Hask

instance Polynomial_ <: Hask where
    embedType_ = Embed2 evalPolynomial_

pow :: Rig r => r -> Int -> r
pow r i = foldl' (*) one $ P.replicate i r

evalPolynomial_ :: Polynomial_ a b -> a -> b
evalPolynomial_ (Polynomial_ xs) r = sum $ imap go xs
    where
        go i x = x*pow r i

-- FIXME:
-- Polynomial_s should use the derivative interface from the Derivative module
--
-- class Category cat => Smooth cat where
--     derivative :: ValidCategory cat a b => cat a b Linear.+> cat a b
--
-- instance Smooth Polynomial_ where
--     derivative = unsafeProveLinear go
--         where
--             go (Polynomial_ xs) =  Polynomial_ $ P.tail $ P.zipWith (*) (inflist zero one) xs
--             inflist xs x = xs : inflist (xs+x) x
--
-- data MonoidT c a b = MonoidT (c a)


