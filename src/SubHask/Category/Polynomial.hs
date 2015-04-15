module SubHask.Category.Polynomial
    where

import Data.List ((++),intersperse,replicate,zipWith)
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra
import SubHask.Category.Trans.Linear as Linear
import SubHask.SubType

-------------------------------------------------------------------------------

data family ProofOf (cat::k) a

newtype instance ProofOf Polynomial a = ProofOf { unProofOf :: Polynomial a a }

instance Ring a => Semigroup (ProofOf Polynomial a) where
    (ProofOf p1)+(ProofOf p2) = ProofOf $ p1+p2

instance Ring a => Cancellative (ProofOf Polynomial a) where
    (ProofOf p1)-(ProofOf p2) = ProofOf $ p1-p2

instance Ring a => Monoid (ProofOf Polynomial a) where
    zero = ProofOf zero

instance (Ring a, Abelian a) => Abelian (ProofOf Polynomial a)

instance Ring a => Group (ProofOf Polynomial a) where
    negate (ProofOf p) = ProofOf $ negate p

instance Ring a => Rg (ProofOf Polynomial a) where
    (ProofOf p1)*(ProofOf p2) = ProofOf $ p1*p2

instance Ring a => Rig (ProofOf Polynomial a) where
    one = ProofOf one

instance Ring a => Ring (ProofOf Polynomial a) where
    fromInteger i = ProofOf $ fromInteger i

provePolynomial :: Ring a => (ProofOf Polynomial a -> ProofOf Polynomial a) -> Polynomial a a
provePolynomial f = unProofOf $ f $ ProofOf $ Polynomial [0,1]

-------------------------------------------------------------------------------

-- | The type of Polynomials over an arbitrary ring.
--
-- See <https://en.wikipedia.org/wiki/Polynomial_ring wikipedia> for more detail.
--
-- FIXME:
-- "Polynomial" takes two type parameters in order to be compatible with the "Category" hierarchy of classes.
-- But currently, both types must match each other.
-- Can/Should we generalize this to allow polynomials between types?
--
data Polynomial a b where
    Polynomial :: (Ring a, a~b) => [a] -> Polynomial a b

instance Show r => Show (Polynomial r r) where
    show (Polynomial p) = concat $ intersperse " + " $ go p 0
        where
            go :: [r] -> Int -> [String]
            go []     i = []
            go (x:xs) 0 = [show x] ++ go xs 1
            go (x:xs) 1 = [show x ++ "x"] ++ go xs 2
            go (x:xs) i = [show x ++ "x^" ++ show i] ++ go xs (i+1)

---------------------------------------

type instance Scalar (Polynomial a b) = Scalar b
type instance Logic (Polynomial a b) = Logic b

instance Eq b => Eq_ (Polynomial a b) where
    (Polynomial xs)==(Polynomial ys) = xs==ys

instance Ring r => Semigroup (Polynomial r r) where
    (Polynomial p1)+(Polynomial p2) = Polynomial $ sumList (+) p1 p2

instance Ring r => Monoid (Polynomial r r) where
    zero = Polynomial []

instance Ring r => Cancellative (Polynomial r r) where
    (Polynomial p1)-(Polynomial p2) = Polynomial $ sumList (-) p1 p2

instance Ring r => Group (Polynomial r r) where
    negate (Polynomial p) = Polynomial $ P.map negate p

instance (Ring r, Abelian r) => Abelian (Polynomial r r)

instance Ring r => Rg (Polynomial r r) where
    (Polynomial p1)*(Polynomial p2) = Polynomial $ P.foldl (sumList (+)) [] $ go p1 zero
        where
            go []     i = []
            go (x:xs) i = (replicate i zero ++ P.map (*x) p2):go xs (i+one)

instance Ring r => Rig (Polynomial r r) where
    one = Polynomial [one]

instance Ring r => Ring (Polynomial r r) where
    fromInteger i = Polynomial [fromInteger i]

type instance Polynomial r r >< r = Polynomial r r

instance IsScalar r => Module (Polynomial r r) where
    (Polynomial xs) .*  r               = Polynomial $ P.map (*r) xs
    (Polynomial xs) .*. (Polynomial ys) = Polynomial $ zipWith (*) xs ys

sumList f [] ys = ys
sumList f xs [] = xs
sumList f (x:xs) (y:ys) = f x y:sumList f xs ys

---------------------------------------

instance Category Polynomial where
    type ValidCategory Polynomial a = Ring a
    id = Polynomial [zero, one]
    (Polynomial xs) . p2@(Polynomial _) = Polynomial (map (\x -> Polynomial [x]) xs) $ p2

instance Sup Polynomial Hask Hask
instance Sup Hask Polynomial Hask

instance Polynomial <: Hask where
    embedType_ = Embed2 evalPolynomial

pow :: Rig r => r -> Int -> r
pow r i = foldl' (*) one $ P.replicate i r

evalPolynomial :: Polynomial a b -> a -> b
evalPolynomial (Polynomial xs) r = sum $ map go $ P.zip [0..] xs
    where
        go (i,x) = x*pow r i

-------------------------------------------------------------------------------

-- FIXME:
-- Polynomials should use the derivative interface from the Derivative module
--
-- class Category cat => Smooth cat where
--     derivative :: ValidCategory cat a b => cat a b Linear.+> cat a b
--
-- instance Smooth Polynomial where
--     derivative = unsafeProveLinear go
--         where
--             go (Polynomial xs) =  Polynomial $ P.tail $ P.zipWith (*) (inflist zero one) xs
--             inflist xs x = xs : inflist (xs+x) x
--
-- data MonoidT c a b = MonoidT (c a)


