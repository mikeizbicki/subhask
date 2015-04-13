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

data family Polynomial r1 r2
data instance Polynomial r r where
    Polynomial :: Ring r => [r] -> Polynomial r r

instance Show r => Show (Polynomial r r) where
    show (Polynomial p) = concat $ intersperse " + " $ go p 0
        where
            go :: [r] -> Int -> [String]
            go []     i = []
            go (x:xs) 0 = [show x] ++ go xs 1
            go (x:xs) 1 = [show x ++ "x"] ++ go xs 2
            go (x:xs) i = [show x ++ "x^" ++ show i] ++ go xs (i+1)

---------------------------------------

type instance Scalar (Polynomial r r) = Scalar r
type instance Logic (Polynomial r r) = Logic r

instance Eq r => Eq_ (Polynomial r r) where
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
--
-- FIXME:
-- None of this works because our definition of a Polynomial requires the domain and range to have the same type.
-- This shouldn't be necessary in general, but it's going to take some thinking to fix it.
--
-- instance Category Polynomial where
--     type ValidCategory Polynomial a = (Ring a, IsScalar a)
--     id = Polynomial [zero, one]
--     p1.p2 = evalPolynomial p1 p2
--
-- instance Sup Polynomial Hask Hask
-- instance Sup Hask Polynomial Hask
--
-- instance Polynomial <: Hask where
--     embedType_ = Embed2 evalPolynomial

pow :: Rig r => r -> Int -> r
pow r i = P.foldl (*) one $ P.replicate i r

evalPolynomial :: Polynomial m m -> m -> m
evalPolynomial (Polynomial xs) m = P.foldl1 (+) $ P.map (\(i,c) -> c*(pow m i)) $ P.zip [0..] xs

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


