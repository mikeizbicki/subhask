module SubHask.Category.Polynomial
    where

import GHC.Prim
import Data.List ((++),intersperse,replicate,zipWith)
import qualified Prelude as P

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra
import SubHask.Category.Trans.Linear as Linear

-------------------------------------------------------------------------------

data family Polynomial r1 r2
data instance Polynomial r r = Polynomial [r]

instance Show r => Show (Polynomial r r) where
    show (Polynomial p) = concat $ intersperse " + " $ go p 0
        where
            go :: [r] -> P.Int -> [P.String]
            go []     i = []
            go (x:xs) 0 = [show x] ++ go xs 1
            go (x:xs) 1 = [show x ++ "x"] ++ go xs 2
            go (x:xs) i = [show x ++ "x^" ++ show i] ++ go xs (i+1)

pow :: Ring r => r -> P.Int -> r
pow r i = P.foldl (*) one $ P.replicate i r

-- evalPolynomial :: (Ring m, Module m) => Polynomial m m -> m -> m
-- evalPolynomial (Polynomial xs) m = P.foldl1 (+) $ P.map (\(i,c) -> c.*(pow m i)) $ P.zip [0..] xs

---------------------------------------

type instance Scalar (Polynomial r r) = Scalar r
type instance Logic (Polynomial r r) = Logic r

instance Eq r => Eq_ (Polynomial r r) where
    (Polynomial xs)==(Polynomial ys) = xs==ys

instance Ring r => Semigroup (Polynomial r r) where
    (Polynomial p1)+(Polynomial p2) = Polynomial $ sumList p1 p2

instance Ring r => Monoid (Polynomial r r) where
    zero = Polynomial []

instance Ring r => Cancellative (Polynomial r r) where
    p1-p2 = p1 + negate p2

instance Ring r => Group (Polynomial r r) where
    negate (Polynomial p) = Polynomial $ P.map negate p

instance Ring r => Abelian (Polynomial r r)

instance Ring r => Rg (Polynomial r r) where
    (Polynomial p1)*(Polynomial p2) = Polynomial $ P.foldl sumList [] $ go p1 zero
        where
            go []     i = []
            go (x:xs) i = (replicate i zero ++ P.map (*x) p2):go xs (i+one)

instance Ring r => Rig (Polynomial r r) where
    one = Polynomial [one]

instance (IsScalar r, Ring r) => Module (Polynomial r r) where
    r *. (Polynomial xs) = Polynomial $ P.map (*r) xs
    (Polynomial xs) .*. (Polynomial ys) = Polynomial $ zipWith (*) xs ys

sumList [] ys = ys
sumList xs [] = xs
sumList (x:xs) (y:ys) = x+y:sumList xs ys

---------------------------------------

-- instance Category Polynomial where
--     type ValidCategory Polynomial a b = (a~b, Ring a, Module a, IsScalar a)
--     id = Polynomial [zero, one]
--     p1.p2 = evalPolynomial p1 p2
--
-- instance SubCategory Polynomial (->) where
--     embed = evalPolynomial

-------------------------------------------------------------------------------

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


