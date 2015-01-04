{-# LANGUAGE CPP,MagicHash,UnboxedTuples #-}

-- | This module defines the algebraic type-classes used in subhask.
-- The class hierarchies are significantly more general than those in the standard Prelude.
module SubHask.Algebra
    (
    -- * Comparisons
    Logic
    , ClassicalLogic
    , Eq_ (..)
    , Eq
    , law_Eq_reflexive
    , law_Eq_symmetric
    , law_Eq_transitive
    , POrd_ (..)
    , POrd
    , law_POrd_commutative
    , law_POrd_associative
    , theorem_POrd_idempotent
    , Lattice_ (..)
    , Lattice
    , isChain
    , isAntichain
    , POrdering (..)
    , law_Lattice_commutative
    , law_Lattice_associative
    , theorem_Lattice_idempotent
    , law_Lattice_infabsorption
    , law_Lattice_supabsorption
    , law_Lattice_reflexivity
    , law_Lattice_antisymmetry
    , law_Lattice_transitivity
    , defn_Lattice_greaterthan
    , MinBound_ (..)
    , MinBound
    , law_MinBound_inf
    , Bounded (..)
    , law_Bounded_sup
    , supremum
    , supremum_
    , infimum
    , infimum_
    , disjoint
    , Heyting (..)
    , modusPonens
    , law_Heyting_maxbound
    , law_Heyting_infleft
    , law_Heyting_infright
    , law_Heyting_distributive
    , Boolean (..)
    , law_Boolean_infcomplement
    , law_Boolean_supcomplement
    , law_Boolean_infdistributivity
    , law_Boolean_supdistributivity

--     , defn_Latticelessthaninf
--     , defn_Latticelessthansup
    , Graded (..)
    , law_Graded_pred
    , law_Graded_fromEnum
    , Ord_ (..)
    , law_Ord_totality
    , law_Ord_min
    , law_Ord_max
    , Ord
    , Ordering (..)
    , min
    , max
    , maximum
    , maximum_
    , minimum
    , minimum_
    , argmin
    , argmax
--     , argminimum_
--     , argmaximum_
    , Enum (..)
    , law_Enum_succ
    , law_Enum_toEnum

    -- ** Boolean helpers
    , (||)
    , (&&)
    , true
    , false
    , and
    , or

    -- * Set-like
    , Elem
    , Container (..)
    , empty
    , law_Container_preservation
    , law_Container_empty
    , Unfoldable (..)
    , singletonAt
    , insert
    , fromString
    , law_Unfoldable_singleton
    , theorem_Unfoldable_insert
    , defn_Unfoldable_cons
    , defn_Unfoldable_snoc
    , defn_Unfoldable_fromList
    , defn_Unfoldable_fromListN
    , Indexed (..)
    , law_Indexed_cons
    , Foldable (..)
    , foldtree1
    , length
    , reduce
    , concat
    , headMaybe
    , tailMaybe
    , lastMaybe
    , initMaybe
    , Index
    , Value

    , Partitionable (..)
    , law_Partitionable_length
    , law_Partitionable_monoid
    , parallelN
    , parallel

    , Topology (..)
    , FreeMonoid

    -- * Maybe
    , CanError (..)
    , Maybe' (..)

    -- * Number-like
    -- ** Classes with one operator
    , Semigroup (..)
    , law_Semigroup_associativity
    , Cancellative (..)
    , law_Cancellative_rightminus1
    , law_Cancellative_rightminus2
    , Monoid (..)
    , law_Monoid_leftid
    , law_Monoid_rightid
    , Abelian (..)
    , law_Abelian_commutative
    , Group (..)
    , defn_Group_negateminus
    , law_Group_leftinverse
    , law_Group_rightinverse
--     , AbelianGroup

    -- ** Classes with two operators
    , Rg(..)
    , law_Rg_multiplicativeAssociativity
    , law_Rg_multiplicativeCommutivity
    , law_Rg_annihilation
    , law_Rg_distributivityLeft
    , theorem_Rg_distributivityRight
    , Rig(..)
    , law_Rig_multiplicativeId
    , Rng
    , defn_Ring_fromInteger
    , Ring(..)
    , Integral(..)
    , law_Integral_divMod
    , law_Integral_quotRem
    , law_Integral_toFromInverse
    , fromIntegral
    , Field(..)
    , RationalField(..)
    , convertRationalField
    , toFloat
    , toDouble
    , BoundedField(..)
    , infinity
    , negInfinity
    , Floating (..)
    , QuotientField(..)

    -- ** Linear algebra
    , Scalar
    , IsScalar
    , HasScalar
    , Cone (..)
    , Module (..)
    , VectorSpace (..)
    , InnerProductSpace (..)
    , innerProductDistance
    , innerProductNorm
    , OuterProductSpace (..)

    -- ** Sizes
    , Normed (..)
--     , KernelSpace (..)
--     , mkSelfKernel
--     , SelfKernel
    , MetricSpace (..)
    , law_MetricSpace_nonnegativity
    , law_MetricSpace_indiscernables
    , law_MetricSpace_symmetry
    , law_MetricSpace_triangle
    )
    where

import qualified Prelude as P
import qualified Data.List as L

import Prelude (Ordering (..))
import Data.Ratio
import Data.Typeable
import Test.QuickCheck (Arbitrary (..), frequency)

import Control.Concurrent
import Control.Parallel.Strategies
import System.IO.Unsafe -- used in the parallel function

import GHC.Prim
import GHC.Types
import GHC.Magic

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.SubType

-------------------------------------------------------------------------------
-- relational classes

-- | Every type has an associated logic.
-- Most types use classical logic, which corresponds to the Bool type.
-- But types can use any logical system they want.
-- Functions, for example, use an infinite logic.
-- You probably want your logic to be an instance of "Boolean", but this is not required.
--
-- See wikipedia's articles on <https://en.wikipedia.org/wiki/Algebraic_logic algebraic logic>,
-- and <https://en.wikipedia.org/wiki/Infinitary_logic infinitary logic> for more details.
type family Logic a :: *
type instance Logic Bool = Bool
type instance Logic Char = Bool
type instance Logic Int = Bool
type instance Logic Integer = Bool
type instance Logic Rational = Bool
type instance Logic Float = Bool
type instance Logic Double = Bool
type instance Logic (a->b) = a -> Logic b
type instance Logic (a,b) = Logic a

-- | Classical logic is implemented using the Prelude's Bool type.
class Logic a ~ Bool => ClassicalLogic a
instance Logic a ~ Bool => ClassicalLogic a

-- | Defines equivalence classes over the type.
-- The types need not have identical representations in the machine to be equal.
--
-- See <https://en.wikipedia.org/wiki/Equivalence_class wikipedia>
-- and <http://ncatlab.org/nlab/show/equivalence+class ncatlab> for more details.
class Eq_ a where

    infix 4 ==
    (==) :: a -> a -> Logic a

    -- | In order to have the "not equals to" relation, your logic must have a notion of "not", and therefore must be "Boolean".
    {-# INLINE (/=) #-}
    infix 4 /=
    (/=) :: Boolean (Logic a) => a -> a -> Logic a
    (/=) = not (==)

law_Eq_reflexive :: Eq a => a -> Logic a
law_Eq_reflexive a = a==a

law_Eq_symmetric :: Eq a => a -> a -> Logic a
law_Eq_symmetric a1 a2 = (a1==a2)==(a2==a1)

law_Eq_transitive :: Eq a => a -> a -> a -> Logic a
law_Eq_transitive a1 a2 a3 = (a1==a2&&a2==a3) ==> (a1==a3)

instance Eq_ Bool     where (==) = (P.==); (/=) = (P./=)
instance Eq_ Char     where (==) = (P.==); (/=) = (P./=)
instance Eq_ Int      where (==) = (P.==); (/=) = (P./=)
instance Eq_ Integer  where (==) = (P.==); (/=) = (P./=)
instance Eq_ Rational where (==) = (P.==); (/=) = (P./=)
instance Eq_ Float    where (==) = (P.==); (/=) = (P./=)
-- instance Eq_ Double   where (==) = (P.==); (/=) = (P./=)
instance Eq_ Double   where
    (D# x) == (D# y) = isTrue# (x ==## y)
    (/=) = (P./=)

instance Eq_ b => Eq_ (a -> b) where
    (f==g) a = f a == f a

instance (Eq_ a, Eq_ b, Logic a ~ Logic b, Lattice (Logic a)) => Eq_ (a,b) where
    (a1,b1)==(a2,b2) = a1==a2 && b1==b2

class (Eq_ a, Logic a ~ Bool) => Eq a
instance (Eq_ a, Logic a ~ Bool) => Eq a

--------------------

-- | This is more commonly known as a "meet" semilattice
class Eq_ b => POrd_ b where
    inf :: b -> b -> b

    {-# INLINE (<=) #-}
    infix 4 <=
    (<=) :: b -> b -> Logic b
    b1 <= b2 = inf b1 b2 == b1

    {-# INLINE (<) #-}
    infix 4 <
    (<) :: Boolean (Logic b) => b -> b -> Logic b
    b1 < b2 = inf b1 b2 == b1 && b1 /= b2

class (Eq b, POrd_ b) => POrd b
instance (Eq b, POrd_ b) => POrd b

law_POrd_commutative :: (Eq b, POrd_ b) => b -> b -> Bool
law_POrd_commutative b1 b2 = inf b1 b2 == inf b2 b1

law_POrd_associative :: (Eq b, POrd_ b) => b -> b -> b -> Bool
law_POrd_associative b1 b2 b3 = inf (inf b1 b2) b3 == inf b1 (inf b2 b3)

theorem_POrd_idempotent :: (Eq b, POrd_ b) => b -> Bool
theorem_POrd_idempotent b = inf b b == b

instance POrd_ Bool       where inf = (P.&&)
instance POrd_ Char       where inf = P.min
instance POrd_ Int        where inf = P.min
instance POrd_ Integer    where inf = P.min
instance POrd_ Float      where inf = P.min
instance POrd_ Double     where inf = P.min
instance POrd_ Rational   where inf = P.min
instance POrd_ b => POrd_ (a -> b) where
    inf f g = \x -> inf (f x) (g x)

-------------------

-- | Most Lattice literature only considers 'Bounded' lattices, but here we have both upper and lower bounded lattices.
--
-- prop> minBound <= b || not (minBound > b)
--
class POrd_ b => MinBound_ b where
    minBound :: b

class (Eq b, MinBound_ b) => MinBound b
instance (Eq b, MinBound_ b) => MinBound b

law_MinBound_inf :: (Eq b, MinBound_ b) => b -> Bool
law_MinBound_inf b = inf b minBound == minBound

-- | "false" is an upper bound because `a && false = false` for all a.
false :: MinBound_ b => b
false = minBound

-- | Two sets are disjoint if their infimum is the empty set.
-- This function generalizes the notion of disjointness for any lower bounded lattice.
-- FIXME: add other notions of disjoint
disjoint :: (Eq b, MinBound_ b) => b -> b -> Bool
disjoint b1 b2 = (inf b1 b2) == minBound

instance MinBound_ Bool where minBound = False
instance MinBound_ Char where minBound = P.minBound
instance MinBound_ Int where minBound = P.minBound
instance MinBound_ Float where minBound = -1/0 -- FIXME: should be a primop for this
instance MinBound_ Double where minBound = -1/0

instance MinBound_ b => MinBound_ (a -> b) where minBound = \x -> minBound
-- instance Lattice a => MinBound_ [a] where minBound = []


-------------------

-- | Represents all the possible ordering relations in a classical logic (i.e. Logic a ~ Bool)
data POrdering
    = PLT
    | PGT
    | PEQ
    | PNA
    deriving (Read,Show)

type instance Logic POrdering = Bool

instance Arbitrary POrdering where
    arbitrary = frequency
        [ (1, P.return PLT)
        , (1, P.return PGT)
        , (1, P.return PEQ)
        , (1, P.return PNA)
        ]

instance Eq_ POrdering where
    PLT == PLT = True
    PGT == PGT = True
    PEQ == PEQ = True
    PNA == PNA = True
    _ == _ = False

-- | FIXME: there are many semigroups over POrdering;
-- how should we represent the others? newtypes?
instance Semigroup POrdering where
    PEQ + x = x
    PLT + _ = PLT
    PGT + _ = PGT
    PNA + _ = PNA

instance Semigroup Ordering where
    EQ + x = x
    LT + _ = LT
    GT + _ = GT

instance Monoid POrdering where
    zero = PEQ

instance Monoid Ordering where
    zero = EQ


-- |
--
--
-- See <https://en.wikipedia.org/wiki/Lattice_%28order%29 wikipedia> for more details.
class POrd_ b => Lattice_ b where
    sup :: b -> b -> b

    {-# INLINE (>=) #-}
    infix 4 >=
    (>=) :: b -> b -> Logic b
    b1 >= b2 = sup b1 b2 == b1

    {-# INLINE (>) #-}
    infix 4 >
    (>) :: Boolean (Logic b) => b -> b -> Logic b
    b1 > b2 = sup b1 b2 == b1 && b1 /= b2

    -- | This function does not make sense on non-classical logics
    --
    -- FIXME: there are probably related functions for all these other logics;
    -- is there a nice way to represent them all?
    {-# INLINABLE pcompare #-}
    pcompare :: Logic b ~ Bool => b -> b -> POrdering
    pcompare a b = if a==b
        then PEQ
        else if a < b
            then PLT
            else if a > b
                then PGT
                else PNA

class (Eq b, Lattice_ b) => Lattice b
instance (Eq b, Lattice_ b) => Lattice b

law_Lattice_commutative :: (Eq b, Lattice_ b) => b -> b -> Bool
law_Lattice_commutative b1 b2 = sup b1 b2 == sup b2 b1

law_Lattice_associative :: (Eq b, Lattice_ b) => b -> b -> b -> Bool
law_Lattice_associative b1 b2 b3 = sup (sup b1 b2) b3 == sup b1 (sup b2 b3)

theorem_Lattice_idempotent :: (Eq b, Lattice_ b) => b -> Bool
theorem_Lattice_idempotent b = sup b b == b

law_Lattice_infabsorption :: (Eq b, Lattice b) => b -> b -> Bool
law_Lattice_infabsorption b1 b2 = inf b1 (sup b1 b2) == b1

law_Lattice_supabsorption :: (Eq b, Lattice b) => b -> b -> Bool
law_Lattice_supabsorption b1 b2 = sup b1 (inf b1 b2) == b1

law_Lattice_reflexivity :: Lattice a => a -> Logic a
law_Lattice_reflexivity a = a<=a

law_Lattice_antisymmetry :: Lattice a => a -> a -> Logic a
law_Lattice_antisymmetry a1 a2
    | a1 <= a2 && a2 <= a1 = a1 == a2
    | otherwise = true

law_Lattice_transitivity :: Lattice a => a -> a -> a -> Logic a
law_Lattice_transitivity  a1 a2 a3
    | a1 <= a2 && a2 <= a3 = a1 <= a3
    | a1 <= a3 && a3 <= a2 = a1 <= a2
    | a2 <= a1 && a1 <= a3 = a2 <= a3
    | a2 <= a3 && a3 <= a1 = a2 <= a1
    | a3 <= a2 && a2 <= a1 = a3 <= a1
    | a3 <= a1 && a1 <= a2 = a3 <= a2
    | otherwise = true

defn_Lattice_greaterthan :: Lattice a => a -> a -> Logic a
defn_Lattice_greaterthan a1 a2
    | a1 < a2 = a2 >= a1
    | a1 > a2 = a2 <= a1
    | otherwise = true

instance Lattice_ Bool       where sup = (P.||)
instance Lattice_ Char       where sup = P.max
instance Lattice_ Int        where sup = P.max
instance Lattice_ Integer    where sup = P.max
instance Lattice_ Float      where sup = P.max
instance Lattice_ Double     where sup = P.max
instance Lattice_ Rational   where sup = P.max
instance Lattice_ b => Lattice_ (a -> b) where
    sup f g = \x -> sup (f x) (g x)

{-# INLINE (&&) #-}
infixr 3 &&
(&&) :: Lattice_ b => b -> b -> b
(&&) = inf

{-# INLINE (||) #-}
infixr 2 ||
(||) :: Lattice_ b => b -> b -> b
(||) = sup

-- | A chain is a collection of elements all of which can be compared
isChain :: Lattice a => [a] -> Logic a
isChain [] = true
isChain (x:xs) = all (/=PNA) (map (pcompare x) xs) && isAntichain xs
--
-- | An antichain is a collection of elements none of which can be compared
--
-- See <http://en.wikipedia.org/wiki/Antichain wikipedia> for more details.
--
-- See also the article on <http://en.wikipedia.org/wiki/Dilworth%27s_theorem Dilward's Theorem>.
isAntichain :: Lattice a => [a] -> Logic a
isAntichain [] = true
isAntichain (x:xs) = all (==PNA) (map (pcompare x) xs) && isAntichain xs

-------------------

-- | In a WellFounded type, every element (except the 'maxBound" if it exists) has a successor element
--
-- See <ncatlab http://ncatlab.org/nlab/show/well-founded+relation> for more info.
class (Graded b, Ord_ b) => Enum b where
    succ :: b -> b

    toEnum :: Int -> b

law_Enum_succ :: Enum b => b -> b -> Bool
law_Enum_succ b1 b2 = fromEnum (succ b1) == fromEnum b1+1
                   || fromEnum (succ b1) == fromEnum b1

law_Enum_toEnum :: (Lattice b, Enum b) => b -> Bool
law_Enum_toEnum b = toEnum (fromEnum b) == b

instance Enum Bool where
    succ True = True
    succ False = True

    toEnum 1 = True
    toEnum 0 = False

instance Enum Int where
    succ i = if i == maxBound
        then i
        else i+1

    toEnum = id

instance Enum Char where
    succ = P.succ
    toEnum i = if i < 0
        then P.toEnum 0
        else P.toEnum i

instance Enum Integer where succ = P.succ; toEnum = P.toEnum

-- | An element of a graded poset has a unique predecessor.
--
-- See <https://en.wikipedia.org/wiki/Graded_poset wikipedia> for more details.
class Lattice b => Graded b where
    -- | the predecessor in the ordering
    pred :: b -> b

    -- | Algebrists typically call this function the "rank" of the element in the poset;
    -- however we use the name from the standard prelude instead
    fromEnum :: b -> Int

law_Graded_pred :: Graded b => b -> b -> Bool
law_Graded_pred b1 b2 = fromEnum (pred b1) == fromEnum b1-1
                     || fromEnum (pred b1) == fromEnum b1

law_Graded_fromEnum :: (Lattice b, Graded b) => b -> b -> Bool
law_Graded_fromEnum b1 b2
    | b1 <  b2  = fromEnum b1 <  fromEnum b2
    | b1 >  b2  = fromEnum b1 >  fromEnum b2
    | b1 == b2  = fromEnum b1 == fromEnum b2
    | otherwise = True

instance Graded Bool where
    pred True = False
    pred False = False

    fromEnum True = 1
    fromEnum False = 0

instance Graded Int where
    pred i = if i == minBound
        then i
        else i-1

    fromEnum = id

instance Graded Char where
    pred c = if c=='\NUL'
        then '\NUL'
        else P.pred c
    fromEnum = P.fromEnum
instance Graded Integer where pred = P.pred; fromEnum = P.fromEnum

(<.) :: (Lattice b, Graded b) => b -> b -> Bool
b1 <. b2 = b1 == pred b2

(>.) :: (Lattice b, Enum b) => b -> b -> Bool
b1 >. b2 = b1 == succ b2

---------------------------------------

-- | This is the class of total orderings.
--
-- See https://en.wikipedia.org/wiki/Total_order
class Lattice_ a => Ord_ a where
    compare :: (Logic a~Bool, Ord_ a) => a -> a -> Ordering
    compare a1 a2 = case pcompare a1 a2 of
        PLT -> LT
        PGT -> GT
        PEQ -> EQ
        PNA -> error "PNA given by pcompare on a totally ordered type"

law_Ord_totality :: Ord a => a -> a -> Bool
law_Ord_totality a1 a2 = a1 <= a2 || a2 <= a1

law_Ord_min :: Ord a => a -> a -> Bool
law_Ord_min a1 a2 = min a1 a2 == a1
                 || min a1 a2 == a2

law_Ord_max :: Ord a => a -> a -> Bool
law_Ord_max a1 a2 = max a1 a2 == a1
                 || max a1 a2 == a2

{-# INLINE min #-}
min :: Ord_ a => a -> a -> a
min = inf

{-# INLINE max #-}
max :: Ord_ a => a -> a -> a
max = sup

class (Eq a, Ord_ a) => Ord a
instance (Eq a, Ord_ a) => Ord a

instance Ord_ Char
instance Ord_ Int
instance Ord_ Integer
instance Ord_ Float
instance Ord_ Double
instance Ord_ Rational
instance Ord_ Bool

-------------------

-- | A Bounded lattice is a lattice with both a minimum and maximum element
--
class (Lattice_ b, MinBound_ b) => Bounded b where
    maxBound :: b

law_Bounded_sup :: (Eq b, Bounded b) => b -> Bool
law_Bounded_sup b = sup b maxBound == maxBound

-- | "true" is an lower bound because `a && true = true` for all a.
true :: Bounded b => b
true = maxBound

instance Bounded Bool   where maxBound = True
instance Bounded Char   where maxBound = P.maxBound
instance Bounded Int    where maxBound = P.maxBound
instance Bounded Float  where maxBound = 1/0 -- FIXME: should be a primop for infinity
instance Bounded Double where maxBound = 1/0
instance Bounded b => Bounded (a -> b) where maxBound = \x -> maxBound

-- | Heyting algebras are lattices that support implication, but not necessarily the law of excluded middle.
--
-- FIXME:
-- Is every Heyting algebra a cancellative Abelian semigroup?
-- If so, should we make that explicit in the class hierarchy?
--
-- ==== Laws
-- There is a single, simple law that Heyting algebras must satisfy:
--
-- prop> a ==> b = c   ===>   a && c < b
--
-- ==== Theorems
-- From the laws, we automatically get the properties of:
--
-- distributivity
--
-- See <https://en.wikipedia.org/wiki/Heyting_algebra wikipedia> for more details.
class Bounded b => Heyting b where
    -- | FIXME: think carefully about infix
    infixl 3 ==>
    (==>) :: b -> b -> b

law_Heyting_maxbound :: (Eq b, Heyting b) => b -> Bool
law_Heyting_maxbound b = (b ==> b) == maxBound

law_Heyting_infleft :: (Eq b, Heyting b) => b -> b -> Bool
law_Heyting_infleft b1 b2 = (b1 && (b1 ==> b2)) == (b1 && b2)

law_Heyting_infright :: (Eq b, Heyting b) => b -> b -> Bool
law_Heyting_infright b1 b2 = (b2 && (b1 ==> b2)) == b2

law_Heyting_distributive :: (Eq b, Heyting b) => b -> b -> b -> Bool
law_Heyting_distributive b1 b2 b3 = (b1 ==> (b2 && b3)) == ((b1 ==> b2) && (b1 ==> b3))

-- | FIXME: add the axioms for intuitionist logic, which are theorems based on these laws
--

-- | Modus ponens gives us a default definition for "==>" in a "Boolean" algebra.
-- This formula is guaranteed to not work in a "Heyting" algebra that is not "Boolean".
--
-- See <https://en.wikipedia.org/wiki/Modus_ponens wikipedia> for more details.
modusPonens :: Boolean b => b -> b -> b
modusPonens b1 b2 = not b1 || b2

instance Heyting Bool where (==>) = modusPonens

instance Heyting b => Heyting (a -> b) where (f==>g) a = f a ==> g a

-- | Generalizes Boolean variables.
--
-- See <https://en.wikipedia.org/wiki/Boolean_algebra_%28structure%29 wikipedia> for more details.
class Heyting b => Boolean b where
    not :: b -> b

--     type IfResult b a :: *
--     type IfResult b a = a

--     ifThenElse :: b -> a -> a -> IfResult b a
--     ifThenElse = P.ifThenElse

law_Boolean_infcomplement :: (Eq b, Boolean b) => b -> Bool
law_Boolean_infcomplement b = (b || not b) == true

law_Boolean_supcomplement :: (Eq b, Boolean b) => b -> Bool
law_Boolean_supcomplement b = (b && not b) == false

law_Boolean_infdistributivity :: (Eq b, Boolean b) => b -> b -> b -> Bool
law_Boolean_infdistributivity b1 b2 b3 = (b1 || (b2 && b3)) == ((b1 || b2) && (b1 || b3))

law_Boolean_supdistributivity :: (Eq b, Boolean b) => b -> b -> b -> Bool
law_Boolean_supdistributivity b1 b2 b3 = (b1 && (b2 || b3)) == ((b1 && b2) || (b1 && b3))

instance Boolean Bool where not = P.not
instance Boolean b => Boolean (a -> b) where not f = \x -> not $ f x

-------------------------------------------------------------------------------
-- numeric classes

class Semigroup g where
    infixl 6 +
    (+) :: g -> g -> g


    -- | this quantity is related to the concept of machine precision and floating point error
--     associativeEpsilon :: Ring (Scalar g) => g -> Scalar g
--     associativeEpsilon _ = 0

law_Semigroup_associativity :: (Eq g, Semigroup g ) => g -> g -> g -> Bool
law_Semigroup_associativity g1 g2 g3 = g1 + (g2 + g3) == (g1 + g2) + g3

-- theorem_Semigroup_associativity ::
--     ( Ring (Scalar g)
--     , Eq (Scalar g)
--     , Eq g
--     , Semigroup g
--     ) => g -> g -> g -> Bool
-- theorem_Semigroup_associativity g1 g2 g3 = if associativeEpsilon g1==0
--     then g1 + (g2 + g3) == (g1 + g2) + g3
--     else True
--
-- law_Semigroup_epsilonAssociativity ::
--     ( Semigroup g
--     , Normed g
--     , Field (Scalar g)
--     ) => g -> g -> g -> Bool
-- law_Semigroup_epsilonAssociativity g1 g2 g3
--     = relativeSemigroupError g1 g2 g3 < associativeEpsilon g1

relativeSemigroupError ::
    ( Semigroup g
    , Normed g
    , Field (Scalar g)
    ) => g -> g -> g -> Scalar g
relativeSemigroupError g1 g2 g3
    = abs (   g1 + ( g2    + g3 ) )
    / abs ( ( g1 +   g2  ) + g3   )

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle :: Semigroup m => m -> m
cycle xs = xs' where xs' = xs + xs'

instance Semigroup Int      where (+) = (P.+)
instance Semigroup Integer  where (+) = (P.+)
instance Semigroup Float    where (+) = (P.+)
instance Semigroup Double   where (+) = (P.+)
instance Semigroup Rational where (+) = (P.+)

instance Semigroup a => Semigroup (Maybe a) where
    (Just a1) + (Just a2) = Just $ a1+a2
    Nothing   + a2        = a2
    a1        + Nothing   = a1

instance Semigroup a => Semigroup (Maybe' a) where
    (Just' a1) + (Just' a2) = Just' $ a1+a2
    Nothing'   + a2         = a2
    a1         + Nothing'   = a1

instance Semigroup () where
    ()+() = ()

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (a1,b1)+(a2,b2) = (a1+a2,b1+b2)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
    (a1,b1,c1)+(a2,b2,c2) = (a1+a2,b1+b2,c1+c2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a,b,c,d) where
    (a1,b1,c1,d1)+(a2,b2,c2,d2) = (a1+a2,b1+b2,c1+c2,d1+d2)

instance Semigroup   b => Semigroup   (a -> b) where f+g = \a -> f a + g a

---------------------------------------

class Semigroup g => Monoid g where
    zero :: g

law_Monoid_leftid :: (Monoid g, Eq g) => g -> Bool
law_Monoid_leftid g = zero + g == g

law_Monoid_rightid :: (Monoid g, Eq g) => g -> Bool
law_Monoid_rightid g = g + zero == g

---------

instance Monoid Int       where zero = 0
instance Monoid Integer   where zero = 0
instance Monoid Float     where zero = 0
instance Monoid Double    where zero = 0
instance Monoid Rational  where zero = 0

instance Semigroup a => Monoid (Maybe a) where
    zero = Nothing

instance Semigroup a => Monoid (Maybe' a) where
    zero = Nothing'

instance Monoid () where
    zero = ()

instance (Monoid a, Monoid b) => Monoid (a,b) where
    zero = (zero,zero)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
    zero = (zero,zero,zero)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
    zero = (zero,zero,zero,zero)

instance Monoid      b => Monoid      (a -> b) where zero = \a -> zero

---------------------------------------

-- | In a cancellative semigroup,
--
-- 1)
--
-- > a + b = a + c   ==>   b = c
-- so
-- > (a + b) - b = a + (b - b) = a
--
-- 2)
--
-- > b + a = c + a   ==>   b = c
-- so
-- > -b + (b + a) = (-b + b) + a = a
--
-- This allows us to define "subtraction" in the semigroup.
-- If the semigroup is embeddable in a group, subtraction can be thought of as performing the group subtraction and projecting the result back into the domain of the cancellative semigroup.
-- It is an open problem to fully characterize which cancellative semigroups can be embedded into groups.
--
-- See <http://en.wikipedia.org/wiki/Cancellative_semigroup wikipedia> for more details.
class Semigroup g => Cancellative g where
    infixl 6 -
    (-) :: g -> g -> g

law_Cancellative_rightminus1 :: (Eq g, Cancellative g) => g -> g -> Bool
law_Cancellative_rightminus1 g1 g2 = (g1 + g2) - g2 == g1

law_Cancellative_rightminus2 :: (Eq g, Cancellative g) => g -> g -> Bool
law_Cancellative_rightminus2 g1 g2 = g1 + (g2 - g2) == g1

instance Cancellative Int        where (-) = (P.-)
instance Cancellative Integer    where (-) = (P.-)
instance Cancellative Float      where (-) = (P.-)
instance Cancellative Double     where (-) = (P.-)
instance Cancellative Rational   where (-) = (P.-)

instance Cancellative () where
    ()-() = ()

instance (Cancellative a, Cancellative b) => Cancellative (a,b) where
    (a1,b1)-(a2,b2) = (a1-a2,b1-b2)

instance (Cancellative a, Cancellative b, Cancellative c) => Cancellative (a,b,c) where
    (a1,b1,c1)-(a2,b2,c2) = (a1-a2,b1-b2,c1-c2)

instance (Cancellative a, Cancellative b, Cancellative c, Cancellative d) => Cancellative (a,b,c,d) where
    (a1,b1,c1,d1)-(a2,b2,c2,d2) = (a1-a2,b1-b2,c1-c2,d1-d2)

instance Cancellative b => Cancellative (a -> b) where
    f-g = \a -> f a - g a

-- | The GrothendieckGroup is a general way to construct groups from cancellative groups.
--
-- FIXME: How should this be related to the Ratio type?
--
-- See <http://en.wikipedia.org/wiki/Grothendieck_group wikipedia> for more details.
data GrothendieckGroup g where
    GrotheindieckGroup :: Cancellative g => g -> GrothendieckGroup g

---------------------------------------

class (Cancellative g, Monoid g) => Group g where
    {-# INLINE negate #-}
    negate :: g -> g
    negate g = zero - g

defn_Group_negateminus :: (Eq g, Group g) => g -> g -> Bool
defn_Group_negateminus g1 g2 = g1 + negate g2 == g1 - g2

law_Group_leftinverse :: (Eq g, Group g) => g -> Bool
law_Group_leftinverse g = negate g + g == zero

law_Group_rightinverse :: (Eq g, Group g) => g -> Bool
law_Group_rightinverse g = g + negate g == zero

instance Group Int        where negate = P.negate
instance Group Integer    where negate = P.negate
instance Group Float      where negate = P.negate
instance Group Double     where negate = P.negate
instance Group Rational   where negate = P.negate

instance Group () where
    negate () = ()

instance (Group a, Group b) => Group (a,b) where
    negate (a,b) = (negate a,negate b)

instance (Group a, Group b, Group c) => Group (a,b,c) where
    negate (a,b,c) = (negate a,negate b,negate c)

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
    negate (a,b,c,d) = (negate a,negate b,negate c,negate d)

instance Group b => Group (a -> b) where negate f = negate . f

---------------------------------------

-- type AbelianGroup g = (Abelian g, Group g)
-- class AbelianGroup g

class Semigroup m => Abelian m

law_Abelian_commutative :: (Abelian g, Eq g) => g -> g -> Bool
law_Abelian_commutative g1 g2 = g1 + g2 == g2 + g1

instance Abelian Int
instance Abelian Integer
instance Abelian Float
instance Abelian Double
instance Abelian Rational

instance Abelian ()
instance (Abelian a, Abelian b) => Abelian (a,b)
instance (Abelian a, Abelian b, Abelian c) => Abelian (a,b,c)
instance (Abelian a, Abelian b, Abelian c, Abelian d) => Abelian (a,b,c,d)

instance Abelian b => Abelian (a -> b)

---------------------------------------

-- | FIXME: What constraint should be here? Semigroup?
--
-- See <http://ncatlab.org/nlab/show/normed%20group ncatlab>
class
    ( Ord_ (Scalar g)
    , HasScalar g
    ) => Normed g where
    abs :: g -> Scalar g

instance Normed Int       where abs = P.abs
instance Normed Integer   where abs = P.abs
instance Normed Float     where abs = P.abs
instance Normed Double    where abs = P.abs
instance Normed Rational  where abs = P.abs

---------------------------------------

-- | A Rg is a Ring without multiplicative identity or negative numbers.
-- (Hence the removal of the i and n from the name.)
--
-- There is no standard terminology for this structure.
-- They might also be called \"semirings without identity\", \"pre-semirings\", or \"hemirings\".
-- See <http://math.stackexchange.com/questions/359437/name-for-a-semiring-minus-multiplicative-identity-requirement this stackexchange question> for a discussion on naming.
--
class (Abelian r, Monoid r) => Rg r where
    infixl 7 *
    (*) :: r -> r -> r

law_Rg_multiplicativeAssociativity :: (Eq r, Rg r) => r -> r -> r -> Bool
law_Rg_multiplicativeAssociativity r1 r2 r3 = (r1 * r2) * r3 == r1 * (r2 * r3)

law_Rg_multiplicativeCommutivity :: (Eq r, Rg r) => r -> r -> Bool
law_Rg_multiplicativeCommutivity r1 r2 = r1*r2 == r2*r1

law_Rg_annihilation :: (Eq r, Rg r) => r -> Bool
law_Rg_annihilation r = r * zero == zero

law_Rg_distributivityLeft :: (Eq r, Rg r) => r -> r -> r -> Bool
law_Rg_distributivityLeft r1 r2 r3 = r1*(r2+r3) == r1*r2+r1*r3

theorem_Rg_distributivityRight :: (Eq r, Rg r) => r -> r -> r -> Bool
theorem_Rg_distributivityRight r1 r2 r3 = (r2+r3)*r1 == r2*r1+r3*r1

instance Rg Int         where (*) = (P.*)
instance Rg Integer     where (*) = (P.*)
instance Rg Float       where (*) = (P.*)
instance Rg Double      where (*) = (P.*)
instance Rg Rational    where (*) = (P.*)

instance Rg b => Rg (a -> b) where f*g = \a -> f a * g a

---------------------------------------

-- | A Rig is a Rg with multiplicative identity.
-- They are also known as semirings.
--
-- See <https://en.wikipedia.org/wiki/Semiring wikipedia>
-- and <http://ncatlab.org/nlab/show/rig ncatlab>
-- for more details.
class (Monoid r, Rg r) => Rig r where
    -- | the multiplicative identity
    one :: r

law_Rig_multiplicativeId :: (Eq r, Rig r) => r -> Bool
law_Rig_multiplicativeId r = r * one == r && one * r == r

instance Rig Int         where one = 1
instance Rig Integer     where one = 1
instance Rig Float       where one = 1
instance Rig Double      where one = 1
instance Rig Rational    where one = 1

instance Rig b => Rig (a -> b) where one = \a -> one

---------------------------------------

-- | FIXME: made into a class due to TH limitations
-- > type Rng r = (Rg r, Group r)
class (Rg r, Group r) => Rng r
instance (Rg r, Group r) => Rng r

-- |
--
-- It is not part of the standard definition of rings that they have a "fromInteger" function.
-- It follows from the definition, however, that we can construct such a function.
-- The "slowFromInteger" function is this standard construction.
--
-- See <https://en.wikipedia.org/wiki/Ring_%28mathematics%29 wikipedia>
-- and <http://ncatlab.org/nlab/show/ring ncatlab>
-- for more details.
class (Rng r, Rig r) => Ring r where
    fromInteger :: Integer -> r
    fromInteger = slowFromInteger

defn_Ring_fromInteger :: (Eq r, Ring r) => r -> Integer -> Bool
defn_Ring_fromInteger r i = fromInteger i `asTypeOf` r
                         == slowFromInteger i

-- | Here we construct an element of the Ring based on the additive and multiplicative identities.
-- This function takes O(n) time, where n is the size of the Integer.
-- Most types should be able to compute this value significantly faster.
--
-- FIXME: replace this with peasant multiplication.
slowFromInteger :: forall r. (Rng r, Rig r) => Integer -> r
slowFromInteger i = if i>0
    then          foldl' (+) zero $ P.map (const (one::r)) [1..        i]
    else negate $ foldl' (+) zero $ P.map (const (one::r)) [1.. negate i]

instance Ring Int         where fromInteger = P.fromInteger
instance Ring Integer     where fromInteger = P.fromInteger
instance Ring Float       where fromInteger = P.fromInteger
instance Ring Double      where fromInteger = P.fromInteger
instance Ring Rational    where fromInteger = P.fromInteger

instance Ring b => Ring (a -> b) where fromInteger i = \a -> fromInteger i

---------------------------------------

-- | 'Integral' numbers can be formed from a wide class of things that behave
-- like integers, but intuitively look nothing like integers.
--
-- FIXME: All Fields are integral domains; should we make it a subclass?  This wouuld have the (minor?) problem of making the Integral class have to be an approximate embedding.
-- FIXME: Not all integral domains are homomorphic to the integers (e.g. a field)
--
-- See wikipedia on <https://en.wikipedia.org/wiki/Integral_element integral elements>,
--  <https://en.wikipedia.org/wiki/Integral_domain integral domains>,
-- and the <https://en.wikipedia.org/wiki/Ring_of_integers ring of integers>.
class Ring a => Integral a where
    toInteger :: a -> Integer

    infixl 7  `quot`, `rem`

    -- | truncates towards zero
    quot :: a -> a -> a
    quot a1 a2 = fst (quotRem a1 a2)

    rem :: a -> a -> a
    rem a1 a2 = snd (quotRem a1 a2)

    quotRem :: a -> a -> (a,a)


    infixl 7 `div`, `mod`

    -- | truncates towards negative infinity
    div :: a -> a -> a
    div a1 a2 = fst (divMod a1 a2)

    mod :: a -> a -> a
    mod a1 a2 = snd (divMod a1 a2)

    divMod :: a -> a -> (a,a)


law_Integral_divMod :: (Eq a, Integral a) => a -> a -> Bool
law_Integral_divMod a1 a2 = if a2 /= 0
    then a2 * (a1 `div` a2) + (a1 `mod` a2) == a1
    else True

law_Integral_quotRem :: (Eq a, Integral a) => a -> a -> Bool
law_Integral_quotRem a1 a2 = if a2 /= 0
    then a2 * (a1 `quot` a2) + (a1 `rem` a2) == a1
    else True

law_Integral_toFromInverse :: (Eq a, Integral a) => a -> Bool
law_Integral_toFromInverse a = fromInteger (toInteger a) == a

{-# NOINLINE [1] fromIntegral #-}
fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral = fromInteger . toInteger

{-# RULES
"fromIntegral/Int->Int" fromIntegral = id :: Int -> Int
    #-}

instance Integral Int where
    div = P.div
    mod = P.mod
    divMod = P.divMod
    quot = P.quot
    rem = P.rem
    quotRem = P.quotRem
    toInteger = P.toInteger

instance Integral Integer where
    div = P.div
    mod = P.mod
    divMod = P.divMod
    quot = P.quot
    rem = P.rem
    quotRem = P.quotRem
    toInteger = P.toInteger

---------------------------------------

-- | Fields are Rings with a multiplicative inverse.
--
-- See <https://en.wikipedia.org/wiki/Field_%28mathematics%29 wikipedia>
-- and <http://ncatlab.org/nlab/show/field ncatlab>
-- for more details.
class Ring r => Field r where
    {-# INLINE reciprocal #-}
    reciprocal :: r -> r
    reciprocal r = one/r

    {-# INLINE (/) #-}
    infixl 7 /
    (/) :: r -> r -> r
    n/d = n * reciprocal d

    {-# INLINE fromRational #-}
    fromRational :: Rational -> r
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)


instance Field Float      where (/) = (P./); fromRational=P.fromRational
instance Field Double     where (/) = (P./); fromRational=P.fromRational
instance Field Rational   where (/) = (P./); fromRational=P.fromRational

instance Field b => Field (a -> b) where reciprocal f = reciprocal . f

----------------------------------------

-- | A Rational field is a field with only a single dimension.
class Field r => RationalField r where
    toRational :: r -> Rational

instance RationalField Float    where  toRational=P.toRational
instance RationalField Double   where  toRational=P.toRational
instance RationalField Rational where  toRational=P.toRational

convertRationalField :: (RationalField a, RationalField b) => a -> b
convertRationalField = fromRational . toRational

-- |
--
-- FIXME:
-- These functions don't work for Int's, but they should
toFloat :: RationalField a => a -> Float
toFloat = convertRationalField

toDouble :: RationalField a => a -> Double
toDouble = convertRationalField

---------------------------------------

-- | The prototypical example of a bounded field is the extended real numbers.
-- Other examples are the extended hyperreal numbers and the extended rationals.
-- Each of these fields has been extensively studied, but I don't know of any studies of this particular abstraction of these fields.
--
-- See <https://en.wikipedia.org/wiki/Extended_real_number_line wikipedia> for more details.
class (Field r, Bounded r) => BoundedField r where
    nan :: r
    nan = 0/0

    isNaN :: r -> Bool

infinity :: BoundedField r => r
infinity = maxBound

negInfinity :: BoundedField r => r
negInfinity = minBound

instance BoundedField Float  where isNaN = P.isNaN
instance BoundedField Double where isNaN = P.isNaN

---------------------------------------

-- | A 'QuotientField' is a field with an 'IntegralDomain' as a subring.
-- There may be many such subrings (for example, every field has itself as an integral domain subring).
-- This is especially true in Haskell because we have different data types that represent essentially the same ring (e.g. "Int" and "Integer").
-- Therefore this is a multiparameter type class.
-- The 'r' parameter represents the quotient field, and the 's' parameter represents the subring.
-- The main purpose of this class is to provide functions that map elements in 'r' to elements in 's' in various ways.
--
-- FIXME: Need examples.  Is there a better representation?
--
-- See <http://en.wikipedia.org/wiki/Field_of_fractions wikipedia> for more details.
--
class (Ring r, Integral s) => QuotientField r s where
    truncate    :: r -> s
    round       :: r -> s
    ceiling     :: r -> s
    floor       :: r -> s

    (^^)        :: r -> s -> r

#define mkQuotientField(r,s) \
instance QuotientField r s where \
    truncate = P.truncate; \
    round    = P.round; \
    ceiling  = P.ceiling; \
    floor    = P.floor; \
    (^^)     = (P.^^)

mkQuotientField(Float,Int)
mkQuotientField(Float,Integer)
mkQuotientField(Double,Int)
mkQuotientField(Double,Integer)
mkQuotientField(Rational,Int)
mkQuotientField(Rational,Integer)

instance QuotientField Int Int where
    truncate = id
    round = id
    ceiling = id
    floor = id
    (^^) = (P.^)

instance QuotientField Integer Integer where
    truncate = id
    round = id
    ceiling = id
    floor = id
    (^^) = (P.^)

---------------------------------------

-- | FIXME: add rest of Floating functions
--
-- FIXME: There are better characterizations of many of these functions than floating.
class Field r => Floating r where
    pi :: r
    exp :: r -> r
    sqrt :: r -> r
    log :: r -> r
    tanh :: r -> r
    (**) :: r -> r -> r
    infixl 8 **

instance Floating Float where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    tanh = P.tanh
    (**) = (P.**)

instance Floating Double where
    pi = P.pi
    sqrt = P.sqrt
    log = P.log
    exp = P.exp
    tanh = P.tanh
    (**) = (P.**)

---------------------------------------

type family Scalar m

-- FIXME: made into classes due to TH limitations
-- > type IsScalar r = (Ring r, Scalar r ~ r)
class (Ring r, Scalar r~r) => IsScalar r
instance (Ring r, Scalar r~r) => IsScalar r

-- FIXME: made into classes due to TH limitations
-- > type HasScalar a = IsScalar (Scalar a)
class ({-Logic a~Logic (Scalar a), -}IsScalar (Scalar a)) => HasScalar a
instance ({-Logic a~Logic (Scalar a), -}IsScalar (Scalar a)) => HasScalar a

type instance Scalar Int      = Int
type instance Scalar Integer  = Integer
type instance Scalar Float    = Float
type instance Scalar Double   = Double
type instance Scalar Rational = Rational

type instance Scalar (a,b) = Scalar a
type instance Scalar (a,b,c) = Scalar a
type instance Scalar (a,b,c,d) = Scalar a

type instance Scalar (a -> b) = Scalar b

---------------------------------------

-- | A Cone is an \"almost linear\" subspace of a module.
-- Examples include the cone of positive real numbers and the cone of positive semidefinite matrices.
--
-- See <http://en.wikipedia.org/wiki/Cone_%28linear_algebra%29 wikipedia for more details.
--
-- FIXME:
-- There are many possible laws for cones (as seen in the wikipedia article).
-- I need to explicitly formulate them here.
-- Intuitively, the laws should apply the module operations and then project back into the "closest point" in the cone.
--
-- FIXME:
-- We're using the definition of a cone from linear algebra.
-- This definition is closely related to the definition from topology.
-- What is needed to ensure our definition generalizes to topological cones?
-- See <http://en.wikipedia.org/wiki/Cone_(topology) wikipedia>
-- and <http://ncatlab.org/nlab/show/cone ncatlab> for more details.
class (Cancellative m, HasScalar m, Rig (Scalar m)) => Cone m where
    infixl 7 *..
    (*..) :: Scalar m -> m -> m

    infixl 7 ..*..
    (..*..) :: m -> m -> m

---------------------------------------

class (Abelian m, Group m, HasScalar m, Ring (Scalar m)) => Module m where
    infixl 7 *.
    (*.) :: Scalar m -> m -> m

    infixl 7 .*.
    (.*.) :: m -> m -> m

{-# INLINE (.*) #-}
infixl 7 .*
(.*) :: Module m => m -> Scalar m -> m
m .* r  = r *. m

instance (Module a, Module b, Scalar a ~ Scalar b) => Module (a,b) where
    r *. (a,b) = (r*.a, r*.b)
    (a1,b1).*.(a2,b2) = (a1.*.a2,b1.*.b2)

instance (Module a, Module b, Module c, Scalar a ~ Scalar b, Scalar a ~ Scalar c) => Module (a,b,c) where
    r *. (a,b,c) = (r*.a, r*.b,r*.c)
    (a1,b1,c1).*.(a2,b2,c2) = (a1.*.a2,b1.*.b2,c1.*.c2)

instance
    ( Module a, Module b, Module c, Module d
    , Scalar a ~ Scalar b, Scalar a ~ Scalar c, Scalar a~Scalar d
    ) => Module (a,b,c,d)
        where
    r *. (a,b,c,d) = (r*.a, r*.b,r*.c,r*.d)
    (a1,b1,c1,d1).*.(a2,b2,c2,d2) = (a1.*.a2,b1.*.b2,c1.*.c2,d1.*.d2)

instance Module Int       where (*.) = (*); (.*.) = (*)
instance Module Integer   where (*.) = (*); (.*.) = (*)
instance Module Float     where (*.) = (*); (.*.) = (*)
instance Module Double    where (*.) = (*); (.*.) = (*)
instance Module Rational  where (*.) = (*); (.*.) = (*)

instance Module      b => Module      (a -> b) where
    b  *. f = \a -> b    *. f a
    g .*. f = \a -> g a .*. f a

---------------------------------------

class (Module v, Field (Scalar v)) => VectorSpace v where
    {-# INLINE (./) #-}
    infixl 7 ./
    (./) :: v -> Scalar v -> v
    v ./ r = v .* reciprocal r

    infixl 7 ./.
    (./.) :: v -> v -> v

-- {-# INLINE normalize #-}
-- normalize :: (Normed v, VectorSpace v) => v -> v
-- normalize v = if abs v==zero
--     then v
--     else v ./ abs v


instance (VectorSpace a,VectorSpace b, Scalar a ~ Scalar b) => VectorSpace (a,b) where
    (a,b) ./ r = (a./r,b./r)
    (a1,b1)./.(a2,b2) = (a1./.a2,b1./.b2)

instance (VectorSpace a, VectorSpace b, VectorSpace c, Scalar a ~ Scalar b, Scalar a ~ Scalar c) => VectorSpace (a,b,c) where
    (a,b,c) ./ r = (a./r,b./r,c./r)
    (a1,b1,c1)./.(a2,b2,c2) = (a1./.a2,b1./.b2,c1./.c2)

instance
    ( VectorSpace a, VectorSpace b, VectorSpace c, VectorSpace d
    , Scalar a ~ Scalar b, Scalar a ~ Scalar c, Scalar a~Scalar d
    ) => VectorSpace (a,b,c,d)
        where
    (a,b,c,d)./r = (a./r, b./r,c./r,d./r)
    (a1,b1,c1,d1)./.(a2,b2,c2,d2) = (a1./.a2,b1./.b2,c1./.c2,d1./.d2)

instance VectorSpace Float     where (./) = (/); (./.) = (/)
instance VectorSpace Double    where (./) = (/); (./.) = (/)
instance VectorSpace Rational  where (./) = (/); (./.) = (/)

instance VectorSpace b => VectorSpace (a -> b) where g ./. f = \a -> g a ./. f a

---------------------------------------

-- |
--
-- Note: It is not axiomatic that an inner product space's field must be non-finite (and hence normed and ordered).
-- However, it necessarily follows from the axioms.
-- Therefore, we include these class constraints.
-- In practice, this greatly simplifies many type signatures.
-- See this <http://math.stackexchange.com/questions/49348/inner-product-spaces-over-finite-fields stackoverflow question> for a detailed explanation of these constraints.
--
-- Note: Similarly, it is not axiomatic that every 'InnerProductSpace' is a 'MetricSpace'.
-- This is easy to see, however, since the "innerProductNorm" function can be used to define a metric on any inner product space.
-- The implementation will probably not be efficient, however.
--
-- Note: Machine learning papers often talk about Hilbert spaces, which are a minor extension of inner product spaces.
-- Specifically, the metric space must be complete.
-- I know of no useful complete metric spaces that can be represented in finite space on a computer, however, so we use the more general inner product spaces in this library.
class
    ( VectorSpace v
    , MetricSpace v
    , Normed v
    , HasScalar v
    , Normed (Scalar v)
    , Floating (Scalar v)
    ) => InnerProductSpace v
        where

    infix 8 <>
    (<>) :: v -> v -> Scalar v

{-# INLINE squaredInnerProductNorm #-}
squaredInnerProductNorm :: InnerProductSpace v => v -> Scalar v
squaredInnerProductNorm v = v<>v

{-# INLINE innerProductNorm #-}
innerProductNorm :: (Floating (Scalar v), InnerProductSpace v) => v -> Scalar v
innerProductNorm = sqrt . squaredInnerProductNorm

{-# INLINE innerProductDistance #-}
innerProductDistance :: (Floating (Scalar v), InnerProductSpace v) => v -> v -> Scalar v
innerProductDistance v1 v2 = innerProductNorm $ v1-v2

---------------------------------------

-- | FIXME: This needs to relate to a Monoidal category
class
    ( VectorSpace v
    , Scalar (Outer v) ~ Scalar v
    , Ring (Outer v)
    ) => OuterProductSpace v
        where
    type Outer v
    infix 8 ><
    (><) :: v -> v -> Outer v

---------------------------------------

-- | Metric spaces give us the most intuitive notion of distance between objects.
--
-- FIXME: There are many other notions of distance and we should make a while hierarchy.
class
    ( Ord_ (Scalar v)
    , Ring (Scalar v)
    , Eq_ v
    , Boolean (Logic v)
    , Logic (Scalar v) ~ Logic v
    ) => MetricSpace v
        where

    distance :: v -> v -> Scalar v

    -- | FIXME: this should have a default implementation in terms of isFartherThanWithDistanceCanError
    -- the weird constraints on that function prevent this
    {-# INLINE isFartherThan #-}
    isFartherThan :: v -> v -> Scalar v -> Logic v
    isFartherThan s1 s2 b =
        {-# SCC isFartherThan #-}
        distance s1 s2 > b

    {-# INLINE isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError ::
        ( Logic (Scalar v)~Bool
        , CanError (Scalar v)
        ) => v
          -> v
          -> Scalar v
          -> Scalar v
    isFartherThanWithDistanceCanError s1 s2 b =
        {-# SCC isFartherThanWithDistanceCanError  #-}
        if dist > b
            then errorVal
            else dist
        where
            dist = distance s1 s2

law_MetricSpace_nonnegativity :: MetricSpace v => v -> v -> Logic v
law_MetricSpace_nonnegativity v1 v2 = distance v1 v2 >= 0

law_MetricSpace_indiscernables :: (Eq v, MetricSpace v) => v -> v -> Logic v
law_MetricSpace_indiscernables v1 v2 = if v1 == v2
    then distance v1 v2 == 0
    else distance v1 v2 > 0

law_MetricSpace_symmetry :: MetricSpace v => v -> v -> Logic v
law_MetricSpace_symmetry v1 v2 = distance v1 v2 == distance v2 v1

law_MetricSpace_triangle :: MetricSpace v => v -> v -> v -> Logic v
law_MetricSpace_triangle m1 m2 m3
    = distance m1 m2 <= distance m1 m3 + distance m2 m3
   && distance m1 m3 <= distance m1 m2 + distance m2 m3
   && distance m2 m3 <= distance m1 m3 + distance m2 m1

instance MetricSpace Int      where distance x1 x2 = abs $ x1 - x2
instance MetricSpace Integer  where distance x1 x2 = abs $ x1 - x2
instance MetricSpace Float    where distance x1 x2 = abs $ x1 - x2
instance MetricSpace Double   where distance x1 x2 = abs $ x1 - x2
instance MetricSpace Rational where distance x1 x2 = abs $ x1 - x2

---------

data Maybe' a
    = Nothing'
    | Just' !a

instance NFData a => NFData (Maybe' a) where
    rnf Nothing' = ()
    rnf (Just' a) = rnf a

class CanError a where
    errorVal :: a
    isError :: a -> Bool

    isJust :: a -> Bool
    isJust = not isError

instance CanError (Maybe a) where
    {-# INLINE isError #-}
    isError Nothing = True
    isError _ = False

    {-# INLINE errorVal #-}
    errorVal = Nothing

instance CanError (Maybe' a) where
    {-# INLINE isError #-}
    isError Nothing' = True
    isError _ = False

    {-# INLINE errorVal #-}
    errorVal = Nothing'

instance CanError [a] where
    {-# INLINE isError #-}
    isError [] = True
    isError _  = False

    {-# INLINE errorVal #-}
    errorVal = []

instance CanError Float where
    {-# INLINE isError #-}
    {-# INLINE errorVal #-}
    isError = isNaN
    errorVal = 0/0

instance CanError Double where
    {-# INLINE isError #-}
    {-# INLINE errorVal #-}
    isError = isNaN
    errorVal = 0/0

-------------------------------------------------------------------------------
-- set-like

class (Monoid s, Container s, MinBound_ s, Unfoldable s, Foldable s) => FreeMonoid s
instance (Monoid s, Container s, MinBound_ s, Unfoldable s, Foldable s) => FreeMonoid s

type Item s = Elem s
type family Elem s

-- |
-- concept must be generic enough for:
--   maps
--   hash tables
--   lists/sequences
--   fuzzy sets?
--   bloom filters
class (Semigroup s, Eq_ s) => Container s where
    elem :: Elem s -> s -> Logic s

    notElem :: Elem s -> s -> Logic s

law_Container_preservation :: (Heyting (Logic s), Container s) => s -> s -> Elem s -> Logic s
law_Container_preservation s1 s2 e = (e `elem` s1 || e `elem` s2) ==> (e `elem` (s1+s2))

law_Container_empty :: (Monoid s, Container s) => s -> Elem s -> Logic s
law_Container_empty s e = notElem e (empty `asTypeOf` s)

-- | a slightly more suggestive name for a container's monoid identity
empty :: (Monoid s, Container s) => s
empty = zero

-- type family Index s :: *
-- type family Value s :: *

type family Fst a where
    Fst (a,b) = a

type family Snd a where
    Snd (a,b) = b

type Index s = Fst (Elem s)
type Value s = Snd (Elem s)

-- | An indexed container us a container of tuples (a,b).
-- Every value of type a is associated with a value of type b.
class (Elem s~(Index s, Value s), Unfoldable s) => Indexed s where
    (!) :: s -> Index s -> Value s
    (!) s i = case s !! i of
        Just x -> x
        Nothing -> error "used (!) on an invalid index"

    (!!) :: s -> Index s -> Maybe (Value s)

    findWithDefault :: Value s -> Index s -> s -> Value s
    findWithDefault def i s = case s !! i of
        Nothing -> def
        Just e -> e

    hasIndex :: Index s -> s -> Bool
    hasIndex i s = case s !! i of
        Nothing -> False
        Just _ -> True

    -- FIXME: everything below should probably find homes in a different class

    indices :: s -> [Index s]

    values :: s -> [Value s]

law_Indexed_cons :: (Indexed s, Eq_ (Value s)) => s -> Elem s -> Logic (Value s)
law_Indexed_cons s e = cons e s ! fst e == snd e

-- |
--
-- prop> isOpen empty == True
--
-- prop> isOpen a && isOpen b   ===>   isOpen (a || b)
--
-- prop> isOpen a && isOpen b   ===>   isOpen (a && b)
--
-- prop> closed
--
-- FIXME: how does this relate to smooth functions?
class (Boolean s, Container s) => Topology s where
    isOpen   :: s -> Bool
    isClosed :: s -> Bool

    isClopen :: s -> Bool
    isClopen s = isOpen && isClosed $ s

    isNeighborhood :: Elem s -> s -> Bool

-- |
--
-- TODO: How is this related to Constuctable sets?
-- https://en.wikipedia.org/wiki/Constructible_set_%28topology%29
class (Monoid s, Container s) => Unfoldable s where
    -- | creates the smallest container with the given element
    --
    -- > elem x (singleton x) == True
    --
    -- but it is not necessarily the case that
    --
    -- > x /= y   ===>   elem y (singleton x) == False
    --
    -- TODO: should we add this restriction?
    singleton :: Elem s -> s

    -- | FIXME: if -XOverloadedLists is enabled, this causes an infinite loop for some reason
    fromList :: [Elem s] -> s
    fromList xs = foldr cons zero xs

    fromListN :: Int -> [Elem s] -> s
    fromListN _ = fromList

    -- | inserts an element on the left
    cons :: Elem s -> s -> s
    cons x xs = singleton x + xs

    -- | inserts an element on the right;
    -- in a non-abelian Unfoldable, this may not insert the element;
    -- this occurs, for example, in the Map type.
    snoc :: s -> Elem s -> s
    snoc xs x = xs + singleton x

law_Unfoldable_singleton :: Unfoldable s => s -> Elem s -> Logic s
law_Unfoldable_singleton s e = elem e $ singleton e `asTypeOf` s

theorem_Unfoldable_insert :: Unfoldable s => s -> Elem s -> Logic s
theorem_Unfoldable_insert s e = elem e (cons e s)

defn_Unfoldable_fromList :: Unfoldable s => s -> [Elem s] -> Logic s
defn_Unfoldable_fromList s es = fromList es `asTypeOf` s == foldr cons zero es

defn_Unfoldable_fromListN :: (Eq (Elem s), Unfoldable s) => s -> [Elem s] -> Logic s
defn_Unfoldable_fromListN s es = (fromList es `asTypeOf` s)==fromListN (length es) es

defn_Unfoldable_cons :: Unfoldable s => s -> Elem s -> Logic s
defn_Unfoldable_cons s e = cons e s == singleton e + s

defn_Unfoldable_snoc :: Unfoldable s => s -> Elem s -> Logic s
defn_Unfoldable_snoc s e = snoc s e == s + singleton e

-- | curried version of singleton
singletonAt ::
    ( Unfoldable a
    , Elem a ~ (k,v)
    ) => k -> v -> a
singletonAt k v = singleton (k,v)

-- | Insert an element into the container
insert :: Unfoldable s => Elem s -> s -> s
insert = cons

-- | This function needed for the OverloadedStrings language extension
fromString :: (Unfoldable s, Elem s ~ Char) => String -> s
fromString = fromList

-- | Provides inverse operations for "Unfoldable".
--
class Monoid s => Foldable s where

    {-# INLINE toList #-}
    toList :: Foldable s => s -> [Elem s]
    toList s = foldr (:) [] s

    -- |
    --
    -- > unCons zero == Nothing
    --
    -- > unCons (cons x xs) = Just (x, xs)
    --
    unCons :: s -> Maybe (Elem s,s)

    -- |
    --
    -- > unSnoc zero == Nothing
    --
    -- > unSnoc (snoc xs x) = Just (xs, x)
    --
    unSnoc :: s -> Maybe (s,Elem s)

    -- |
    --
    -- prop> isEmpty x == (abs x == 0)
    isEmpty :: s -> Bool
    isEmpty s = case unCons s of
        Nothing -> True
        otherwise -> False

    foldMap :: Monoid a => (Elem s -> a) -> s -> a
    foldr   :: (Elem s -> a -> a) -> a -> s -> a
    foldr'  :: (Elem s -> a -> a) -> a -> s -> a
    foldl   :: (a -> Elem s -> a) -> a -> s -> a
    foldl'  :: (a -> Elem s -> a) -> a -> s -> a

    foldr1  :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldr1  f s = foldr1  f (toList s)
    foldr1' :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldr1' f s = foldr1' f (toList s)
    foldl1  :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldl1  f s = foldl1  f (toList s)
    foldl1' :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldl1' f s = foldl1' f (toList s)

    -- | the default summation uses kahan summation
    sum :: Monoid (Elem s) => s -> Elem s
    sum xs = foldl' (+) zero $ toList xs

--     sum :: (Abelian (Elem s), Group (Elem s)) => s -> Elem s
--     sum = snd . foldl' go (zero,zero)
--         where
--             go (c,t) i = ((t'-t)-y,t')
--                 where
--                     y = i-c
--                     t' = t+y

-- | This fold is not in any of the standard libraries.
foldtree1 :: Monoid a => [a] -> a
foldtree1 as = case go as of
    []  -> zero
    [a] -> a
    as  -> foldtree1 as
    where
        go []  = []
        go [a] = [a]
        go (a1:a2:as) = (a1+a2):go as

{-# INLINE[1] convertContainer #-}
convertContainer :: (Foldable s, Unfoldable t, Elem s ~ Elem t) => s -> t
convertContainer = fromList . toList

{-# INLINE reduce #-}
reduce :: (Monoid (Elem s), Foldable s) => s -> Elem s
reduce s = foldl' (+) zero s

-- | For anything foldable, the norm must be compatible with the folding structure.
{-# INLINE length #-}
length :: (Normed s, Unfoldable s) => s -> Scalar s
length = abs

{-# INLINE and #-}
and :: (Foldable bs, Elem bs~b, Boolean b) => bs -> b
and = foldl' inf true

{-# INLINE or #-}
or :: (Foldable bs, Elem bs~b, Boolean b) => bs -> b
or = foldl' sup false

{-# INLINE argmin #-}
argmin :: Ord b => a -> a -> (a -> b) -> a
argmin a1 a2 f = if f a1 < f a2 then a1 else a2

{-# INLINE argmax #-}
argmax :: Ord b => a -> a -> (a -> b) -> a
argmax a1 a2 f = if f a1 > f a2 then a1 else a2

-- {-# INLINE argminimum_ #-}
-- argminimum_ :: Ord_ b => a -> [a] -> (a -> b) -> a
-- argminimum_ a as f = fstHask $ foldl' go (a,f a) as
--     where
--         go (a1,fa1) a2 = if fa1 < fa2
--             then (a1,fa1)
--             else (a2,fa2)
--             where fa2 = f a2
--
-- {-# INLINE argmaximum_ #-}
-- argmaximum_ :: Ord_ b => a -> [a] -> (a -> b) -> a
-- argmaximum_ a as f = fstHask $ foldl' go (a,f a) as
--     where
--         go (a1,fa1) a2 = if fa1 > fa2
--             then (a1,fa1)
--             else (a2,fa2)
--             where fa2 = f a2

{-# INLINE maximum #-}
maximum :: Bounded b => [b] -> b
maximum = supremum

{-# INLINE maximum_ #-}
maximum_ :: Ord_ b => b -> [b] -> b
maximum_ = supremum_

{-# INLINE minimum #-}
minimum :: Bounded b => [b] -> b
minimum = infimum

{-# INLINE minimum_ #-}
minimum_ :: Ord_ b => b -> [b] -> b
minimum_ = infimum_

{-# INLINE supremum #-}
supremum :: (Foldable bs, Elem bs~b, Bounded b) => bs -> b
supremum = supremum_ minBound

{-# INLINE supremum_ #-}
supremum_ :: (Foldable bs, Elem bs~b, Lattice_ b) => b -> bs -> b
supremum_ = foldl' sup

{-# INLINE infimum #-}
infimum :: (Foldable bs, Elem bs~b, Bounded b) => bs -> b
infimum = infimum_ maxBound

{-# INLINE infimum_ #-}
infimum_ :: (Foldable bs, Elem bs~b, POrd_ b) => b -> bs -> b
infimum_ = foldl' inf

{-# INLINE concat #-}
concat :: (Monoid (Elem s), Foldable s) => s -> Elem s
concat = foldl' (+) zero

{-# INLINE headMaybe #-}
headMaybe :: Foldable s => s -> Maybe (Elem s)
headMaybe = P.fmap fst . unCons

{-# INLINE tailMaybe #-}
tailMaybe :: Foldable s => s -> Maybe s
tailMaybe = P.fmap snd . unCons

{-# INLINE lastMaybe #-}
lastMaybe :: Foldable s => s -> Maybe (Elem s)
lastMaybe = P.fmap snd . unSnoc

{-# INLINE initMaybe #-}
initMaybe :: Foldable s => s -> Maybe s
initMaybe = P.fmap fst . unSnoc

-- | A Partitionable container can be split up into an arbitrary number of subcontainers of roughly equal size.
class (Monoid t, Container t) => Partitionable t where

    -- | The Int must be >0
    partition :: Int -> t -> [t]

law_Partitionable_length :: (ClassicalLogic t, Partitionable t) => Int -> t -> Bool
law_Partitionable_length n t
    | n > 0 = length (partition n t) <= n
    | otherwise = True

law_Partitionable_monoid :: (ClassicalLogic t, Partitionable t) => Int -> t -> Bool
law_Partitionable_monoid n t
    | n > 0 = sum (partition n t) == t
    | otherwise = True

instance (Eq_ a, Boolean (Logic a)) => Partitionable [a] where
    partition = partition_noncommutative
--     partition n xs = go xs
--         where
--             go [] = []
--             go xs =  a:go b
--                 where
--                     (a,b) = P.splitAt len xs
--
--             size = length xs
--             len = size `div` n
--                 + if size `rem` n == 0 then 0 else 1

-- | This is an alternative definition for list partitioning.
-- It should be faster on large lists because it only requires one traversal.
-- But it also breaks parallelism for non-commutative operations.
partition_noncommutative :: Int -> [a] -> [[a]]
partition_noncommutative n xs = [map snd $ P.filter (\(i,x)->i `mod` n==j) ixs | j<-[0..n-1]]
    where
        ixs = addIndex 0 xs
        addIndex i [] = []
        addIndex i (x:xs) = (i,x):(addIndex (i+1) xs)


-- | Parallelizes any batch trainer to run over multiple processors on a single machine.
{-# INLINE [2] parallelN #-}
parallelN ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => Int -- ^ number of parallel threads
      -> (domain -> range) -- ^ sequential batch trainer
      -> (domain -> range) -- ^ parallel batch trainer
parallelN n f =  foldtree1 . parMap rdeepseq f . partition n

-- | Parallelizes any monoid homomorphism.
-- The function automatically detects the number of available processors and parallelizes the function accordingly.
-- This requires the use of unsafePerformIO, however, the result is safe.
{-# INLINE [2] parallel #-}
parallel ::
    ( Partitionable domain
    , Monoid range
    , NFData range
    ) => (domain -> range) -- ^ sequential batch trainer
      -> (domain -> range) -- ^ parallel batch trainer
parallel = if dopar
    then parallelN numproc
    else id
    where
        numproc = unsafePerformIO getNumCapabilities
        dopar = numproc > 1

-------------------

type instance Scalar [a] = Int
type instance Logic [a] = Logic a
type instance Elem [a] = a

instance (Heyting (Logic a), Eq_ a) => Eq_ [a] where
    (x:xs)==(y:ys) = x==y && xs==ys
    (x:xs)==[]     = false
    []    ==(y:ts) = false
    []    ==[]     = true

instance (Logic a~Bool, POrd_ a) => POrd_ [a] where
    inf [] _  = []
    inf _  [] = []
    inf (x:xs) (y:ys) = if x==y
        then x:inf xs ys
        else []

instance (Logic a~Bool, POrd_ a) => MinBound_ [a] where
    minBound = []

instance Normed [a] where
    abs = P.length

instance Semigroup [a] where
    (+) = (P.++)

instance Monoid [a] where
    zero = []

instance (Boolean (Logic a), Eq_ a) => Container [a] where
    elem _ []       = false
    elem x (y:ys)   = x==y || elem x ys

    notElem = not elem

instance (Boolean (Logic a), Eq_ a) => Unfoldable [a] where
    singleton a = [a]
    cons x xs = x:xs

instance Foldable [a] where
    unCons [] = Nothing
    unCons (x:xs) = Just (x,xs)

    unSnoc [] = Nothing
    unSnoc xs = Just (P.init xs,P.last xs)

    foldMap f s = concat $ P.map f s

    foldr = L.foldr
    foldr' = L.foldr
    foldr1 = L.foldr1
    foldr1' = L.foldr1

    foldl = L.foldl
    foldl' = L.foldl'
    foldl1 = L.foldl1
    foldl1' = L.foldl1'

-- type instance Index [a] = Int

-- instance Indexed [a] where
--     (!!) [] _ = Nothing
--     (!!) (x:xs) 0 = Just x
--     (!!) (x:xs) i = xs !! (i-1)

