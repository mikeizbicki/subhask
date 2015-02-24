{-# LANGUAGE CPP,MagicHash,UnboxedTuples #-}
{-# LANGUAGE IncoherentInstances #-}

-- | This module defines the algebraic type-classes used in subhask.
-- The class hierarchies are significantly more general than those in the standard Prelude.
module SubHask.Algebra
    (
    -- * Comparisons
    Logic
    , ValidLogic
    , ClassicalLogic
    , Eq_ (..)
    , Eq
    , ValidEq
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
    , Complemented (..)
    , law_Complemented_not
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
    , law_Container_preservation
--     , law_Unfoldable_empty
--     , defn_Container_sizeDisjoint

    , Constructible (..)
    , fromString
    , fromList
    , fromListN
    , insert
    , empty
    , isEmpty
    , defn_Constructible_cons
    , defn_Constructible_snoc
    , defn_Constructible_fromList
    , defn_Constructible_fromListN
--     , law_Constructible_MonoidMinBound
--     , law_Constructible_MonoidNormed
--     , defn_Constructible_infDisjoint
    , law_Constructible_singleton
    , theorem_Constructible_cons

    , Foldable (..)
    , foldtree1
    , length
    , reduce
    , concat
    , headMaybe
    , tailMaybe
    , lastMaybe
    , initMaybe

    -- *** indexed containers
    , Index
    , SetIndex

    , Value
    , SetValue

    , IxConstructible (..)
    , IxContainer (..)
    , (!?)


    -- * Maybe
    , CanError (..)
    , Maybe' (..)
    , Labeled' (..)

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
    , defn_Monoid_isZero
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

    -- ** Sizes
    , Normed (..)
    , abs
    , Metric (..)
    , isFartherThan
    , lb2distanceUB
    , law_Metric_nonnegativity
    , law_Metric_indiscernables
    , law_Metric_symmetry
    , law_Metric_triangle

    -- ** Linear algebra
    , Scalar
    , IsScalar
    , HasScalar
    , Cone (..)
    , Module (..)
    , (.*)
    , VectorSpace (..)
    , Banach (..)
    , Hilbert (..)
    , innerProductDistance
    , innerProductNorm
    , OuterProductSpace (..)

    )
    where

import qualified Prelude as P
import qualified Data.List as L

import Prelude (Ordering (..))
import Control.Monad
import Data.Ratio
import Data.Typeable
import Test.QuickCheck (Arbitrary (..), frequency)

import Control.Concurrent
import Control.Parallel
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
type instance Logic () = ()

type ValidLogic a = Complemented (Logic a)

-- | Classical logic is implemented using the Prelude's Bool type.
class Logic a ~ Bool => ClassicalLogic a
instance Logic a ~ Bool => ClassicalLogic a

-- | Defines equivalence classes over the type.
-- The values need not have identical representations in the machine to be equal.
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

instance Eq_ ()       where () == () = ()

instance Eq_ Bool     where (==) = (P.==); (/=) = (P./=)
instance Eq_ Char     where (==) = (P.==); (/=) = (P./=)
instance Eq_ Int      where (==) = (P.==); (/=) = (P./=)
instance Eq_ Integer  where (==) = (P.==); (/=) = (P./=)
instance Eq_ Rational where (==) = (P.==); (/=) = (P./=)
instance Eq_ Float    where (==) = (P.==); (/=) = (P./=)
instance Eq_ Double   where (==) = (P.==); (/=) = (P./=)

instance Eq_ b => Eq_ (a -> b) where
    (f==g) a = f a == f a

class (Eq_ a, Logic a ~ Bool) => Eq a
instance (Eq_ a, Logic a ~ Bool) => Eq a

class (Eq_ a, ValidLogic a) => ValidEq a
instance (Eq_ a, ValidLogic a) => ValidEq a

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

instance POrd_ ()         where inf () () = ()
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

instance MinBound_ ()   where minBound = ()
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

type instance Logic Ordering = Bool

instance Eq_ Ordering where
    EQ == EQ = True
    LT == LT = True
    GT == GT = True
    _  == _  = False

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

instance Lattice_ ()         where sup () () = ()

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
isChain (x:xs) = all (/=PNA) (map (pcompare x) xs) && isChain xs
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

instance Ord_ ()        --where compare = P.compare
instance Ord_ Char      where compare = P.compare
instance Ord_ Int       where compare = P.compare
instance Ord_ Integer   where compare = P.compare
instance Ord_ Float     where compare = P.compare
instance Ord_ Double    where compare = P.compare
instance Ord_ Rational  where compare = P.compare
instance Ord_ Bool      where compare = P.compare

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

instance Bounded ()     where maxBound = ()
instance Bounded Bool   where maxBound = True
instance Bounded Char   where maxBound = P.maxBound
instance Bounded Int    where maxBound = P.maxBound
instance Bounded Float  where maxBound = 1/0 -- FIXME: should be a primop for infinity
instance Bounded Double where maxBound = 1/0
instance Bounded b => Bounded (a -> b) where maxBound = \x -> maxBound

class Bounded b => Complemented b where
    not :: b -> b

law_Complemented_not :: (ValidLogic b, Complemented b) => b -> Logic b
law_Complemented_not b = not (true  `asTypeOf` b) == false
                      && not (false `asTypeOf` b) == true

instance Complemented ()   where not () = ()
instance Complemented Bool where not = P.not
instance Complemented b => Complemented (a -> b) where not f = \x -> not $ f x

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

instance Heyting ()   where () ==> () = ()
instance Heyting Bool where (==>) = modusPonens
instance Heyting b => Heyting (a -> b) where (f==>g) a = f a ==> g a

-- | Generalizes Boolean variables.
--
-- See <https://en.wikipedia.org/wiki/Boolean_algebra_%28structure%29 wikipedia> for more details.
class (Complemented b, Heyting b) => Boolean b where

law_Boolean_infcomplement :: (Eq b, Boolean b) => b -> Bool
law_Boolean_infcomplement b = (b || not b) == true

law_Boolean_supcomplement :: (Eq b, Boolean b) => b -> Bool
law_Boolean_supcomplement b = (b && not b) == false

law_Boolean_infdistributivity :: (Eq b, Boolean b) => b -> b -> b -> Bool
law_Boolean_infdistributivity b1 b2 b3 = (b1 || (b2 && b3)) == ((b1 || b2) && (b1 || b3))

law_Boolean_supdistributivity :: (Eq b, Boolean b) => b -> b -> b -> Bool
law_Boolean_supdistributivity b1 b2 b3 = (b1 && (b2 || b3)) == ((b1 && b2) || (b1 && b3))

instance Boolean ()
instance Boolean Bool
instance Boolean b => Boolean (a -> b)

-------------------------------------------------------------------------------
-- numeric classes

class Semigroup g where
    infixl 6 +
    (+) :: g -> g -> g

    sgErrorBound :: HasScalar g => g -> Scalar g
    sgErrorBound = 0

-- law_Semigroup_associativity ::
--     ( HasScalar g
--     , Semigroup g
--     , Metric g
--     ) => g -> g -> g -> Bool
-- law_Semigroup_associativity g1 g2 g3 = associator g1 g2 g3 <= sgErrorBound g1

associator :: (Semigroup g, Metric g) => g -> g -> g -> Scalar g
associator g1 g2 g3 = distance ((g1+g2)+g3) (g1+(g2+g3))

-- normedAssociator g1 g2 g3
--     = abs $ 1 - (toRational_ $ size ((g1+g2)+g3))
--               / (toRational_ $ size (g1+(g2+g3)))

toRational_ :: (a <: Rational) => a -> Rational
toRational_ = embedType

law_Semigroup_associativity :: (Eq g, Semigroup g ) => g -> g -> g -> Logic g
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
    = size (   g1 + ( g2    + g3 ) )
    / size ( ( g1 +   g2  ) + g3   )

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle :: Semigroup m => m -> m
cycle xs = xs' where xs' = xs + xs'

instance Semigroup Int      where (+) = (P.+)
instance Semigroup Integer  where (+) = (P.+)
instance Semigroup Float    where (+) = (P.+)
instance Semigroup Double   where (+) = (P.+)
instance Semigroup Rational where (+) = (P.+)

instance Semigroup () where
    ()+() = ()

instance Semigroup   b => Semigroup   (a -> b) where f+g = \a -> f a + g a

---------------------------------------

class Semigroup g => Monoid g where
    zero :: g

-- | FIXME: this should be in the Monoid class
isZero :: (Monoid g, ValidEq g) => g -> Logic g
isZero = (==zero)

law_Monoid_leftid :: (Monoid g, Eq g) => g -> Bool
law_Monoid_leftid g = zero + g == g

law_Monoid_rightid :: (Monoid g, Eq g) => g -> Bool
law_Monoid_rightid g = g + zero == g

defn_Monoid_isZero :: (Monoid g, Eq g) => g -> Bool
defn_Monoid_isZero g = (isZero $ zero `asTypeOf` g)
                    && (g /= zero ==> not isZero g)

---------

instance Monoid Int       where zero = 0
instance Monoid Integer   where zero = 0
instance Monoid Float     where zero = 0
instance Monoid Double    where zero = 0
instance Monoid Rational  where zero = 0

type instance Logic (Maybe a) = Logic a

instance ValidEq a => Eq_ (Maybe a) where
    Nothing   == Nothing   = true
    Nothing   == _         = false
    _         == Nothing   = false
    (Just a1) == (Just a2) = a1==a2

instance Semigroup a => Monoid (Maybe a) where
    zero = Nothing

instance Semigroup a => Monoid (Maybe' a) where
    zero = Nothing'

instance Monoid () where
    zero = ()

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

instance Cancellative b => Cancellative (a -> b) where
    f-g = \a -> f a - g a

-- | The GrothendieckGroup is a general way to construct groups from cancellative semigroups.
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

instance Abelian b => Abelian (a -> b)

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
class (Ring r, Ord_ r, Scalar r~r, Normed r) => IsScalar r
instance (Ring r, Ord_ r, Scalar r~r, Normed r) => IsScalar r

-- FIXME: made into classes due to TH limitations
class (IsScalar (Scalar a)) => HasScalar a
instance (IsScalar (Scalar a)) => HasScalar a

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

-- | FIXME: What constraint should be here? Semigroup?
--
-- See <http://ncatlab.org/nlab/show/normed%20group ncatlab>
class
    ( Ord_ (Scalar g)
    , Scalar (Scalar g) ~ Scalar g
    , Ring (Scalar g)
    ) => Normed g where
    size :: g -> Scalar g

abs :: (Ring g, Normed g) => g -> Scalar g
abs = size

instance Normed Int       where size = P.abs
instance Normed Integer   where size = P.abs
instance Normed Float     where size = P.abs
instance Normed Double    where size = P.abs
instance Normed Rational  where size = P.abs

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

class (Abelian m, Group m, HasScalar m) => Module m where
    infixl 7 *.
    (*.) :: Scalar m -> m -> m

    infixl 7 .*.
    (.*.) :: m -> m -> m

{-# INLINE (.*) #-}
infixl 7 .*
(.*) :: Module m => m -> Scalar m -> m
m .* r  = r *. m

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

instance VectorSpace Float     where (./) = (/); (./.) = (/)
instance VectorSpace Double    where (./) = (/); (./.) = (/)
instance VectorSpace Rational  where (./) = (/); (./.) = (/)

instance VectorSpace b => VectorSpace (a -> b) where g ./. f = \a -> g a ./. f a

---------------------------------------

-- | A Banach space is a Vector Space equipped with a compatible Norm and Metric.
--
-- See <http://en.wikipedia.org/wiki/Banach_space wikipedia> for more details.
class (VectorSpace v, Normed v, Metric v) => Banach v where
    {-# INLINABLE normalize #-}
    normalize :: v -> v
    normalize v = v ./ size v

law_Banach_distance :: Banach v => v -> v -> Logic (Scalar v)
law_Banach_distance v1 v2 = size (v1 - v2) == distance v1 v2

law_Banach_size :: Banach v => v -> Logic (Scalar v)
law_Banach_size v
    = isZero v
   || size (normalize v) == 1

instance Banach Float
instance Banach Double
instance Banach Rational

---------------------------------------

-- | Hilbert spaces are a natural generalization of Euclidean space that allows for infinite dimension.
--
-- See <http://en.wikipedia.org/wiki/Hilbert_space wikipedia> for more details.
class
    ( Banach v
    , Floating (Scalar v)
    ) => Hilbert v
        where

    infix 8 <>
    (<>) :: v -> v -> Scalar v

instance Hilbert Float    where (<>) = (*)
instance Hilbert Double   where (<>) = (*)

{-# INLINE squaredInnerProductNorm #-}
squaredInnerProductNorm :: Hilbert v => v -> Scalar v
squaredInnerProductNorm v = v<>v

{-# INLINE innerProductNorm #-}
innerProductNorm :: Hilbert v => v -> Scalar v
innerProductNorm = sqrt . squaredInnerProductNorm

{-# INLINE innerProductDistance #-}
innerProductDistance :: Hilbert v => v -> v -> Scalar v
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

{-
-- | Bregman divergences generalize the squared Euclidean distance and the KL-divergence.
-- They are closely related to exponential family distributions.
--
-- Mark Reid has a <http://mark.reid.name/blog/meet-the-bregman-divergences.html good tutorial>.
--
-- FIXME:
-- The definition of divergence requires taking the derivative.
-- How should this relate to categories?
class
    ( Hilbert v
    ) => Bregman v
        where

    divergence :: v -> v -> Scalar v
    divergence v1 v2 = f v1 - f v2 - (derivative f v2 <> v1 - v2)
        where
            f = bregmanFunction

    bregmanFunction :: v -> Scalar v

law_Bregman_nonnegativity :: v -> v -> Logic v
law_Bregman_nonnegativity v1 v2 = divergence v1 v2 > 0

law_Bregman_triangle ::
-}

---------------------------------------

-- | Metric spaces give us the most intuitive notion of distance between objects.
--
-- FIXME: There are many other notions of distance and we should make a while hierarchy.
class
    ( HasScalar v
    , Eq_ v
    , Boolean (Logic v)
    , Logic (Scalar v) ~ Logic v
    ) => Metric v
        where

    distance :: v -> v -> Scalar v

    -- | If the distance between two datapoints is less than or equal to the upper bound,
    -- then this function will return the distance.
    -- Otherwise, it will return some number greater than the upper bound.
    {-# INLINABLE distanceUB #-}
    distanceUB :: v -> v -> Scalar v -> Scalar v
    distanceUB v1 v2 _ = {-# SCC distanceUB #-} distance v1 v2

-- | Calling this function will be faster on some 'Metric's than manually checking if distance is greater than the bound.
{-# INLINABLE isFartherThan #-}
isFartherThan :: Metric v => v -> v -> Scalar v -> Logic v
isFartherThan s1 s2 b = {-# SCC isFartherThan #-} distanceUB s1 s2 b > b

-- | This function constructs an efficient default implementation for 'distanceUB' given a function that lower bounds the distance metric.
{-# INLINABLE lb2distanceUB #-}
lb2distanceUB ::
    ( Metric a
    , ClassicalLogic a
    ) => (a -> a -> Scalar a)
      -> (a -> a -> Scalar a -> Scalar a)
lb2distanceUB lb p q b = if lbpq > b
    then lbpq
    else distance p q
    where
        lbpq = lb p q
law_Metric_nonnegativity :: Metric v => v -> v -> Logic v
law_Metric_nonnegativity v1 v2 = distance v1 v2 >= 0

law_Metric_indiscernables :: (Eq v, Metric v) => v -> v -> Logic v
law_Metric_indiscernables v1 v2 = if v1 == v2
    then distance v1 v2 == 0
    else distance v1 v2 > 0

law_Metric_symmetry :: Metric v => v -> v -> Logic v
law_Metric_symmetry v1 v2 = distance v1 v2 == distance v2 v1

law_Metric_triangle :: Metric v => v -> v -> v -> Logic v
law_Metric_triangle m1 m2 m3
    = distance m1 m2 <= distance m1 m3 + distance m2 m3
   && distance m1 m3 <= distance m1 m2 + distance m2 m3
   && distance m2 m3 <= distance m1 m3 + distance m2 m1

instance Metric Int      where distance x1 x2 = abs $ x1 - x2
instance Metric Integer  where distance x1 x2 = abs $ x1 - x2
instance Metric Float    where distance x1 x2 = abs $ x1 - x2
instance Metric Double   where distance x1 x2 = abs $ x1 - x2
instance Metric Rational where distance x1 x2 = abs $ x1 - x2

---------

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

type Item s = Elem s

type family Elem s
type family SetElem s t

-- | Two sets are disjoint if their infimum is the empty set.
-- This function generalizes the notion of disjointness for any lower bounded lattice.
-- FIXME: add other notions of disjoint
infDisjoint :: (Constructible s, MinBound s, Monoid s) => s -> s -> Logic s
infDisjoint s1 s2 = isEmpty $ inf s1 s2

sizeDisjoint :: (Normed s, Constructible s) => s -> s -> Logic (Scalar s)
sizeDisjoint s1 s2 = size s1 + size s2 == size (s1+s2)

-- | This is the class for any type that gets "constructed" from smaller types.
-- It is a massive generalization of the notion of a constructable set in topology.
--
-- See <https://en.wikipedia.org/wiki/Constructible_set_%28topology%29 wikipedia> for more details.
class Semigroup s => Constructible s where

    {-# MINIMAL singleton | cons | fromList1 #-}

    -- | creates the smallest value containing the given element
    singleton :: Elem s -> s
    singleton x = fromList1N 1 x []

    -- | inserts an element on the left
    cons :: Elem s -> s -> s
    cons x xs = singleton x + xs

    -- | inserts an element on the right;
    -- in a non-abelian 'Constructible', this may not insert the element;
    -- this occurs, for example, in the Map type.
    snoc :: s -> Elem s -> s
    snoc xs x = xs + singleton x

    -- | Construct the type from a list.
    -- Since lists may be empty (but not all 'Constructible's can be empty) we explicitly pass in an Elem s.
    fromList1 :: Elem s -> [Elem s] -> s
    fromList1 x xs = foldl' snoc (singleton x) xs

    -- | Like "fromList1" but passes in the size of the list for more efficient construction.
    fromList1N :: Int -> Elem s -> [Elem s] -> s
    fromList1N _ = fromList1

defn_Constructible_fromList :: (Eq_ s, Constructible s) => s -> Elem s -> [Elem s] -> Logic s
defn_Constructible_fromList s e es = fromList1 e es `asTypeOf` s == foldl' snoc (singleton e) es

defn_Constructible_fromListN :: (Eq_ s, Constructible s) => s -> Elem s -> [Elem s] -> Logic s
defn_Constructible_fromListN s e es = (fromList1 e es `asTypeOf` s)==fromList1N (size es+1) e es

defn_Constructible_cons :: (Eq_ s, Constructible s) => s -> Elem s -> Logic s
defn_Constructible_cons s e = cons e s == singleton e + s

defn_Constructible_snoc :: (Eq_ s, Constructible s) => s -> Elem s -> Logic s
defn_Constructible_snoc s e = snoc s e == s + singleton e

-- | A more suggestive name for inserting an element into a container that does not remember location
insert :: Constructible s => Elem s -> s -> s
insert = cons

-- | A slightly more suggestive name for a container's monoid identity
empty :: (Monoid s, Constructible s) => s
empty = zero

-- | A slightly more suggestive name for checking if a container is empty
isEmpty :: (ValidEq s, Monoid s, Constructible s) => s -> Logic s
isEmpty = isZero

-- | This function needed for the OverloadedStrings language extension
fromString :: (Monoid s, Constructible s, Elem s ~ Char) => String -> s
fromString = fromList

-- | FIXME: if -XOverloadedLists is enabled, this causes an infinite loop for some reason
fromList :: (Monoid s, Constructible s) => [Elem s] -> s
fromList [] = zero
fromList (x:xs) = fromList1 x xs

fromListN :: (Monoid s, Constructible s) => Int -> [Elem s] -> s
fromListN 0 [] = zero
fromListN i (x:xs) = fromList1N i x xs

-- | This is a generalization of a "set".
-- We do not require a container to be a boolean algebra, just a semigroup.
class (ValidLogic s, Constructible s) => Container s where
    elem :: Elem s -> s -> Logic s

    notElem :: Elem s -> s -> Logic s
    notElem = not elem

law_Container_preservation :: (Heyting (Logic s), Container s) => s -> s -> Elem s -> Logic s
law_Container_preservation s1 s2 e = (e `elem` s1 || e `elem` s2) ==> (e `elem` (s1+s2))

law_Constructible_singleton :: Container s => s -> Elem s -> Logic s
law_Constructible_singleton s e = elem e $ singleton e `asTypeOf` s

theorem_Constructible_cons :: Container s => s -> Elem s -> Logic s
theorem_Constructible_cons s e = elem e (cons e s)

-- |
--
-- FIXME:
-- This is a correct definition of topologies, but is it useful?
-- How can this relate to continuous functions?
class (Boolean (Logic s), Boolean s, Container s) => Topology s where
    open :: s -> Logic s

    closed :: s -> Logic s
    closed s = open $ not s

    clopen :: s -> Logic s
    clopen = open && closed

-- | Provides inverse operations for "Unfoldable".
class (Constructible s, Monoid s) => Foldable s where

    {-# INLINE toList #-}
    toList :: Foldable s => s -> [Elem s]
    toList s = foldr (:) [] s

    -- | Remove an element from the left of the container.
    uncons :: s -> Maybe (Elem s,s)

    -- | Remove an element from the right of the container.
    unsnoc :: s -> Maybe (s,Elem s)

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

{-# INLINE[1] convertUnfoldable #-}
convertUnfoldable :: (Monoid t, Foldable s, Constructible t, Elem s ~ Elem t) => s -> t
convertUnfoldable = fromList . toList

{-# INLINE reduce #-}
reduce :: (Monoid (Elem s), Foldable s) => s -> Elem s
reduce s = foldl' (+) zero s

-- | For anything foldable, the norm must be compatible with the folding structure.
{-# INLINE length #-}
length :: Normed s => s -> Scalar s
length = size

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
maximum :: (ValidLogic b, Bounded b) => [b] -> b
maximum = supremum

{-# INLINE maximum_ #-}
maximum_ :: (ValidLogic b, Ord_ b) => b -> [b] -> b
maximum_ = supremum_

{-# INLINE minimum #-}
minimum :: (ValidLogic b, Bounded b) => [b] -> b
minimum = infimum

{-# INLINE minimum_ #-}
minimum_ :: (ValidLogic b, Ord_ b) => b -> [b] -> b
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
headMaybe = P.fmap fst . uncons

{-# INLINE tailMaybe #-}
tailMaybe :: Foldable s => s -> Maybe s
tailMaybe = P.fmap snd . uncons

{-# INLINE lastMaybe #-}
lastMaybe :: Foldable s => s -> Maybe (Elem s)
lastMaybe = P.fmap snd . unsnoc

{-# INLINE initMaybe #-}
initMaybe :: Foldable s => s -> Maybe s
initMaybe = P.fmap fst . unsnoc

----------------------------------------

type family Index s
type family Value s
type family SetValue s a
type family SetIndex s a

class IxConstructible s where
    singletonAt :: Index s -> Value s -> s
    insertAt    :: Index s -> Value s -> s -> s

class IxContainer s where
    lookup :: Index s -> s -> Maybe (Value s)

    {-# INLINABLE (!) #-}
    (!) :: s -> Index s -> Value s
    (!) s i = case lookup i s of
        Just x -> x
        Nothing -> error "used (!) on an invalid index"

    {-# INLINABLE findWithDefault #-}
    findWithDefault :: Value s -> Index s -> s -> Value s
    findWithDefault def i s = case s !? i of
        Nothing -> def
        Just e -> e

    {-# INLINABLE hasIndex #-}
    hasIndex :: Index s -> s -> Bool
    hasIndex i s = case s !? i of
        Nothing -> False
        Just _ -> True

    -- | FIXME: should the functions below be moved to other classes?
    indices :: s -> [Index s]
    values  :: s -> [Value s]
    imap ::  (Index s -> Value s -> b) -> s -> SetValue s b

-- | An infix operator equivalent to 'lookup'
{-# INLINABLE (!?) #-}
(!?) :: IxContainer s => s -> Index s -> Maybe (Value s)
(!?) s i = lookup i s

--------------------------------------------------------------------------------

type instance Scalar [a] = Int
type instance Logic [a] = Logic a
type instance Elem [a] = a

instance ValidEq a => Eq_ [a] where
    (x:xs)==(y:ys) = x==y && xs==ys
    (x:xs)==[]     = false
    []    ==(y:ts) = false
    []    ==[]     = true

instance Eq a => POrd_ [a] where
    inf [] _  = []
    inf _  [] = []
    inf (x:xs) (y:ys) = if x==y
        then x:inf xs ys
        else []

instance Eq a => MinBound_ [a] where
    minBound = []

instance Normed [a] where
    size = P.length

instance Semigroup [a] where
    (+) = (P.++)

instance Monoid [a] where
    zero = []

instance ValidEq a => Container [a] where
    elem _ []       = false
    elem x (y:ys)   = x==y || elem x ys

    notElem = not elem

instance Constructible [a] where
    singleton a = [a]
    cons x xs = x:xs
    fromList1 x xs = x:xs
    fromList1N _ x xs = x:xs

instance Foldable [a] where
    uncons [] = Nothing
    uncons (x:xs) = Just (x,xs)

    unsnoc [] = Nothing
    unsnoc xs = Just (P.init xs,P.last xs)

    foldMap f s = concat $ map f s

    foldr = L.foldr
    foldr' = L.foldr
    foldr1 = L.foldr1
    foldr1' = L.foldr1

    foldl = L.foldl
    foldl' = L.foldl'
    foldl1 = L.foldl1
    foldl1' = L.foldl1'

----------------------------------------

instance Semigroup a => Semigroup (Maybe a) where
    (Just a1) + (Just a2) = Just $ a1+a2
    Nothing   + a2        = a2
    a1        + Nothing   = a1

----------

data Maybe' a
    = Nothing'
    | Just' !a

type instance Logic (Maybe' a) = Logic a

instance NFData a => NFData (Maybe' a) where
    rnf Nothing' = ()
    rnf (Just' a) = rnf a

instance ValidEq a => Eq_ (Maybe' a) where
    (Just' a1) == (Just' a2) = a1==a2
    Nothing'   == Nothing'   = true
    _          == _          = false

instance Semigroup a => Semigroup (Maybe' a) where
    (Just' a1) + (Just' a2) = Just' $ a1+a2
    Nothing'   + a2         = a2
    a1         + Nothing'   = a1

----------------------------------------

type instance Logic (a,b) = Logic a
type instance Logic (a,b,c) = Logic a

instance (ValidEq a, ValidEq b, Logic a ~ Logic b) => Eq_ (a,b) where
    (a1,b1)==(a2,b2) = a1==a2 && b1==b2

instance (ValidEq a, ValidEq b, ValidEq c, Logic a ~ Logic b, Logic b~Logic c) => Eq_ (a,b,c) where
    (a1,b1,c1)==(a2,b2,c2) = a1==a2 && b1==b2 && c1==c2

instance (Logic a~Logic b, Semigroup a, Semigroup b) => Semigroup (a,b) where
    (a1,b1)+(a2,b2) = (a1+a2,b1+b2)

instance (Logic a~Logic b, Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
    (a1,b1,c1)+(a2,b2,c2) = (a1+a2,b1+b2,c1+c2)

instance (Logic a~Logic b, Monoid a, Monoid b) => Monoid (a,b) where
    zero = (zero,zero)

instance (Logic a~Logic b, Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
    zero = (zero,zero,zero)

instance (Logic a~Logic b, Cancellative a, Cancellative b) => Cancellative (a,b) where
    (a1,b1)-(a2,b2) = (a1-a2,b1-b2)

instance (Logic a~Logic b, Cancellative a, Cancellative b, Cancellative c) => Cancellative (a,b,c) where
    (a1,b1,c1)-(a2,b2,c2) = (a1-a2,b1-b2,c1-c2)

instance (Logic a~Logic b, Group a, Group b) => Group (a,b) where
    negate (a,b) = (negate a,negate b)

instance (Logic a~Logic b, Group a, Group b, Group c) => Group (a,b,c) where
    negate (a,b,c) = (negate a,negate b,negate c)

instance (Logic a~Logic b, Abelian a, Abelian b) => Abelian (a,b)

instance (Logic a~Logic b, Abelian a, Abelian b, Abelian c) => Abelian (a,b,c)

instance (Logic a~Logic b, Module a, Module b, Scalar a ~ Scalar b) => Module (a,b) where
    r *. (a,b) = (r*.a, r*.b)
    (a1,b1).*.(a2,b2) = (a1.*.a2,b1.*.b2)

instance (Logic a~Logic b, Module a, Module b, Module c, Scalar a ~ Scalar b, Scalar c~Scalar b) => Module (a,b,c) where
    r *. (a,b,c) = (r*.a, r*.b,r*.c)
    (a1,b1,c1).*.(a2,b2,c2) = (a1.*.a2,b1.*.b2,c1.*.c2)

instance (Logic a~Logic b, VectorSpace a,VectorSpace b, Scalar a ~ Scalar b) => VectorSpace (a,b) where
    (a,b) ./ r = (a./r,b./r)
    (a1,b1)./.(a2,b2) = (a1./.a2,b1./.b2)

instance (Logic a~Logic b, VectorSpace a,VectorSpace b, VectorSpace c, Scalar a ~ Scalar b, Scalar c~Scalar b) => VectorSpace (a,b,c) where
    (a,b,c) ./ r = (a./r,b./r,c./r)
    (a1,b1,c1)./.(a2,b2,c2) = (a1./.a2,b1./.b2,c1./.c2)

--------------------------------------------------------------------------------

data Labeled' x y = Labeled' { xLabeled' :: !x, yLabeled' :: !y }
    deriving (Read,Show)

instance (NFData x, NFData y) => NFData (Labeled' x y) where
    rnf (Labeled' x y) = deepseq x $ rnf y

instance (Arbitrary x, Arbitrary y) => Arbitrary (Labeled' x y) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Labeled' x y

type instance Scalar (Labeled' x y) = Scalar x
type instance Logic (Labeled' x y) = Logic x
type instance Elem (Labeled' x y) = Elem x

-----

instance Eq_ x => Eq_ (Labeled' x y) where
    (Labeled' x1 y1) == (Labeled' x2 y2) = x1==x2

instance (ClassicalLogic x, Ord_ x) => POrd_ (Labeled' x y) where
    inf (Labeled' x1 y1) (Labeled' x2 y2) = if x1 < x2
        then Labeled' x1 y1
        else Labeled' x2 y2
    (Labeled' x1 _)< (Labeled' x2 _) = x1< x2
    (Labeled' x1 _)<=(Labeled' x2 _) = x1<=x2

instance (ClassicalLogic x, Ord_ x) => Lattice_ (Labeled' x y) where
    sup (Labeled' x1 y1) (Labeled' x2 y2) = if x1 >= x2
        then Labeled' x1 y1
        else Labeled' x2 y2
    (Labeled' x1 _)> (Labeled' x2 _) = x1> x2
    (Labeled' x1 _)>=(Labeled' x2 _) = x1>=x2

instance (ClassicalLogic x, Ord_ x) => Ord_ (Labeled' x y) where

-----

instance Metric x => Metric (Labeled' x y) where
    distance (Labeled' x1 y1) (Labeled' x2 y2) = distance x1 x2
    distanceUB (Labeled' x1 y1) (Labeled' x2 y2) = distanceUB x1 x2

instance Normed x => Normed (Labeled' x y) where
    size (Labeled' x _) = size x


