{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module defines the algebraic type-classes used in subhask.
-- The class hierarchies are significantly more general than those in the standard Prelude.
module SubHask.Algebra
    (
    -- * Comparisons
    Logic
--     , TLogic
    , IdempLogic
    , Classical
    , ClassicalLogic
    , Elem
--     , TElem
    , Container (..)
    , law_Container_preservation
    , ifThenElse
    , Eq (..)
    , law_Eq_reflexive
    , law_Eq_symmetric
    , law_Eq_transitive
    , defn_Eq_noteq
    , POrd (..)
    , law_POrd_commutative
    , law_POrd_associative
    , theorem_POrd_idempotent
    , Lattice (..)
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
    , MinBound (..)
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
    , Boolean
    , law_Boolean_infcomplement
    , law_Boolean_supcomplement
    , law_Boolean_infdistributivity
    , law_Boolean_supdistributivity
    , Ord (..)
    , law_Ord_totality
    , law_Ord_min
    , law_Ord_max
    , Ordering (..)
    , min
    , max
    , maximum
    , maximum_
    , minimum
    , minimum_
    , argmin
    , argmax
    , Graded (..)
    , law_Graded_fromEnum
    , law_Graded_pred
    , defn_Graded_predN
    , (>.)
    , (<.)
    , Enum (..)
    , law_Enum_toEnum
    , law_Enum_succ
    , defn_Enum_succN

    -- ** Boolean helpers
    , (||)
    , (&&)
    , true
    , false
    , and
    , or

    -- * Set-like
    , Constructible (..)
    , Constructible0
    , law_Constructible_singleton
    , defn_Constructible_cons
    , defn_Constructible_snoc
    , defn_Constructible_fromList
    , defn_Constructible_fromListN
    , theorem_Constructible_cons
    , fromString
    , fromList
    , fromListN
    , generate
    , insert
    , empty
    , isEmpty
    , infDisjoint
    , sizeDisjoint

    , Foldable (..)
    , law_Foldable_sum
    , theorem_Foldable_tofrom
    , defn_Foldable_foldr
    , defn_Foldable_foldr'
    , defn_Foldable_foldl
    , defn_Foldable_foldl'
    , defn_Foldable_foldr1
    , defn_Foldable_foldr1'
    , defn_Foldable_foldl1
    , defn_Foldable_foldl1'

    , foldtree1
    , convertUnfoldable
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

    , IxContainer (..)
    , law_IxContainer_preservation
    , defn_IxContainer_bang
    , defn_IxContainer_findWithDefault
    , defn_IxContainer_hasIndex
    , (!?)

    , Sliceable (..)
    , law_Sliceable_restorable
    , law_Sliceable_preservation

    , IxConstructible (..)
    , law_IxConstructible_lookup
    , defn_IxConstructible_consAt
    , defn_IxConstructible_snocAt
    , defn_IxConstructible_fromIxList
    , theorem_IxConstructible_preservation
    , insertAt

    -- * Types
    , CanError (..)
    , Maybe' (..)
    , justs'
    , Labeled' (..)

    -- * Number-like
    -- ** Classes with one operator
    , Semigroup (..)
    , law_Semigroup_associativity
    , defn_Semigroup_plusequal
    , cycle
    , Actor
    , Action (..)
    , law_Action_compatibility
    , defn_Action_dotplusequal
    , (+.)
    , Cancellative (..)
    , law_Cancellative_rightminus1
    , law_Cancellative_rightminus2
    , defn_Cancellative_plusequal
    , Monoid (..)
    , isZero
    , notZero
    , law_Monoid_leftid
    , law_Monoid_rightid
    , defn_Monoid_isZero
    , Abelian
    , law_Abelian_commutative
    , Group (..)
    , law_Group_leftinverse
    , law_Group_rightinverse
    , defn_Group_negateminus

    -- ** Classes with two operators
    , Rg(..)
    , law_Rg_multiplicativeAssociativity
    , law_Rg_multiplicativeCommutivity
    , law_Rg_annihilation
    , law_Rg_distributivityLeft
    , theorem_Rg_distributivityRight
    , defn_Rg_timesequal
    , Rig(..)
    , isOne
    , notOne
    , law_Rig_multiplicativeId
    , Rng
    , defn_Ring_fromInteger
    , Ring(..)
    , indicator
    , Integral(..)
    , law_Integral_divMod
    , law_Integral_quotRem
    , law_Integral_toFromInverse
    , roundUpToNearest
--     , roundUpToNearestBase2
    , fromIntegral
    , Field(..)
    , OrdField
    , RationalField(..)
    , convertRationalField
    , toFloat
    , toDouble
    , BoundedField(..)
    , infinity
    , negInfinity
    , ExpRing (..)
    , (^)
    , ExpField (..)
    , Real (..)
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
--     , TScalar
    , ValidScalar
    , HasScalar
    , Cone (..)
    , Module (..)
    , law_Module_multiplication
    , law_Module_addition
    , law_Module_action
    , law_Module_unital
    , defn_Module_dotstarequal
    , (*.)
    , FreeModule (..)
    , law_FreeModule_commutative
    , law_FreeModule_associative
    , defn_FreeModule_dotstardotequal
    , FreeModule1 (..)
    , law_FreeModule_id
    , FiniteModule (..)
    , Vector (..)
    , Reisz (..)
    , Banach (..)
    , law_Banach_distance
    , law_Banach_size
    , Hilbert (..)
    , HasValidSquare
    , Transposable (..)
--     , TSquare
    , squaredInnerProductNorm
    , innerProductDistance
    , innerProductNorm

    -- * Helper functions
    , simpleMutableDefn
    , module SubHask.Mutable
    )
    where

import qualified Prelude as P
import qualified Data.Number.Erf as P
import qualified Math.Gamma as P
import qualified Data.List as L

import Control.Monad hiding (liftM)
import Control.Monad.ST
import Data.Ratio
import Data.Typeable
import Test.QuickCheck (frequency)

import Control.Parallel.Strategies

import GHC.Types hiding (Module)

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Mutable

import Homoiconic.Constrained

-------------------------------------------------------------------------------
-- Helper functions

-- | Creates a quickcheck property for a simple mutable operator defined using "immutable2mutable"
simpleMutableDefn :: (Eq a, IsMutable a)
    => (Mutable (ST s) a -> b -> ST s ()) -- ^ mutable function
    -> (a -> b -> a)              -- ^ create a mutable function using "immutable2mutable"
    -> (a -> b -> Logic a)        -- ^ the output property
simpleMutableDefn mf f a b = unsafeRunMutableProperty $ do
    ma1 <- thaw a
    ma2 <- thaw a
    mf ma1 b
    immutable2mutable f ma2 b
    a1 <- freeze ma1
    a2 <- freeze ma2
    return $ a1==a2

-------------------------------------------------------------------------------
-- comparison hierarchy

-- | This is a generalization of a "set".
-- We do not require a container to be a boolean algebra, just a semigroup.
class Eq a => Container a where
    {-# MINIMAL elem | notElem #-}
    elem :: Elem a -> a -> Logic a
    elem = not notElem

    notElem :: Elem a -> a -> Logic a
    notElem = not elem

law_Container_preservation :: Container s => s -> s -> Elem s -> Logic s
law_Container_preservation a1 a2 e = (a1==a2) ==> ((e `elem` a1) ==> (e `elem` a2))

type instance Elem Bool = ()
instance Container Bool where
    elem _ True  = True
    elem _ False = False

type instance Elem () = ()
instance Container () where
    elem () = \_ -> ()

instance Eq b => Container (a -> b)

--------------------

class
    ( Monoid (Elem a)
    , Container a
    , IfThenElse (Logic a)
    ) => IfThenElse a
        where
    ifThenElse :: a -> b -> b -> b
    ifThenElse a b1 b2 = ifThenElse (zero `elem` a) b1 b2

instance Semigroup Bool where (+) = (||)
instance Monoid Bool where zero = False
instance IfThenElse Bool where
    ifThenElse True  b _ = b
    ifThenElse False _ b = b

instance IfThenElse () where
    ifThenElse () b _ = b

----------------------------------------

-- | Every type has an associated logic.
-- Most types use classical logic, which corresponds to the Bool type.
-- But types can use any logical system they want.
-- Functions, for example, use an infinite logic.
-- You probably want your logic to be an instance of "Boolean", but this is not required.
--
-- See wikipedia's articles on <https://en.wikipedia.org/wiki/Algebraic_logic algebraic logic>,
-- and <https://en.wikipedia.org/wiki/Infinitary_logic infinitary logic> for more details.
type family Logic a :: *

type IdempLogic a = Logic (Logic (Logic a))~Logic (Logic a)

type ClassicalLogic a = Logic a ~Bool

type Classical (alg :: * -> Constraint) (a :: *) = (Logic a~Bool, alg a)

-- | Defines equivalence classes over the type.
-- The values need not have identical representations in the machine to be equal.
--
-- See <https://en.wikipedia.org/wiki/Equivalence_class wikipedia>
-- and <http://ncatlab.org/nlab/show/equivalence+class ncatlab> for more details.
class (IdempLogic a, Container (Logic a), Boolean (Logic a)) => Eq a where
    {-# MINIMAL (==) | (/=) #-}

    infix 4 ==
    (==) :: a -> a -> Logic a
    (==) = not (/=)

    infix 4 /=
    (/=) :: a -> a -> Logic a
    (/=) = not (==)

law_Eq_reflexive :: Eq a => a -> Logic a
law_Eq_reflexive a = a==a

law_Eq_symmetric :: Eq a => a -> a -> Logic (Logic a)
law_Eq_symmetric a1 a2 = (a1==a2)==(a2==a1)

law_Eq_transitive :: Eq a => a -> a -> a -> Logic a
law_Eq_transitive a1 a2 a3 = (a1==a2&&a2==a3) ==> (a1==a3)

defn_Eq_noteq :: (IdempLogic a, Eq a) => a -> a -> Logic (Logic a)
defn_Eq_noteq a1 a2 = (a1/=a2) == (not $ a1==a2)

#define mkEq(x) \
type instance Logic x = Bool; \
instance Eq x where (==) = (P.==); (/=) = (P./=)

mkEq(Bool)
mkEq(Char)
mkEq(Int)
mkEq(Integer)
mkEq(Rational)
mkEq(Float)
mkEq(Double)

type instance Logic () = ()
instance Eq () where
    () == () = ()
    () /= () = ()

-- type instance Logic (a -> b) = (Neighbor (a -> b) -> Bool)
type instance Logic (a -> b) = a -> Logic b
instance Eq b => Eq (a -> b) where
--     (==) f g (xs,nb) = go xs
--         where
--             go (x:xs) = (f x==g x) nb && go xs
--             go []     = True

--------------------

-- | This is more commonly known as a "meet" semilattice
class Eq b => POrd b where
    inf :: b -> b -> b

    {-# INLINE (<=) #-}
    infix 4 <=
    (<=) :: b -> b -> Logic b
    b1 <= b2 = inf b1 b2 == b1

    {-# INLINE (<) #-}
    (<) :: b -> b -> Logic b
    b1 < b2 = inf b1 b2 == b1 && b1 /= b2

law_POrd_commutative :: POrd b => b -> b -> Logic b
law_POrd_commutative b1 b2 = inf b1 b2 == inf b2 b1

law_POrd_associative :: POrd b => b -> b -> b -> Logic b
law_POrd_associative b1 b2 b3 = inf (inf b1 b2) b3 == inf b1 (inf b2 b3)

theorem_POrd_idempotent :: POrd b => b -> Logic b
theorem_POrd_idempotent b = inf b b == b

#define mkPOrd(x) \
instance POrd x where \
    inf = (P.min) ;\
    (<=) = (P.<=) ;\
    (<) = (P.<) ;\
    {-# INLINE inf #-} ;\
    {-# INLINE (<=) #-} ;\
    {-# INLINE (<) #-}

mkPOrd(Bool)
mkPOrd(Char)
mkPOrd(Int)
mkPOrd(Integer)
mkPOrd(Float)
mkPOrd(Double)
mkPOrd(Rational)

instance POrd () where
    {-# INLINE inf #-}
    inf () () = ()

instance POrd b => POrd (a -> b) where
    {-# INLINE inf #-}
    inf f g = \x -> inf (f x) (g x)

    {-# INLINE (<=) #-}
    (f<=g) a = f a <= g a

-------------------

-- | Most Lattice literature only considers 'Bounded' lattices, but here we have both upper and lower bounded lattices.
--
-- prop> minBound <= b || not (minBound > b)
--
class POrd b => MinBound b where
    minBound :: b

law_MinBound_inf :: MinBound b => b -> Logic b
law_MinBound_inf b = inf b minBound == minBound

-- | "false" is an upper bound because `a && false = false` for all a.
{-# INLINE false #-}
false :: MinBound b => b
false = minBound

instance MinBound ()       where minBound = ()         ; {-# INLINE minBound #-}
instance MinBound Bool     where minBound = False      ; {-# INLINE minBound #-}
instance MinBound Char     where minBound = P.minBound ; {-# INLINE minBound #-}
instance MinBound Int      where minBound = P.minBound ; {-# INLINE minBound #-}
instance MinBound Float    where minBound = -1/0       ; {-# INLINE minBound #-}
instance MinBound Double   where minBound = -1/0       ; {-# INLINE minBound #-}
-- FIXME: should be a primop for this

instance MinBound b => MinBound (a -> b) where minBound = \_ -> minBound ; {-# INLINE minBound #-}

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

instance Eq POrdering where
    {-# INLINE (==) #-}
    PLT == PLT = True
    PGT == PGT = True
    PEQ == PEQ = True
    PNA == PNA = True
    _ == _ = False

-- | FIXME: there are many semigroups over POrdering;
-- how should we represent the others? newtypes?
instance Semigroup POrdering where
    {-# INLINE (+) #-}
    PEQ + x = x
    PLT + _ = PLT
    PGT + _ = PGT
    PNA + _ = PNA

type instance Logic Ordering = Bool

instance Eq Ordering where
    {-# INLINE (==) #-}
    EQ == EQ = True
    LT == LT = True
    GT == GT = True
    _  == _  = False

instance Semigroup Ordering where
    {-# INLINE (+) #-}
    EQ + x = x
    LT + _ = LT
    GT + _ = GT

instance Monoid POrdering where
    {-# INLINE zero #-}
    zero = PEQ

instance Monoid Ordering where
    {-# INLINE zero #-}
    zero = EQ


-- |
--
--
-- See <https://en.wikipedia.org/wiki/Lattice%28order%29 wikipedia> for more details.
class POrd b => Lattice b where
    sup :: b -> b -> b

    {-# INLINE (>=) #-}
    infix 4 >=
    (>=) :: b -> b -> Logic b
    b1 >= b2 = sup b1 b2 == b1

    {-# INLINE (>) #-}
    (>) :: b -> b -> Logic b
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

law_Lattice_commutative :: Lattice b => b -> b -> Logic b
law_Lattice_commutative b1 b2 = sup b1 b2 == sup b2 b1

law_Lattice_associative :: Lattice b => b -> b -> b -> Logic b
law_Lattice_associative b1 b2 b3 = sup (sup b1 b2) b3 == sup b1 (sup b2 b3)

theorem_Lattice_idempotent :: Lattice b => b -> Logic b
theorem_Lattice_idempotent b = sup b b == b

law_Lattice_infabsorption :: Lattice b => b -> b -> Logic b
law_Lattice_infabsorption b1 b2 = inf b1 (sup b1 b2) == b1

law_Lattice_supabsorption :: Lattice b => b -> b -> Logic b
law_Lattice_supabsorption b1 b2 = sup b1 (inf b1 b2) == b1

law_Lattice_reflexivity :: Lattice a => a -> Logic a
law_Lattice_reflexivity a = a<=a

law_Lattice_antisymmetry :: (ClassicalLogic a, Lattice a) => a -> a -> Logic a
law_Lattice_antisymmetry a1 a2
    | a1 <= a2 && a2 <= a1 = a1 == a2
    | otherwise = true

law_Lattice_transitivity :: (ClassicalLogic a, Lattice a) => a -> a -> a -> Logic a
law_Lattice_transitivity  a1 a2 a3
    | a1 <= a2 && a2 <= a3 = a1 <= a3
    | a1 <= a3 && a3 <= a2 = a1 <= a2
    | a2 <= a1 && a1 <= a3 = a2 <= a3
    | a2 <= a3 && a3 <= a1 = a2 <= a1
    | a3 <= a2 && a2 <= a1 = a3 <= a1
    | a3 <= a1 && a1 <= a2 = a3 <= a2
    | otherwise = true

defn_Lattice_greaterthan :: (ClassicalLogic a, Lattice a) => a -> a -> Logic a
defn_Lattice_greaterthan a1 a2
    | a1 < a2 = a2 >= a1
    | a1 > a2 = a2 <= a1
    | otherwise = true

#define mkLattice(x)\
instance Lattice x where \
    sup = (P.max) ;\
    (>=) = (P.>=) ;\
    (>) = (P.>) ;\
    {-# INLINE sup #-} ;\
    {-# INLINE (>=) #-} ;\
    {-# INLINE (>) #-}

mkLattice(Bool)
mkLattice(Char)
mkLattice(Int)
mkLattice(Integer)
mkLattice(Float)
mkLattice(Double)
mkLattice(Rational)

instance Lattice () where
    {-# INLINE sup #-}
    sup () () = ()

instance Lattice b => Lattice (a -> b) where
    {-# INLINE sup #-}
    sup f g = \x -> sup (f x) (g x)

    {-# INLINE (>=) #-}
    (f>=g) a = f a >= g a

{-# INLINE (&&) #-}
infixr 3 &&
(&&) :: Lattice b => b -> b -> b
(&&) = inf

{-# INLINE (||) #-}
infixr 2 ||
(||) :: Lattice b => b -> b -> b
(||) = sup

-- | A chain is a collection of elements all of which can be compared
{-# INLINABLE isChain #-}
isChain :: (Lattice a, ClassicalLogic a) => [a] -> Logic a
isChain [] = true
isChain (x:xs) = all (/=PNA) (map (pcompare x) xs) && isChain xs

-- | An antichain is a collection of elements none of which can be compared
--
-- See <http://en.wikipedia.org/wiki/Antichain wikipedia> for more details.
--
-- See also the article on <http://en.wikipedia.org/wiki/Dilworth%27s_theorem Dilward's Theorem>.
{-# INLINABLE isAntichain #-}
isAntichain :: (Lattice a, ClassicalLogic a) => [a] -> Logic a
isAntichain [] = true
isAntichain (x:xs) = all (==PNA) (map (pcompare x) xs) && isAntichain xs

-------------------

-- | An element of a graded lattice has a unique predecessor.
--
-- See <https://en.wikipedia.org/wiki/Graded_poset wikipedia> for more details.
class Lattice b => Graded b where
    -- | Algebrists typically call this function the "rank" of the element in the poset;
    -- however we use the name from the standard prelude instead
    fromEnum :: b -> Int

    -- | The predecessor in the ordering
    pred :: b -> b

    -- | Repeatedly apply the "pred" function
    predN :: Int -> b -> b
    predN i b
        | i  < 0 = error $ "predN called on negative number "++show i
        | i == 0 = b
        | i  > 0 = predN (i-1) $ pred b

law_Graded_fromEnum :: (Lattice b, ClassicalLogic b, Graded b) => b -> b -> Bool
law_Graded_fromEnum b1 b2
    | b1 <  b2  = fromEnum b1 <  fromEnum b2
    | b1 >  b2  = fromEnum b1 >  fromEnum b2
    | b1 == b2  = fromEnum b1 == fromEnum b2
    | otherwise = True

law_Graded_pred :: Graded b => b -> b -> Bool
law_Graded_pred b1 _ = fromEnum (pred b1) == fromEnum b1-1
                     || fromEnum (pred b1) == fromEnum b1

defn_Graded_predN :: Graded b => Int -> b -> Logic b
defn_Graded_predN i b
    | i < 0 = true
    | otherwise = go i b == predN i b
    where
        go :: Graded b => Int -> b -> b
        go 0  b' = b'
        go i' b' = go (i'-1) $ pred b'

instance Graded Bool where
    {-# INLINE pred #-}
    pred True = False
    pred False = False

    {-# INLINE fromEnum #-}
    fromEnum True = 1
    fromEnum False = 0

instance Graded Int where
    {-# INLINE pred #-}
    pred i = if i == minBound
        then i
        else i-1

    {-# INLINE predN #-}
    predN n i = if i-n <= i
        then i-n
        else minBound

    {-# INLINE fromEnum #-}
    fromEnum = id

instance Graded Char where
    {-# INLINE pred #-}
    pred c = if c=='\NUL'
        then '\NUL'
        else P.pred c

    {-# INLINE fromEnum #-}
    fromEnum = P.fromEnum

instance Graded Integer where
    {-# INLINE pred #-}
    pred = P.pred

    {-# INLINE predN #-}
    predN n i = i - toInteger n

    {-# INLINE fromEnum #-}
    fromEnum = P.fromEnum

{-# INLINE (<.) #-}
(<.) :: Graded b => b -> b -> Logic b
b1 <. b2 = b1 == pred b2

-- | In a well founded ordering, every element (except possibly the "maxBound" if it exists) has a successor element.
-- We use the "Enum" to represent well founded orderings to maintain consistency with the standard Prelude.
--
-- See <http://ncatlab.org/nlab/show/well-founded+relation ncatlab> for more info.
class (Graded b, Ord b) => Enum b where
    -- | The next element in the ordering
    succ :: b -> b

    -- | Advance many elements into the ordering.
    -- This value may be negative to move backwards.
    succN :: Int -> b -> b
    succN i b = toEnum $ fromEnum b + i

    -- | Given an index (also called a rank) of an element, return the element
    toEnum :: Int -> b

law_Enum_toEnum :: Enum b => b -> Logic b
law_Enum_toEnum b = toEnum (fromEnum b) == b

law_Enum_succ :: Enum b => b -> Bool
law_Enum_succ b1 = fromEnum (succ b1) == fromEnum b1+1
                || fromEnum (succ b1) == fromEnum b1

defn_Enum_succN :: Enum b => Int -> b -> Logic b
defn_Enum_succN i b = succN i b == toEnum (fromEnum b + i)

instance Enum Bool where
    {-# INLINE succ #-}
    succ True = True
    succ False = True

    {-# INLINE toEnum #-}
    toEnum i
        | i > 0 = True
        | otherwise = False

instance Enum Int where
    {-# INLINE succ #-}
    succ i = if i == maxBound
        then i
        else i+1

    {-# INLINE toEnum #-}
    toEnum = id

instance Enum Char where
    {-# INLINE succ #-}
    succ = P.succ

    {-# INLINE toEnum #-}
    toEnum i = if i < 0
        then P.toEnum 0
        else P.toEnum i

instance Enum Integer where
    {-# INLINE succ #-}
    succ = P.succ

    {-# INLINE toEnum #-}
    toEnum = P.toEnum


{-# INLINE (>.) #-}
(>.) :: Enum b => b -> b -> Logic b
b1 >. b2 = b1 == succ b2

---------------------------------------

-- | This is the class of total orderings.
--
-- See https://en.wikipedia.org/wiki/Total_order
class Lattice a => Ord a where
    compare :: ClassicalLogic a => a -> a -> Ordering
    compare a1 a2 = case pcompare a1 a2 of
        PLT -> LT
        PGT -> GT
        PEQ -> EQ
        PNA -> error "PNA given by pcompare on a totally ordered type"

law_Ord_totality :: Ord a => a -> a -> Logic a
law_Ord_totality a1 a2 = a1 <= a2 || a2 <= a1

law_Ord_min :: Ord a => a -> a -> Logic a
law_Ord_min a1 a2 = min a1 a2 == a1
                 || min a1 a2 == a2

law_Ord_max :: Ord a => a -> a -> Logic a
law_Ord_max a1 a2 = max a1 a2 == a1
                 || max a1 a2 == a2

{-# INLINE min #-}
min :: Ord a => a -> a -> a
min = inf

{-# INLINE max #-}
max :: Ord a => a -> a -> a
max = sup

instance Ord ()
instance Ord Char      where compare = P.compare ; {-# INLINE compare #-}
instance Ord Int       where compare = P.compare ; {-# INLINE compare #-}
instance Ord Integer   where compare = P.compare ; {-# INLINE compare #-}
instance Ord Float     where compare = P.compare ; {-# INLINE compare #-}
instance Ord Double    where compare = P.compare ; {-# INLINE compare #-}
instance Ord Rational  where compare = P.compare ; {-# INLINE compare #-}
instance Ord Bool      where compare = P.compare ; {-# INLINE compare #-}

-------------------

-- | A Bounded lattice is a lattice with both a minimum and maximum element
--
class (Lattice b, MinBound b) => Bounded b where
    maxBound :: b

law_Bounded_sup :: Bounded b => b -> Logic b
law_Bounded_sup b = sup b maxBound == maxBound

-- | "true" is an lower bound because `a && true = true` for all a.
{-# INLINE true #-}
true :: Bounded b => b
true = maxBound

instance Bounded ()     where maxBound = ()         ; {-# INLINE maxBound #-}
instance Bounded Bool   where maxBound = True       ; {-# INLINE maxBound #-}
instance Bounded Char   where maxBound = P.maxBound ; {-# INLINE maxBound #-}
instance Bounded Int    where maxBound = P.maxBound ; {-# INLINE maxBound #-}
instance Bounded Float  where maxBound = 1/0        ; {-# INLINE maxBound #-}
instance Bounded Double where maxBound = 1/0        ; {-# INLINE maxBound #-}
-- FIXME: should be a primop for infinity

instance Bounded b => Bounded (a -> b) where
    {-# INLINE maxBound #-}
    maxBound = \_ -> maxBound

--------------------

class Bounded b => Complemented b where
    not :: b -> b

law_Complemented_not :: Complemented b => b -> Logic b
law_Complemented_not b = not (true  `asTypeOf` b) == false
                      && not (false `asTypeOf` b) == true

instance Complemented ()   where
    {-# INLINE not #-}
    not () = ()

instance Complemented Bool where
    {-# INLINE not #-}
    not = P.not

instance Complemented b => Complemented (a -> b) where
    {-# INLINE not #-}
    not f = \x -> not $ f x

-- | Heyting algebras are lattices that support implication, but not necessarily the law of excluded middle.
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
--
-- Note that while Heyting algebras are abelian semigroups with respect to &&, they are not in general cancellative.
class Bounded b => Heyting b where
    -- | FIXME: think carefully about infix
    infixl 3 ==>
    (==>) :: b -> b -> b

law_Heyting_maxbound :: Heyting b => b -> Logic b
law_Heyting_maxbound b = (b ==> b) == maxBound

law_Heyting_infleft :: Heyting b => b -> b -> Logic b
law_Heyting_infleft b1 b2 = (b1 && (b1 ==> b2)) == (b1 && b2)

law_Heyting_infright :: Heyting b => b -> b -> Logic b
law_Heyting_infright b1 b2 = (b2 && (b1 ==> b2)) == b2

law_Heyting_distributive :: Heyting b => b -> b -> b -> Logic b
law_Heyting_distributive b1 b2 b3 = (b1 ==> (b2 && b3)) == ((b1 ==> b2) && (b1 ==> b3))

-- | FIXME: add the axioms for intuitionist logic, which are theorems based on these laws
--

-- | Modus ponens gives us a default definition for "==>" in a "Boolean" algebra.
-- This formula is guaranteed to not work in a "Heyting" algebra that is not "Boolean".
--
-- See <https://en.wikipedia.org/wiki/Modus_ponens wikipedia> for more details.
modusPonens :: Boolean b => b -> b -> b
modusPonens b1 b2 = not b1 || b2

instance Heyting ()   where
    {-# INLINE (==>) #-}
    () ==> () = ()

instance Heyting Bool where
    {-# INLINE (==>) #-}
    (==>) = modusPonens

instance Heyting b => Heyting (a -> b) where
    {-# INLINE (==>) #-}
    (f==>g) a = f a ==> g a

-- | Generalizes Boolean variables.
--
-- See <https://en.wikipedia.org/wiki/Boolean_algebra_%28structure%29 wikipedia> for more details.
class (Complemented b, Heyting b) => Boolean b where

law_Boolean_infcomplement :: Boolean b => b -> Logic b
law_Boolean_infcomplement b = (b || not b) == true

law_Boolean_supcomplement :: Boolean b => b -> Logic b
law_Boolean_supcomplement b = (b && not b) == false

law_Boolean_infdistributivity :: Boolean b => b -> b -> b -> Logic b
law_Boolean_infdistributivity b1 b2 b3 = (b1 || (b2 && b3)) == ((b1 || b2) && (b1 || b3))

law_Boolean_supdistributivity :: Boolean b => b -> b -> b -> Logic b
law_Boolean_supdistributivity b1 b2 b3 = (b1 && (b2 || b3)) == ((b1 && b2) || (b1 && b3))

instance Boolean ()
instance Boolean Bool
instance Boolean b => Boolean (a -> b)

-------------------------------------------------------------------------------
-- numeric classes

class IsMutable g => Semigroup g where
    {-# MINIMAL (+) | (+=) #-}

    {-# INLINE (+) #-}
    infixl 6 +
    (+) :: g -> g -> g
    (+) = mutable2immutable (+=)

    {-# INLINE (+=) #-}
    infixr 5 +=
    (+=) :: PrimBase m => Mutable m g -> g -> m ()
    (+=) = immutable2mutable (+)

law_Semigroup_associativity :: (Eq g, Semigroup g) => g -> g -> g -> Logic g
law_Semigroup_associativity g1 g2 g3 = g1 + (g2 + g3) == (g1 + g2) + g3

defn_Semigroup_plusequal :: (Eq g, Semigroup g) => g -> g -> Logic g
defn_Semigroup_plusequal = simpleMutableDefn (+=) (+)

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle :: Semigroup m => m -> m
cycle xs = xs' where xs' = xs + xs'

instance Semigroup Int      where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Integer  where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Float    where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Double   where (+) = (P.+) ; {-# INLINE (+) #-}
instance Semigroup Rational where (+) = (P.+) ; {-# INLINE (+) #-}

instance Semigroup () where
    {-# INLINE (+) #-}
    ()+() = ()

instance Semigroup   b => Semigroup   (a -> b) where
    {-# INLINE (+) #-}
    f+g = \a -> f a + g a

---------------------------------------

-- | This type class is only used by the "Action" class.
-- It represents the semigroup that acts on our type.
type family Actor s

-- | Semigroup actions let us apply a semigroup to a set.
-- The theory of Modules is essentially the theory of Ring actions.
-- (See <http://mathoverflow.net/questions/100565/why-are-ring-actions-much-harder-to-find-than-group-actions mathoverflow>.)
-- That is why the two classes use similar notation.
--
-- See <https://en.wikipedia.org/wiki/Semigroup_action wikipedia> for more detail.
--
-- FIXME: These types could probably use a more expressive name.
--
-- FIXME: We would like every Semigroup to act on itself, but this results in a class cycle.
class (IsMutable s, Semigroup (Actor s)) => Action s where
    {-# MINIMAL (.+) | (.+=) #-}

    {-# INLINE (.+) #-}
    infixl 6 .+
    (.+) :: s -> Actor s -> s
    (.+) = mutable2immutable (.+=)

    {-# INLINE (.+=) #-}
    infixr 5 .+=
    (.+=) :: (PrimBase m) => Mutable m s -> Actor s -> m ()
    (.+=) = immutable2mutable (.+)

law_Action_compatibility :: (Eq s, Action s) => Actor s -> Actor s -> s -> Logic s
law_Action_compatibility a1 a2 s = (a1+a2) +. s == a1 +. a2 +. s

defn_Action_dotplusequal :: (Eq s, Action s) => s -> Actor s -> Logic s
defn_Action_dotplusequal = simpleMutableDefn (.+=) (.+)

-- | > s .+ a = a +. s
{-# INLINE (+.) #-}
infixr 6 +.
(+.) :: Action s => Actor s -> s -> s
a +. s = s .+ a

type instance Actor Int      = Int
type instance Actor Integer  = Integer
type instance Actor Float    = Float
type instance Actor Double   = Double
type instance Actor Rational = Rational
type instance Actor ()       = ()
type instance Actor (a->b)   = a->Actor b

instance Action Int      where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action Integer  where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action Float    where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action Double   where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action Rational where (.+) = (+) ; {-# INLINE (.+) #-}
instance Action ()       where (.+) = (+) ; {-# INLINE (.+) #-}

instance Action b => Action (a->b) where
    {-# INLINE (.+) #-}
    f.+g = \x -> f x.+g x

---------------------------------------

class Semigroup g => Monoid g where
    zero :: g

-- | FIXME:
-- Mive into Monoid class
isZero :: (Monoid g, Eq g) => g -> Logic g
isZero = (==zero)

notZero :: (Monoid g, Eq g) => g -> Logic g
notZero = (/=zero)

law_Monoid_leftid :: (Monoid g, Eq g) => g -> Logic g
law_Monoid_leftid g = zero + g == g

law_Monoid_rightid :: (Monoid g, Eq g) => g -> Logic g
law_Monoid_rightid g = g + zero == g

defn_Monoid_isZero :: (Monoid g, Eq g) => g -> Logic g
defn_Monoid_isZero g = (isZero $ zero `asTypeOf` g)
                    && (g /= zero ==> not isZero g)

---------

instance Monoid Int       where zero = 0 ; {-# INLINE zero #-}
instance Monoid Integer   where zero = 0 ; {-# INLINE zero #-}
instance Monoid Float     where zero = 0 ; {-# INLINE zero #-}
instance Monoid Double    where zero = 0 ; {-# INLINE zero #-}
instance Monoid Rational  where zero = 0 ; {-# INLINE zero #-}

instance Monoid () where
    {-# INLINE zero #-}
    zero = ()

instance Monoid b => Monoid (a -> b) where
    {-# INLINE zero #-}
    zero = \_ -> zero

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
    {-# MINIMAL (-) | (-=) #-}

    {-# INLINE (-) #-}
    infixl 6 -
    (-) :: g -> g -> g
    (-) = mutable2immutable (-=)

    {-# INLINE (-=) #-}
    infixr 5 -=
    (-=) :: (PrimBase m) => Mutable m g -> g -> m ()
    (-=) = immutable2mutable (-)


law_Cancellative_rightminus1 :: (Eq g, Cancellative g) => g -> g -> Logic g
law_Cancellative_rightminus1 g1 g2 = (g1 + g2) - g2 == g1

law_Cancellative_rightminus2 :: (Eq g, Cancellative g) => g -> g -> Logic g
law_Cancellative_rightminus2 g1 g2 = g1 + (g2 - g2) == g1

defn_Cancellative_plusequal :: (Eq g, Cancellative g) => g -> g -> Logic g
defn_Cancellative_plusequal = simpleMutableDefn (-=) (-)

instance Cancellative Int        where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative Integer    where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative Float      where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative Double     where (-) = (P.-) ; {-# INLINE (-) #-}
instance Cancellative Rational   where (-) = (P.-) ; {-# INLINE (-) #-}

instance Cancellative () where
    {-# INLINE (-) #-}
    ()-() = ()

instance Cancellative b => Cancellative (a -> b) where
    {-# INLINE (-) #-}
    f-g = \a -> f a - g a

---------------------------------------

class (Cancellative g, Monoid g) => Group g where
    {-# INLINE negate #-}
    negate :: g -> g
    negate g = zero - g

defn_Group_negateminus :: (Eq g, Group g) => g -> g -> Logic g
defn_Group_negateminus g1 g2 = g1 + negate g2 == g1 - g2

law_Group_leftinverse :: (Eq g, Group g) => g -> Logic g
law_Group_leftinverse g = negate g + g == zero

law_Group_rightinverse :: (Eq g, Group g) => g -> Logic g
law_Group_rightinverse g = g + negate g == zero

instance Group Int        where negate = P.negate ; {-# INLINE negate #-}
instance Group Integer    where negate = P.negate ; {-# INLINE negate #-}
instance Group Float      where negate = P.negate ; {-# INLINE negate #-}
instance Group Double     where negate = P.negate ; {-# INLINE negate #-}
instance Group Rational   where negate = P.negate ; {-# INLINE negate #-}

instance Group () where
    {-# INLINE negate #-}
    negate () = ()

instance Group b => Group (a -> b) where
    {-# INLINE negate #-}
    negate f = negate . f

---------------------------------------

class Semigroup m => Abelian m

law_Abelian_commutative :: (Abelian g, Eq g) => g -> g -> Logic g
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
    {-# MINIMAL (*) | (*=) #-}

    {-# INLINE (*) #-}
    infixl 7 *
    (*) :: r -> r -> r
    (*) = mutable2immutable (*=)

    {-# INLINE (*=) #-}
    infixr 5 *=
    (*=) :: (PrimBase m) => Mutable m r -> r -> m ()
    (*=) = immutable2mutable (*)

law_Rg_multiplicativeAssociativity :: (Eq r, Rg r) => r -> r -> r -> Logic r
law_Rg_multiplicativeAssociativity r1 r2 r3 = (r1 * r2) * r3 == r1 * (r2 * r3)

law_Rg_multiplicativeCommutivity :: (Eq r, Rg r) => r -> r -> Logic r
law_Rg_multiplicativeCommutivity r1 r2 = r1*r2 == r2*r1

law_Rg_annihilation :: (Eq r, Rg r) => r -> Logic r
law_Rg_annihilation r = r * zero == zero

law_Rg_distributivityLeft :: (Eq r, Rg r) => r -> r -> r -> Logic r
law_Rg_distributivityLeft r1 r2 r3 = r1*(r2+r3) == r1*r2+r1*r3

theorem_Rg_distributivityRight :: (Eq r, Rg r) => r -> r -> r -> Logic r
theorem_Rg_distributivityRight r1 r2 r3 = (r2+r3)*r1 == r2*r1+r3*r1

defn_Rg_timesequal :: (Eq g, Rg g) => g -> g -> Logic g
defn_Rg_timesequal = simpleMutableDefn (*=) (*)

instance Rg Int         where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg Integer     where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg Float       where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg Double      where (*) = (P.*) ; {-# INLINE (*) #-}
instance Rg Rational    where (*) = (P.*) ; {-# INLINE (*) #-}

instance Rg b => Rg (a -> b) where
    {-# INLINE (*) #-}
    f*g = \a -> f a * g a

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

-- | FIXME: this should be in the Rig class, but putting it there requires a lot of changes to Eq
isOne :: (Rig g, Eq g) => g -> Logic g
isOne = (==one)

-- | FIXME: this should be in the Rig class, but putting it there requires a lot of changes to Eq
notOne :: (Rig g, Eq g) => g -> Logic g
notOne = (/=one)

law_Rig_multiplicativeId :: (Eq r, Rig r) => r -> Logic r
law_Rig_multiplicativeId r = r * one == r && one * r == r

instance Rig Int         where one = 1 ; {-# INLINE one #-}
instance Rig Integer     where one = 1 ; {-# INLINE one #-}
instance Rig Float       where one = 1 ; {-# INLINE one #-}
instance Rig Double      where one = 1 ; {-# INLINE one #-}
instance Rig Rational    where one = 1 ; {-# INLINE one #-}

instance Rig b => Rig (a -> b) where
    {-# INLINE one #-}
    one = \_ -> one

---------------------------------------

-- | A "Ring" without identity.
type Rng r = (Rg r, Group r)

-- |
--
-- It is not part of the standard definition of rings that they have a "fromInteger" function.
-- It follows from the definition, however, that we can construct such a function.
-- The "slowFromInteger" function is this standard construction.
--
-- See <https://en.wikipedia.org/wiki/Ring_%28mathematics%29 wikipedia>
-- and <http://ncatlab.org/nlab/show/ring ncatlab>
-- for more details.
--
-- FIXME:
-- We can construct a "Module" from any ring by taking (*)=(.*.).
-- Thus, "Module" should be a superclass of "Ring".
-- Currently, however, this creates a class cycle, so we can't do it.
-- A number of type signatures are therefore more complicated than they need to be.
class (Rng r, Rig r) => Ring r where
    fromInteger :: Integer -> r
    fromInteger = slowFromInteger

defn_Ring_fromInteger :: (Eq r, Ring r) => r -> Integer -> Logic r
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

instance Ring Int         where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring Integer     where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring Float       where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring Double      where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}
instance Ring Rational    where fromInteger = P.fromInteger ; {-# INLINE fromInteger #-}

instance Ring b => Ring (a -> b) where
    {-# INLINE fromInteger #-}
    fromInteger i = \_ -> fromInteger i

{-# INLINABLE indicator #-}
indicator :: Ring r => Bool -> r
indicator True = 1
indicator False = 0

---------------------------------------

-- | 'Integral' numbers can be formed from a wide class of things that behave
-- like integers, but intuitively look nothing like integers.
--
-- FIXME: All Fields are integral domains; should we make it a subclass?  This would have the (minor?) problem of making the Integral class have to be an approximate embedding.
-- FIXME: Not all integral domains are homomorphic to the integers (e.g. a field)
--
-- See wikipedia on <https://en.wikipedia.org/wiki/Integral_element integral elements>,
--  <https://en.wikipedia.org/wiki/Integral_domain integral domains>,
-- and the <https://en.wikipedia.org/wiki/Ring_of_integers ring of integers>.
class Ring a => Integral a where
    toInteger :: a -> Integer

    infixl 7  `quot`, `rem`

    -- | truncates towards zero
    {-# INLINE quot #-}
    quot :: a -> a -> a
    quot a1 a2 = fst (quotRem a1 a2)

    {-# INLINE rem #-}
    rem :: a -> a -> a
    rem a1 a2 = snd (quotRem a1 a2)

    quotRem :: a -> a -> (a,a)


    infixl 7 `div`, `mod`

    -- | truncates towards negative infinity
    {-# INLINE div #-}
    div :: a -> a -> a
    div a1 a2 = fst (divMod a1 a2)

    {-# INLINE mod #-}
    mod :: a -> a -> a
    mod a1 a2 = snd (divMod a1 a2)

    divMod :: a -> a -> (a,a)


law_Integral_divMod :: (Eq a, Integral a, ClassicalLogic a) => a -> a -> Bool
law_Integral_divMod a1 a2 = if a2 /= 0
    then a2 * (a1 `div` a2) + (a1 `mod` a2) == a1
    else True

law_Integral_quotRem :: (Eq a, Integral a, ClassicalLogic a) => a -> a -> Bool
law_Integral_quotRem a1 a2 = if a2 /= 0
    then a2 * (a1 `quot` a2) + (a1 `rem` a2) == a1
    else True

law_Integral_toFromInverse :: (Eq a, Integral a) => a -> Logic a
law_Integral_toFromInverse a = fromInteger (toInteger a) == a

{-# INLINE[1] fromIntegral #-}
fromIntegral :: (Integral a, Ring b) => a -> b
fromIntegral = fromInteger . toInteger

-- | FIXME:
-- This should be moved into the class hierarchy and generalized.
--
-- FIXME:
-- There are more efficient implementations available if you restrict m to powers of 2.
-- Is GHC smart enough to convert `rem` into bit shifts?
-- See for more possibilities:
-- http://stackoverflow.com/questions/3407012/c-rounding-up-to-the-nearest-multiple-of-a-number
{-# INLINE roundUpToNearest #-}
roundUpToNearest :: Int -> Int -> Int
roundUpToNearest m x = x + m - 1 - (x-1)`rem`m
-- roundUpToNearest m x = if s==0
--     then
--     else x+r
--     where
--         s = x`rem`m
--         r = if s==0 then 0 else m-s

-- FIXME:
-- need more RULES; need tests
{-# RULES
"subhask/fromIntegral/Int->Int" fromIntegral = id :: Int -> Int
    #-}

instance Integral Int where
    {-# INLINE div #-}
    {-# INLINE mod #-}
    {-# INLINE divMod #-}
    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE quotRem #-}
    {-# INLINE toInteger #-}
    div = P.div
    mod = P.mod
    divMod = P.divMod
    quot = P.quot
    rem = P.rem
    quotRem = P.quotRem
    toInteger = P.toInteger

instance Integral Integer where
    {-# INLINE div #-}
    {-# INLINE mod #-}
    {-# INLINE divMod #-}
    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE quotRem #-}
    {-# INLINE toInteger #-}
    div = P.div
    mod = P.mod
    divMod = P.divMod
    quot = P.quot
    rem = P.rem
    quotRem = P.quotRem
    toInteger = P.toInteger

instance Integral b => Integral (a -> b) where
    {-# INLINE div #-}
    {-# INLINE mod #-}
    {-# INLINE divMod #-}
    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE quotRem #-}
    {-# INLINE toInteger #-}
    quot f1 f2 = \a -> quot (f1 a) (f2 a)
    rem f1 f2 = \a -> rem (f1 a) (f2 a)
    quotRem f1 f2 = (quot f1 f2, rem f1 f2)

    div f1 f2 = \a -> div (f1 a) (f2 a)
    mod f1 f2 = \a -> mod (f1 a) (f2 a)
    divMod f1 f2 = (div f1 f2, mod f1 f2)

    -- FIXME
    toInteger = error "toInteger shouldn't be in the integral class b/c of bad function instance"

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

--     infixr 5 /=
--     (/=) :: (PrimBase m) => Mutable m g -> g -> m ()
--     (/=) = immutable2mutable (/)

    {-# INLINE fromRational #-}
    fromRational :: Rational -> r
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

#define mkField(x) \
instance Field x where \
    (/) = (P./) ;\
    fromRational=P.fromRational ;\
    {-# INLINE fromRational #-} ;\
    {-# INLINE (/) #-}

mkField(Float)
mkField(Double)
mkField(Rational)

instance Field b => Field (a -> b) where
    {-# INLINE reciprocal #-}
    reciprocal f = reciprocal . f

----------------------------------------

-- | Ordered fields are generalizations of the rational numbers that maintain most of the nice properties.
-- In particular, all finite fields and the complex numbers are NOT ordered fields.
--
-- See <http://en.wikipedia.org/wiki/Ordered_field wikipedia> for more details.
class (Field r, Ord r, ValidScalar r) => OrdField r

instance OrdField Float
instance OrdField Double
instance OrdField Rational

---------------------------------------

-- | The prototypical example of a bounded field is the extended real numbers.
-- Other examples are the extended hyperreal numbers and the extended rationals.
-- Each of these fields has been extensively studied, but I don't know of any studies of this particular abstraction of these fields.
--
-- See <https://en.wikipedia.org/wiki/Extended_real_number_line wikipedia> for more details.
class (OrdField r, Bounded r) => BoundedField r where
    {-# INLINE nan #-}
    nan :: r
    nan = 0/0

    isNaN :: r -> Bool

{-# INLINE infinity #-}
infinity :: BoundedField r => r
infinity = maxBound

{-# INLINE negInfinity #-}
negInfinity :: BoundedField r => r
negInfinity = minBound

instance BoundedField Float  where isNaN = P.isNaN ; {-# INLINE isNaN #-}
instance BoundedField Double where isNaN = P.isNaN ; {-# INLINE isNaN #-}

----------------------------------------

-- | A Rational field is a field with only a single dimension.
--
-- FIXME: this isn't part of standard math; why is it here?
class Field r => RationalField r where
    toRational :: r -> Rational

instance RationalField Float    where  toRational=P.toRational ; {-# INLINE toRational #-}
instance RationalField Double   where  toRational=P.toRational ; {-# INLINE toRational #-}
instance RationalField Rational where  toRational=P.toRational ; {-# INLINE toRational #-}

{-# INLINE convertRationalField #-}
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
    (^^)     = (P.^^); \
    {-# INLINE truncate #-} ;\
    {-# INLINE round #-} ;\
    {-# INLINE ceiling #-} ;\
    {-# INLINE floor #-} ;\
    {-# INLINE (^^) #-} ;\

mkQuotientField(Float,Int)
mkQuotientField(Float,Integer)
mkQuotientField(Double,Int)
mkQuotientField(Double,Integer)
mkQuotientField(Rational,Int)
mkQuotientField(Rational,Integer)

-- mkQuotientField(Integer,Integer)
-- mkQuotientField(Int,Int)

instance QuotientField b1 b2 => QuotientField (a -> b1) (a -> b2) where
    truncate f = \a -> truncate $ f a
    round f = \a -> round $ f a
    ceiling f = \a -> ceiling $ f a
    floor f = \a -> floor $ f a
    (^^) f1 f2 = \a -> (^^) (f1 a) (f2 a)

---------------------------------------

-- | Rings augmented with the ability to take exponents.
--
-- Not all rings have this ability.
-- Consider the ring of rational numbers (represented by "Rational" in Haskell).
-- Raising any rational to an integral power results in another rational.
-- But raising to a fractional power results in an irrational number.
-- For example, the square root of 2.
--
-- See <http://en.wikipedia.org/wiki/Exponential_field#Exponential_rings wikipedia> for more detail.
--
-- FIXME:
-- This class hierarchy doesn't give a nice way exponentiate the integers.
-- We need to add instances for all the quotient groups.
class Ring r => ExpRing r where
    (**) :: r -> r -> r
    infixl 8 **

    logBase :: r -> r -> r

-- | An alternate form of "(**)" that some people find more convenient.
(^) :: ExpRing r => r -> r -> r
(^) = (**)

instance ExpRing Float where
    {-# INLINE (**) #-}
    (**) = (P.**)

    {-# INLINE logBase #-}
    logBase = P.logBase

instance ExpRing Double where
    {-# INLINE (**) #-}
    (**) = (P.**)

    {-# INLINE logBase #-}
    logBase = P.logBase

---------------------------------------

-- | Fields augmented with exponents and logarithms.
--
-- Technically, there are fields for which only a subset of the functions below are meaningful.
-- But these fields don't have any practical computational uses that I'm aware of.
-- So I've combined them all into a single class for simplicity.
--
-- See <http://en.wikipedia.org/wiki/Exponential_field wikipedia> for more detail.
class (ExpRing r, Field r) => ExpField r where
    sqrt :: r -> r
    sqrt r = r**(1/2)

    exp :: r -> r
    log :: r -> r

instance ExpField Float where
    sqrt = P.sqrt
    log = P.log
    exp = P.exp

instance ExpField Double where
    sqrt = P.sqrt
    log = P.log
    exp = P.exp

---------------------------------------

-- | This is a catch-all class for things the real numbers can do but don't exist in other classes.
--
-- FIXME:
-- Factor this out into a more appropriate class hierarchy.
-- For example, some (all?) trig functions need to move to a separate class in order to support trig in finite fields (see <https://en.wikipedia.org/wiki/Trigonometry_in_Galois_fields wikipedia>).
--
-- FIXME:
-- This class is misleading/incorrect for complex numbers.
--
-- FIXME:
-- There's a lot more functions that need adding.
class ExpField r => Real r where
    gamma :: r -> r
    lnGamma :: r -> r
    erf :: r -> r
    pi :: r
    sin :: r -> r
    cos :: r -> r
    tan :: r -> r
    asin :: r -> r
    acos :: r -> r
    atan :: r -> r
    sinh :: r -> r
    cosh :: r -> r
    tanh :: r -> r
    asinh :: r -> r
    acosh :: r -> r
    atanh :: r -> r

instance Real Float where
    gamma = P.gamma
    lnGamma = P.lnGamma
    erf = P.erf

    pi = P.pi

    sin = P.sin
    cos = P.cos
    tan = P.tan
    asin = P.asin
    acos = P.acos
    atan = P.atan
    sinh = P.sinh
    cosh = P.cosh
    tanh = P.tanh
    asinh = P.asinh
    acosh = P.acosh
    atanh = P.atanh

instance Real Double where
    gamma = P.gamma
    lnGamma = P.lnGamma
    erf = P.erf

    pi = P.pi

    sin = P.sin
    cos = P.cos
    tan = P.tan
    asin = P.asin
    acos = P.acos
    atan = P.atan
    sinh = P.sinh
    cosh = P.cosh
    tanh = P.tanh
    asinh = P.asinh
    acosh = P.acosh
    atanh = P.atanh

---------------------------------------

type family Scalar m

-- | A synonym that covers everything we intuitively thing scalar variables should have.
type ValidScalar r = (Ring r, Ord r, Scalar r~r, Normed r)

-- | A (sometimes) more convenient version of "ValidScalar".
type HasScalar a = ValidScalar (Scalar a)

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
    ( Ord (Scalar g)
    , Scalar (Scalar g) ~ Scalar g
    , Ring (Scalar g)
    ) => Normed g where
    size :: g -> Scalar g

    sizeSquared :: g -> Scalar g
    sizeSquared g = s*s
        where
            s = size g

abs :: ValidScalar g => g -> g
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
-- See <http://en.wikipedia.org/wiki/Cone_%28linear_algebra%29 wikipedia> for more details.
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

class
    ( Abelian v
    , Group v
    , Ring (Scalar v)
    , Scalar (Scalar v) ~ Scalar v
    ) => Module v
        where

    {-# MINIMAL (.*) | (.*=) #-}

    -- | Scalar multiplication.
    {-# INLINE (.*) #-}
    infixl 7 .*
    (.*) :: v -> Scalar v -> v
    (.*) = mutable2immutable (.*=)

    {-# INLINE (.*=) #-}
    infixr 5 .*=
    (.*=) :: (PrimBase m) => Mutable m v -> Scalar v -> m ()
    (.*=) = immutable2mutable (.*)

law_Module_multiplication :: (Eq m, Module m) => m -> m -> Scalar m -> Logic m
law_Module_multiplication m1 m2 s = s *. (m1 + m2) == s*.m1 + s*.m2

law_Module_addition :: (Eq m, Module m) => m -> Scalar m -> Scalar m -> Logic m
law_Module_addition  m s1 s2 = (s1+s2)*.m == s1*.m + s2*.m

law_Module_action :: (Eq m, Module m) => m -> Scalar m -> Scalar m -> Logic m
law_Module_action m s1 s2 = s1*.(s2*.m) == (s1*s2)*.m

law_Module_unital :: (Eq m, Module m) => m -> Logic m
law_Module_unital m = 1 *. m == m

defn_Module_dotstarequal :: (Eq m, Module m) => m -> Scalar m -> Logic m
defn_Module_dotstarequal = simpleMutableDefn (.*=) (.*)


{-# INLINE (*.) #-}
infixl 7 *.
(*.) :: Module v => Scalar v -> v -> v
r *. v  = v .* r

instance Module Int       where (.*) = (*)
instance Module Integer   where (.*) = (*)
instance Module Float     where (.*) = (*)
instance Module Double    where (.*) = (*)
instance Module Rational  where (.*) = (*)

instance Module b => Module (a -> b) where
    f .*  b = \a -> f a .*  b

---------------------------------------

-- | Free modules have a basis.
-- This means it makes sense to perform operations elementwise on the basis coefficients.
--
-- See <https://en.wikipedia.org/wiki/Free_module wikipedia> for more detail.
class
    ( Module v
--     , IxContainer v
--     , Scalar v~Elem v
    ) => FreeModule v where

    {-# MINIMAL ((.*.) | (.*.=)) #-}

    -- | Multiplication of the components pointwise.
    -- For matrices, this is commonly called Hadamard multiplication.
    --
    -- See <http://en.wikipedia.org/wiki/Hadamard_product_%28matrices%29 wikipedia> for more detail.
    {-# INLINE (.*.) #-}
    infixl 7 .*.
    (.*.) :: v -> v -> v
    (.*.) = mutable2immutable (.*.=)

    {-# INLINE (.*.=) #-}
    infixr 5 .*.=
    (.*.=) :: (PrimBase m) => Mutable m v -> v -> m ()
    (.*.=) = immutable2mutable (.*.)

law_FreeModule_commutative :: (Eq m, FreeModule m) => m -> m -> Logic m
law_FreeModule_commutative m1 m2 = m1.*.m2 == m2.*.m1

law_FreeModule_associative :: (Eq m, FreeModule m) => m -> m -> m -> Logic m
law_FreeModule_associative m1 m2 m3 = m1.*.(m2.*.m3) == (m1.*.m2).*.m3

defn_FreeModule_dotstardotequal :: (Eq m, FreeModule m) => m -> m -> Logic m
defn_FreeModule_dotstardotequal = simpleMutableDefn (.*.=) (.*.)

instance FreeModule Int       where (.*.) = (*)
instance FreeModule Integer   where (.*.) = (*)
instance FreeModule Float     where (.*.) = (*)
instance FreeModule Double    where (.*.) = (*)
instance FreeModule Rational  where (.*.) = (*)

instance FreeModule b => FreeModule (a -> b) where
    g .*. f = \a -> g a .*. f a

---------------------------------------

class FreeModule v => FreeModule1 v where

    -- | The identity for Hadamard multiplication.
    -- Intuitively, this object has the value "one" in every column.
    ones :: v
--     ones = sum $ map snd $ toIxList (basis (msg::v))
--         where
--             msg = error "default implementation of ones requires a statically known basis"

law_FreeModule_id :: (Eq m, FreeModule1 m) => m -> Logic m
law_FreeModule_id m = m == m.*.ones

instance FreeModule1 Int       where ones = one
instance FreeModule1 Integer   where ones = one
instance FreeModule1 Float     where ones = one
instance FreeModule1 Double    where ones = one
instance FreeModule1 Rational  where ones = one

instance (FreeModule b, ValidScalar b) => FreeModule1 (a -> b) where
    ones = \_ -> one

---------------------------------------

-- | If our "FreeModule" has a finite basis, then we can:
--
-- * provide a dense construction method that's a bit more convenient than "fromIxList".
class FreeModule v => FiniteModule v where
    -- | Returns the dimension of the object.
    -- For some objects, this may be known statically, and so the parameter will not be "seq"ed.
    -- But for others, this may not be known statically, and so the parameter will be "seq"ed.
    dim :: v -> (Ring a, Ord a) => a

    unsafeToModule :: [Scalar v] -> v

type instance Elem Int      = Int
type instance Elem Integer  = Integer
type instance Elem Float    = Float
type instance Elem Double   = Double
type instance Elem Rational = Rational

type instance Index Int      = Int
type instance Index Integer  = Int
type instance Index Float    = Int
type instance Index Double   = Int
type instance Index Rational = Int

type instance SetIndex Int      a = Int
type instance SetIndex Integer  a = Int
type instance SetIndex Float    a = Int
type instance SetIndex Double   a = Int
type instance SetIndex Rational a = Int

instance FiniteModule Int       where dim _ = 1; unsafeToModule [x] = x
instance FiniteModule Integer   where dim _ = 1; unsafeToModule [x] = x
instance FiniteModule Float     where dim _ = 1; unsafeToModule [x] = x
instance FiniteModule Double    where dim _ = 1; unsafeToModule [x] = x
instance FiniteModule Rational  where dim _ = 1; unsafeToModule [x] = x

---------------------------------------

class (FreeModule v, Field (Scalar v)) => Vector v where

    {-# MINIMAL (./.) | (./.=) #-}

    {-# INLINE (./) #-}
    infixl 7 ./
    (./) :: v -> Scalar v -> v
    v ./ r = v .* reciprocal r

    {-# INLINE (./.) #-}
    infixl 7 ./.
    (./.) :: v -> v -> v
    (./.) = mutable2immutable (./.=)

    {-# INLINE (./=) #-}
    infixr 5 ./=
    (./=) :: (PrimBase m) => Mutable m v -> Scalar v -> m ()
    (./=) = immutable2mutable (./)

    {-# INLINE (./.=) #-}
    infixr 5 ./.=
    (./.=) :: (PrimBase m) => Mutable m v -> v -> m ()
    (./.=) = immutable2mutable (./.)


instance Vector Float     where (./) = (/); (./.) = (/)
instance Vector Double    where (./) = (/); (./.) = (/)
instance Vector Rational  where (./) = (/); (./.) = (/)

-- instance Vector b => Vector (a -> b) where g ./. f = \a -> g a ./. f a

---------------------------------------

-- | A Reisz space is a vector space obeying nice partial ordering laws.
--
-- See <http://en.wikipedia.org/wiki/Riesz_space wikipedia> for more details.
class (Vector v, Lattice v) => Reisz v where
    --
    -- | An element of a Reisz space can always be split into positive and negative components.
    reiszSplit :: v -> (v,v)

---------------------------------------

-- | A Banach space is a Vector Space equipped with a compatible Norm and Metric.
--
-- See <http://en.wikipedia.org/wiki/Banach_space wikipedia> for more details.
class (Vector v, Normed v, Metric v) => Banach v where
    {-# INLINE normalize #-}
    normalize :: v -> v
    normalize v = v ./ size v

-- | These laws correspond to the actual math class, but they don't
-- actually hold for 'Float' and 'Double'.
--
-- FIXME:
-- Find a way to actually test these laws and add them to
-- "SubHask.TemplateHaskell.Test".
law_Banach_distance :: Banach v => v -> v -> Logic (Scalar v)
law_Banach_distance v1 v2 = size (v1 - v2) == distance v1 v2

law_Banach_size :: (Banach v, Logic v~Logic (Scalar v)) => v -> Logic (Scalar v)
law_Banach_size v
    = isZero v
   || size (normalize v) == 1

instance Banach Float
instance Banach Double
instance Banach Rational

---------------------------------------

type HasValidSquare a =
    ( Vector (Square a)
    , IxContainer (Square a)
    , IxContainer a
    , Transposable (Square a)
--     , IxConstructible (Square a)
--     , IxConstructible a
    , a ~ Elem (Square a)
    , Index a ~ Index (Square a)
--     , Square (Elem a) ~ Elem (Elem (Square a))
    )

-- FIXME:
-- Should these instances exist?
-- If so, put them in a better spot.
instance IxConstructible Float
instance IxConstructible Double
instance IxConstructible Rational

-- | Transpose
--
-- FIXME:
-- All Square types are dagger categories,
-- and the transpose is the dagger operation.
-- The hierarchy should reflect this.
class {-Transposable (Scalar a) =>-} Transposable a where
    trans :: a -> a

    -- | Given a vector, return the basis.
    -- Depending on the type, every vector may use the same basis or different bases.
    -- In the former case, the basis is statically known for every vector and so the vector need not be evaluated.
    -- In the latter case, the vector will be evaluated.
    basis :: Elem a -> a

--     default basis :: (Basis v~[v], FiniteModule v) => v -> Basis v
--     basis v = map go [0..dim v-1]
--         where
--             go i = unsafeToModule $ P.replicate (i-1) 0 ++ [1] ++ P.replicate (dim v-2-i) 0

instance Transposable Float     where trans = id; basis = 1
instance Transposable Double    where trans = id; basis = 1
instance Transposable Rational  where trans = id; basis = 1
instance Transposable Int       where trans = id; basis = 1
instance Transposable Integer   where trans = id; basis = 1

-- | Hilbert spaces generalize Euclidean space by allowing infinite dimensions.
--
-- See <http://en.wikipedia.org/wiki/Hilbert_space wikipedia> for more details.
class (Banach v, HasValidSquare v) => Hilbert v where

    -- | The type of the tensor product of a vector with itself.
    -- That is, the "square" of a type with respect to the tensor product.
    --
    -- FIXME:
    -- Tensors are actually much more general.
    -- For example, they can be the product of two vectors of different types.
    -- To capture this generality requires using the monoidal structure of the Vect category.
    --
    -- FIXME:
    -- All of the tensor related operations here are valid for general Modules.
    -- They should be moved in the hierarchy accordingly.
    type Square v

    -- | The outer product
    (><) :: v -> v -> Square v

    -- | "left multiplication" of a square matrix
    vXm :: v -> Square v -> v

    -- | "right multiplication" of a square matrix
    mXv :: Square v -> v -> v

    -- | The inner product
    infix 8 <>
    (<>) :: v -> v -> Scalar v

instance Hilbert Float where
    type Square Float = Float
    (><) = (*)
    vXm  = (*)
    mXv  = (*)
    (<>) = (*)

instance Hilbert Double where
    type Square Double = Double
    (><) = (*)
    vXm  = (*)
    mXv  = (*)
    (<>) = (*)

instance Hilbert Rational where
    type Square Rational = Rational
    (><) = (*)
    vXm  = (*)
    mXv  = (*)
    (<>) = (*)

{-# INLINE squaredInnerProductNorm #-}
squaredInnerProductNorm :: Hilbert v => v -> Scalar v
squaredInnerProductNorm v = v<>v

{-# INLINE innerProductNorm #-}
innerProductNorm :: Hilbert v => v -> Scalar v
innerProductNorm = undefined -- sqrt . squaredInnerProductNorm

{-# INLINE innerProductDistance #-}
innerProductDistance :: Hilbert v => v -> v -> Scalar v
innerProductDistance _ _ = undefined --innerProductNorm $ v1-v2

---------------------------------------


---------------------------------------

-- | Metric spaces give us the most intuitive notion of distance between objects.
--
-- FIXME: There are many other notions of distance and we should make a whole hierarchy.
class
    ( HasScalar v
    , Eq v
    , Boolean (Logic v)
    ) => Metric v
        where

    distance :: v -> v -> Scalar v

    -- | If the distance between two datapoints is less than or equal to the upper bound,
    -- then this function will return the distance.
    -- Otherwise, it will return some number greater than the upper bound.
    {-# INLINE distanceUB #-}
    distanceUB :: v -> v -> Scalar v -> Scalar v
    distanceUB v1 v2 _ = distance v1 v2

-- | Calling this function will be faster on some 'Metric's than manually checking if distance is greater than the bound.
{-# INLINE isFartherThan #-}
isFartherThan :: Metric v => v -> v -> Scalar v -> Logic (Scalar v)
isFartherThan s1 s2 b = distanceUB s1 s2 b > b

-- | This function constructs an efficient default implementation for 'distanceUB' given a function that lower bounds the distance metric.
{-# INLINE lb2distanceUB #-}
lb2distanceUB ::
    ( Metric a
    , IfThenElse (Logic (Scalar a))
    ) => (a -> a -> Scalar a)
      -> (a -> a -> Scalar a -> Scalar a)
lb2distanceUB lb p q b = if lbpq > b
    then lbpq
    else distance p q
    where
        lbpq = lb p q
law_Metric_nonnegativity :: Metric v => v -> v -> Logic (Scalar v)
law_Metric_nonnegativity v1 v2 = distance v1 v2 >= 0

law_Metric_indiscernables :: (Metric v, IfThenElse (Logic v)) => v -> v -> Logic (Scalar v)
law_Metric_indiscernables v1 v2 = if v1 == v2
    then distance v1 v2 == 0
    else distance v1 v2 > 0

law_Metric_symmetry :: Metric v => v -> v -> Logic (Scalar v)
law_Metric_symmetry v1 v2 = distance v1 v2 == distance v2 v1

law_Metric_triangle :: Metric v => v -> v -> v -> Logic (Scalar v)
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
type family Elem s

-- | Two sets are disjoint if their infimum is the empty set.
-- This function generalizes the notion of disjointness for any lower bounded lattice.
-- FIXME: add other notions of disjoint
infDisjoint :: (Constructible s, MinBound s, Monoid s) => s -> s -> Logic s
infDisjoint s1 s2 = isEmpty $ inf s1 s2

sizeDisjoint :: (Normed s, Constructible s) => s -> s -> Logic (Scalar s)
sizeDisjoint s1 s2 = size s1 + size s2 == size (s1+s2)

type Constructible0 x = (Monoid x, Constructible x)

-- | This is the class for any type that gets "constructed" from smaller types.
-- It is a massive generalization of the notion of a constructible set in topology.
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

defn_Constructible_fromList :: (Eq s, Constructible s) => s -> Elem s -> [Elem s] -> Logic s
defn_Constructible_fromList s e es = fromList1 e es `asTypeOf` s == foldl' snoc (singleton e) es

defn_Constructible_fromListN :: (Eq s, Constructible s) => s -> Elem s -> [Elem s] -> Logic s
defn_Constructible_fromListN s e es = (fromList1 e es `asTypeOf` s)==fromList1N (size es+1) e es

defn_Constructible_cons :: (Eq s, Constructible s) => s -> Elem s -> Logic s
defn_Constructible_cons s e = cons e s == singleton e + s

defn_Constructible_snoc :: (Eq s, Constructible s) => s -> Elem s -> Logic s
defn_Constructible_snoc s e = snoc s e == s + singleton e

law_Constructible_singleton :: (Constructible s, Container s) => s -> Elem s -> Logic s
law_Constructible_singleton s e = elem e $ singleton e `asTypeOf` s

theorem_Constructible_cons :: (Constructible s, Container s) => s -> Elem s -> Logic s
theorem_Constructible_cons s e = elem e (cons e s)

-- | A more suggestive name for inserting an element into a container that does not remember location
insert :: (Abelian s, Constructible s) => Elem s -> s -> s
insert = cons

-- | A slightly more suggestive name for a container's monoid identity
empty :: (Monoid s, Constructible s) => s
empty = zero

-- | A slightly more suggestive name for checking if a container is empty
isEmpty :: (Eq s, Monoid s, Constructible s) => s -> Logic s
isEmpty = isZero

-- | This function needed for the OverloadedStrings language extension
fromString :: (Monoid s, Constructible s, Elem s ~ Char) => String -> s
fromString = fromList

-- | FIXME: if -XOverloadedLists is enabled, this causes an infinite loop for some reason
{-# INLINABLE fromList #-}
fromList :: (Monoid s, Constructible s) => [Elem s] -> s
fromList [] = zero
fromList (x:xs) = fromList1 x xs

{-# INLINABLE fromListN #-}
fromListN :: (Monoid s, Constructible s) => Int -> [Elem s] -> s
fromListN 0 [] = zero
fromListN i (x:xs) = fromList1N i x xs

{-# INLINABLE generate #-}
generate :: (Monoid v, Constructible v) => Int -> (Int -> Elem v) -> v
generate n f = if n <= 0
    then zero
    else fromList1N n (f 0) (map f [1..n-1])

-- | The dual of a monoid, obtained by swapping the arguments of 'mappend'.
newtype DualSG a = DualSG { getDualSG :: a }
        deriving (Read,Show)

instance Semigroup a => Semigroup (DualSG a) where
    (DualSG x)+(DualSG y) = DualSG (x+y)

instance Monoid a => Monoid (DualSG a) where
    zero = DualSG zero

-- | The monoid of endomorphisms under composition.
newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup (Endo a) where
    (Endo f)+(Endo g) = Endo (f.g)

instance Monoid (Endo a) where
    zero = Endo id

-- | Provides inverse operations for "Constructible".
--
-- FIXME:
-- should this class be broken up into smaller pieces?
class (Constructible s, Monoid s, Normed s, Scalar s~Int) => Foldable s where

    {-# MINIMAL foldMap | foldr #-}

    -- | Convert the container into a list.
    toList :: s -> [Elem s]
    toList s = foldr (:) [] s

    -- | Remove an element from the left of the container.
    uncons :: s -> Maybe (Elem s,s)
    uncons s = case toList s of
        []     -> Nothing
        (x:xs) -> Just (x,fromList xs)

    -- | Remove an element from the right of the container.
    unsnoc :: s -> Maybe (s,Elem s)
    unsnoc s = case unsnoc (toList s) of
        Nothing -> Nothing
        Just (xs,x) -> Just (fromList xs,x)

    -- | Add all the elements of the container together.
    {-# INLINABLE sum #-}
    sum :: Monoid (Elem s) => s -> Elem s
    sum xs = foldl' (+) zero $ toList xs

    -- the definitions below are copied from Data.Foldable
    foldMap :: Monoid a => (Elem s -> a) -> s -> a
    foldMap f = foldr ((+) . f) zero

    foldr :: (Elem s -> a -> a) -> a -> s -> a
    foldr f z t = appEndo (foldMap (Endo . f) t) z

    foldr' :: (Elem s -> a -> a) -> a -> s -> a
    foldr' f z0 xs = foldl f' id xs z0
        where f' k x z = k $! f x z

    foldl   :: (a -> Elem s -> a) -> a -> s -> a
    foldl f z t = appEndo (getDualSG (foldMap (DualSG . Endo . flip f) t)) z

    foldl'  :: (a -> Elem s -> a) -> a -> s -> a
    foldl' f z0 xs = foldr f' id xs z0
         where f' x k z = k $! f z x

    -- the following definitions are simpler (IMO) than those in Data.Foldable

    foldr1  :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldr1  f s = foldr1  f (toList s)

    foldr1' :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldr1' f s = foldr1' f (toList s)

    foldl1  :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldl1  f s = foldl1  f (toList s)

    foldl1' :: (Elem s -> Elem s -> Elem s) -> s -> Elem s
    foldl1' f s = foldl1' f (toList s)

defn_Foldable_foldr ::
    ( Eq a
    , a~Elem s
    , Logic (Scalar s) ~ Logic (Elem s)
    , Foldable s
    ) => (Elem s -> Elem s -> Elem s) -> Elem s -> s -> Logic (Elem s)
defn_Foldable_foldr f a s = foldr f a s == foldr f a (toList s)

defn_Foldable_foldr' ::
    ( Eq a
    , a~Elem s
    , Logic (Scalar s) ~ Logic (Elem s)
    , Foldable s
    ) => (Elem s -> Elem s -> Elem s) -> Elem s -> s -> Logic (Elem s)
defn_Foldable_foldr' f a s = foldr' f a s == foldr' f a (toList s)

defn_Foldable_foldl ::
    ( Eq a
    , a~Elem s
    , Logic (Scalar s) ~ Logic (Elem s)
    , Foldable s
    ) => (Elem s -> Elem s -> Elem s) -> Elem s -> s -> Logic (Elem s)
defn_Foldable_foldl f a s = foldl f a s == foldl f a (toList s)

defn_Foldable_foldl' ::
    ( Eq a
    , a~Elem s
    , Logic (Scalar s) ~ Logic (Elem s)
    , Foldable s
    ) => (Elem s -> Elem s -> Elem s) -> Elem s -> s -> Logic (Elem s)
defn_Foldable_foldl' f a s = foldl' f a s == foldl' f a (toList s)

defn_Foldable_foldr1 ::
    ( Eq (Elem s)
    , Foldable s
    , ClassicalLogic (Elem s)
    ) => (Elem s -> Elem s -> Elem s) -> s -> Logic (Elem s)
defn_Foldable_foldr1 f s = (length s > 0) ==> (foldr1 f s == foldr1 f (toList s))

defn_Foldable_foldr1' ::
    ( Eq (Elem s)
    , Foldable s
    , ClassicalLogic (Elem s)
    ) => (Elem s -> Elem s -> Elem s) -> s -> Logic (Elem s)
defn_Foldable_foldr1' f s = (length s > 0) ==> (foldr1' f s == foldr1' f (toList s))

defn_Foldable_foldl1 ::
    ( Eq (Elem s)
    , Foldable s
    , ClassicalLogic (Elem s)
    ) => (Elem s -> Elem s -> Elem s) -> s -> Logic (Elem s)
defn_Foldable_foldl1 f s = (length s > 0) ==> (foldl1 f s == foldl1 f (toList s))

defn_Foldable_foldl1' ::
    ( Eq (Elem s)
    , Foldable s
    , ClassicalLogic (Elem s)
    ) => (Elem s -> Elem s -> Elem s) -> s -> Logic (Elem s)
defn_Foldable_foldl1' f s = (length s > 0) ==> (foldl1' f s == foldl1' f (toList s))

-- |
--
-- Note:
-- The inverse \"theorem\" of @(toList . fromList) xs == xs@ is actually not true.
-- See the "Set" type for a counter example.
theorem_Foldable_tofrom :: (Eq s, Foldable s) => s -> Logic s
theorem_Foldable_tofrom s = fromList (toList s) == s

-- |
-- FIXME:
-- This law can't be automatically included in the current test system because it breaks parametricity by requiring @Monoid (Elem s)@
law_Foldable_sum ::
    ( Logic (Scalar s)~Logic s
    , Logic (Elem s)~Logic s
    , Heyting (Logic s)
    , Monoid (Elem s)
    , Eq (Elem s)
    , Foldable s
    ) => s -> s -> Logic s
law_Foldable_sum s1 s2 = sizeDisjoint s1 s2 ==> (sum (s1+s2) == sum s1 + sum s2)

-- | This fold is not in any of the standard libraries.
foldtree1 :: Monoid a => [a] -> a
foldtree1 as = case go as of
    []  -> zero
    [a] -> a
    as'  -> foldtree1 as'
    where
        go []  = []
        go [a] = [a]
        go (a1:a2:as'') = (a1+a2):go as''

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
argmin :: (Ord b, ClassicalLogic b) => a -> a -> (a -> b) -> a
argmin a1 a2 f = if f a1 < f a2 then a1 else a2

{-# INLINE argmax #-}
argmax :: (Ord b, ClassicalLogic b) => a -> a -> (a -> b) -> a
argmax a1 a2 f = if f a1 > f a2 then a1 else a2

{-# INLINE maximum #-}
maximum :: (Bounded b) => [b] -> b
maximum = supremum

{-# INLINE maximum_ #-}
maximum_ :: (Ord b) => b -> [b] -> b
maximum_ = supremum_

{-# INLINE minimum #-}
minimum :: (Bounded b) => [b] -> b
minimum = infimum

{-# INLINE minimum_ #-}
minimum_ :: (Ord b) => b -> [b] -> b
minimum_ = infimum_

{-# INLINE supremum #-}
supremum :: (Foldable bs, Elem bs~b, Bounded b) => bs -> b
supremum = supremum_ minBound

{-# INLINE supremum_ #-}
supremum_ :: (Foldable bs, Elem bs~b, Lattice b) => b -> bs -> b
supremum_ = foldl' sup

{-# INLINE infimum #-}
infimum :: (Foldable bs, Elem bs~b, Bounded b) => bs -> b
infimum = infimum_ maxBound

{-# INLINE infimum_ #-}
infimum_ :: (Foldable bs, Elem bs~b, POrd b) => b -> bs -> b
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
type family SetIndex s a

-- | An indexed constructible container associates an 'Index' with each 'Elem'.
-- This class generalizes the map abstract data type.
--
-- There are two differences in the indexed hierarchy of containers from the standard hierarchy.
--   1. 'IxConstructible' requires a 'Monoid' constraint whereas 'Constructible' requires a 'Semigroup' constraint because there are no valid 'IxConstructible's (that I know of at least) that are not also 'Monoid's.
--   2. Many regular containers are indexed containers, but not the other way around.
--      So the class hierarchy is in a different order.
--
class Eq s => IxContainer s where
    lookup :: Index s -> s -> Maybe (Elem s)

    {-# INLINABLE (!) #-}
    (!) :: s -> Index s -> Elem s
    (!) s i = case lookup i s of
        Just x -> x
        Nothing -> error "used (!) on an invalid index"

    {-# INLINABLE findWithDefault #-}
    findWithDefault :: Elem s -> Index s -> s -> Elem s
    findWithDefault def i s = case s !? i of
        Nothing -> def
        Just e -> e

    {-# INLINABLE hasIndex #-}
    hasIndex :: Bounded (Logic s) => s -> Index s -> Logic s
    hasIndex s i = case s !? i of
        Nothing -> false
        Just _ -> true

    -- | FIXME: should the functions below be moved to other classes?
    type ValidElem s (e :: *) :: Constraint
    type ValidElem s e = ()

    imap :: (Index s -> Elem s -> Elem s) -> s -> s

    -- | Updates value at specific index. O(n), but can be everwritten with more efficient versions
    infixr 4 !~
    {-# INLINEABLE (!~) #-}
    (!~) :: (Eq (Index s), Logic (Index s)~Bool, ValidElem s (Elem s)) => Index s -> Elem s -> s -> s
    (!~) i e c = imap (\ix el -> if ix == i then e else el) c

    -- | Similar to "!~" the operation "%~" applies a given function to the value
    infixr 4 %~
    {-# INLINEABLE (%~) #-}
    (%~) :: (Eq (Index s), Logic (Index s)~Bool, ValidElem s (Elem s)) => Index s -> (Elem s -> Elem s) -> s -> s
    (%~) i f c = imap (\ix el -> if ix == i then f el else el) c

    toIxList :: s -> [(Index s, Elem s)]

    indices :: s -> [Index s]
    indices = map fst . toIxList

    values :: s -> [Elem s]
    values = map snd . toIxList

law_IxContainer_preservation ::
    ( Logic (Elem s)~Logic s
    , Eq (Elem s)
    , IxContainer s
    , Monoid s
    ) => s -> s -> Index s -> Logic s
law_IxContainer_preservation s1 s2 i = case s1 !? i of
    Just e -> (s1+s2) !? i == Just e
    Nothing -> true

defn_IxContainer_bang ::
    ( Eq (Elem s)
    , IxContainer s
    , Monoid s
    ) => s -> Index s -> Logic (Elem s)
defn_IxContainer_bang s i = case s !? i of
    Nothing -> true
    Just e -> s!i == e

defn_IxContainer_findWithDefault ::
    ( Eq (Elem s)
    , IxContainer s
    , Monoid s
    ) => s -> Index s -> Elem s -> Logic (Elem s)
defn_IxContainer_findWithDefault s i e = case s !? i of
    Nothing -> findWithDefault e i s == e
    Just e' -> findWithDefault e i s == e'

defn_IxContainer_hasIndex ::
    ( Complemented (Logic s)
    , IxContainer s
    , Monoid s
    ) => s -> Index s -> Logic s
defn_IxContainer_hasIndex s i = case s !? i of
    Nothing -> not $ hasIndex s i
    Just _  -> hasIndex s i

-- FIXME:
-- It would be interesting to make the "Index" of scalars be ().
-- Is it worth it?
#define mkIxContainer(t) \
type instance Index t = Int; \
type instance Elem t = t; \
instance IxContainer t where \
    lookup 0 x = Just x; \
    lookup _ _ = Nothing

mkIxContainer(Int)
mkIxContainer(Integer)
mkIxContainer(Float)
mkIxContainer(Double)
mkIxContainer(Rational)

-- FIXME
type instance Index (a -> b) = a
type instance Elem (a -> b) = b
instance Eq b => IxContainer (a -> b) where
    lookup a f = Just $ f a

-- | Sliceable containers generalize the notion of a substring to any IxContainer.
-- Sliceables place a stronger guarantee on the way the Semigroup and IxContainer class interact to make the containers more array-like.
--
-- FIXME:
-- Slice should probably be redefined in terms of "drop" and "take" (which also need to be included into the class hierarchy properly).
class (IxContainer s, Scalar s~Index s, HasScalar s, Normed s) => Sliceable s where
    -- | Extract a subcontainer
    slice
        :: Index s -- ^ starting index
        -> Index s -- ^ number of elements
        -> s       -- ^ container
        -> s

law_Sliceable_restorable ::
    ( Sliceable s
    , Eq s
    , Logic (Index s) ~ Logic s
    , IfThenElse (Logic s)
    , Monoid s
    ) => s -> Index s -> Logic s
law_Sliceable_restorable s i = if i >= 0 && i < length s
    then slice 0 i s + slice i (length s-i) s == s
    else true

law_Sliceable_preservation ::
    ( Eq (Elem s)
    , Sliceable s
    , Logic s ~ Logic (Elem s)
    , Monoid s
    ) => s -> s -> Index s -> Logic s
law_Sliceable_preservation s1 s2 i = case s1 !? i of
    Just e -> (s1+s2) !? i == Just e
    Nothing -> case s2 !? (i - length s1) of
        Just e  -> (s1+s2) !? i == Just e
        Nothing -> true

-- | This is the class for Map-like containers.
-- Every element in these containers must contain both an index and an element.
--
-- Some containers that use indices are not typically constructed with those indices (e.g. Arrays).
-- They belong to the "Sliceable" and "Constructable" classes, but not to this class.
class (Monoid s, IxContainer s) => IxConstructible s where
    {-# MINIMAL singletonAt | consAt #-}

    -- | Construct a container with only the single (index,element) pair.
    -- This function is equivalent to 'singleton' in the 'Constructible' class.
    singletonAt :: Index s -> Elem s -> s
    singletonAt i e = consAt i e zero

    -- | Insert an element, overwriting the previous value if the index already exists.
    -- This function is equivalent to 'cons' in the 'Constructible' class.
    {-# INLINABLE consAt #-}
    consAt :: Index s -> Elem s -> s -> s
    consAt i e s = singletonAt i e + s

    -- | Insert an element only if the index does not already exist.
    -- If the index already exists, the container is unmodified.
    -- This function is equivalent to 'snoc' in the 'Constructible' class.
    {-# INLINABLE snocAt #-}
    snocAt :: s -> Index s -> Elem s -> s
    snocAt s i e = s + singletonAt i e

    -- | This function is the equivalent of 'fromList' in the 'Constructible' class.
    -- We do not require all the variants of 'fromList' because of our 'Monoid' constraint.
    {-# INLINABLE fromIxList #-}
    fromIxList :: [(Index s, Elem s)] -> s
    fromIxList xs = foldl' (\s (i,e) -> snocAt s i e) zero xs

law_IxConstructible_lookup ::
    ( Eq (Elem s)
    , IxConstructible s
    ) => s -> Index s -> Elem s -> Logic (Elem s)
law_IxConstructible_lookup s i e = case lookup i (consAt i e s) of
    Just e' -> e'==e
    Nothing -> false

defn_IxConstructible_consAt :: (Eq s, IxConstructible s) => s -> Index s -> Elem s -> Logic s
defn_IxConstructible_consAt s i e = consAt i e s == singletonAt i e + s

defn_IxConstructible_snocAt :: (Eq s, IxConstructible s) => s -> Index s -> Elem s -> Logic s
defn_IxConstructible_snocAt s i e = snocAt s i e == s + singletonAt i e

defn_IxConstructible_fromIxList :: (Eq s, IxConstructible s) => s -> [(Index s, Elem s)] -> Logic s
defn_IxConstructible_fromIxList t es
    = fromIxList es `asTypeOf` t == foldl' (\s (i,e) -> snocAt s i e) zero es

-- | Follows from "law_IxConstructible_lookup" but is closely related to "law_IxContainer_preservation" and "law_Sliceable_preservation"
theorem_IxConstructible_preservation ::
    ( Eq (Elem s)
    , IxConstructible s
    , Logic s ~ Logic (Elem s)
    ) => s -> s -> Index s -> Logic s
theorem_IxConstructible_preservation s1 s2 i = case s1 !? i of
    Just e -> (s1+s2) !? i == Just e
    Nothing -> case s2 !? i of
        Just e  -> (s1+s2) !? i == Just e
        Nothing -> true

insertAt :: IxConstructible s => Index s -> Elem s -> s -> s
insertAt = consAt

-- | An infix operator equivalent to 'lookup'
-- FIXME: what should the precedence be?
{-# INLINABLE (!?) #-}
(!?) :: IxContainer s => s -> Index s -> Maybe (Elem s)
(!?) s i = lookup i s

--------------------------------------------------------------------------------

type instance Scalar [a] = Int
type instance Logic [a] = Logic a
type instance Elem [a] = a
type instance Index [a] = Int

instance Eq a => Eq [a] where
    (x:xs)==(y:ys) = x==y && xs==ys
    (_:_)==[]     = false
    []    ==(_:_) = false
    []    ==[]     = true

instance (Eq a, ClassicalLogic a) => POrd [a] where
    inf [] _  = []
    inf _  [] = []
    inf (x:xs) (y:ys) = if x==y
        then x:inf xs ys
        else []

instance (Eq a, ClassicalLogic a) => MinBound [a] where
    minBound = []

instance Normed [a] where
    size = P.length

instance Semigroup [a] where
    (+) = (P.++)

instance Monoid [a] where
    zero = []

instance Eq a => Container [a] where
    elem _ []       = false
    elem x (y:ys)   = x==y || elem x ys

    notElem = not elem

instance Constructible [a] where
    singleton a = [a]
    cons x xs = x:xs
    fromList1 x xs = x:xs
    fromList1N _ x xs = x:xs

instance Foldable [a] where
    toList = id

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

instance Eq a => IxContainer [a] where
    lookup 0 (x:_ ) = Just x
    lookup i (_:xs) = lookup (i-1) xs
    lookup _ []     = Nothing

    imap f xs = map (uncurry f) $ P.zip [0..] xs

    toIxList xs = P.zip [0..] xs

----------------------------------------

type instance Scalar (Maybe a) = Scalar a
type instance Logic (Maybe a) = Logic a

instance Eq a => Eq (Maybe a) where
    Nothing   == Nothing   = true
    Nothing   == _         = false
    _         == Nothing   = false
    (Just a1) == (Just a2) = a1==a2

instance Semigroup a => Semigroup (Maybe a) where
    (Just a1) + (Just a2) = Just $ a1+a2
    Nothing   + a2        = a2
    a1        + Nothing   = a1

instance Semigroup a => Monoid (Maybe a) where
    zero = Nothing

----------

data Maybe' a = Nothing' | Just' { fromJust' :: !a }

justs' :: [Maybe' a] -> [a]
justs' [] = []
justs' (Nothing':xs) = justs' xs
justs' (Just' x:xs) = x:justs' xs

type instance Scalar (Maybe' a) = Scalar a
type instance Logic (Maybe' a) = Logic a

instance NFData a => NFData (Maybe' a) where
    rnf Nothing' = ()
    rnf (Just' a) = rnf a

instance Eq a => Eq (Maybe' a) where
    (Just' a1) == (Just' a2) = a1==a2
    Nothing'   == Nothing'   = true
    _          == _          = false

instance Semigroup a => Semigroup (Maybe' a) where
    (Just' a1) + (Just' a2) = Just' $ a1+a2
    Nothing'   + a2         = a2
    a1         + Nothing'   = a1

instance Semigroup a => Monoid (Maybe' a) where
    zero = Nothing'

----------------------------------------

type instance Logic (a,b) = (Logic a, Logic b)

instance (Eq a, Eq b) => Eq (a,b) where
    (==) (a1,b1) (a2,b2) = (a1==a2,b1==b2)

type instance Elem (a,b) = (Elem a, Elem b)
type instance Index (a,b) = (Index a, Index b)
instance (Container a, Container b) => Container (a,b) where
    elem (ea,eb) (a,b) = (elem ea a, elem eb b)

instance (POrd a, POrd b) => POrd (a,b) where
    inf (a1,b1) (a2,b2) = (inf a1 a2, inf b1 b2)

instance (MinBound a, MinBound b) => MinBound (a,b) where
    minBound = (minBound,minBound)

instance (Lattice a, Lattice b) => Lattice (a,b) where
    sup (a1,b1) (a2,b2) = (sup a1 a2, sup b1 b2)

instance (Bounded a, Bounded b) => Bounded (a,b) where
    maxBound = (maxBound,maxBound)

instance (Complemented a, Complemented b) => Complemented (a,b) where
    not (a,b) = (not a, not b)

instance (Heyting a, Heyting b) => Heyting (a,b) where
    (==>) (a1,b1) (a2,b2) = (a1==>a2, b1==>b2)

instance (Boolean a, Boolean b) => Boolean (a,b)

-- type instance Logic (a,b) = Logic a
-- type instance Logic (a,b,c) = Logic a
--
-- instance (Eq a, Eq b, Logic a ~ Logic b) => Eq (a,b) where
--     (a1,b1)==(a2,b2) = a1==a2 && b1==b2
--
-- instance (Eq a, Eq b, Eq c, Logic a ~ Logic b, Logic b~Logic c) => Eq (a,b,c) where
--     (a1,b1,c1)==(a2,b2,c2) = a1==a2 && b1==b2 && c1==c2
--
-- instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
--     (a1,b1)+(a2,b2) = (a1+a2,b1+b2)
--
-- instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a,b,c) where
--     (a1,b1,c1)+(a2,b2,c2) = (a1+a2,b1+b2,c1+c2)
--
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--     zero = (zero,zero)
--
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
--     zero = (zero,zero,zero)
--
-- instance (Cancellative a, Cancellative b) => Cancellative (a,b) where
--     (a1,b1)-(a2,b2) = (a1-a2,b1-b2)
--
-- instance (Cancellative a, Cancellative b, Cancellative c) => Cancellative (a,b,c) where
--     (a1,b1,c1)-(a2,b2,c2) = (a1-a2,b1-b2,c1-c2)
--
-- instance (Group a, Group b) => Group (a,b) where
--     negate (a,b) = (negate a,negate b)
--
-- instance (Group a, Group b, Group c) => Group (a,b,c) where
--     negate (a,b,c) = (negate a,negate b,negate c)
--
-- instance (Abelian a, Abelian b) => Abelian (a,b)
--
-- instance (Abelian a, Abelian b, Abelian c) => Abelian (a,b,c)

----------------------------------------

data Labeled' x y = Labeled' { xLabeled' :: !x, yLabeled' :: !y }
    deriving (Read,Show,Typeable)

instance (NFData x, NFData y) => NFData (Labeled' x y) where
    rnf (Labeled' x y) = deepseq x $ rnf y

instance (Arbitrary x, Arbitrary y) => Arbitrary (Labeled' x y) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Labeled' x y

instance (CoArbitrary x, CoArbitrary y) => CoArbitrary (Labeled' x y) where
    coarbitrary (Labeled' x  y) = coarbitrary (x,y)

type instance Scalar (Labeled' x y) = Scalar x
type instance Actor (Labeled' x y) = x
type instance Logic (Labeled' x y) = Logic x
type instance Elem (Labeled' x y) = Elem x

-----

instance Eq x => Eq (Labeled' x y) where
    (Labeled' x1 _) == (Labeled' x2 _) = x1==x2

instance (ClassicalLogic x, Ord x) => POrd (Labeled' x y) where
    inf (Labeled' x1 y1) (Labeled' x2 y2) = if x1 < x2
        then Labeled' x1 y1
        else Labeled' x2 y2
    (Labeled' x1 _)< (Labeled' x2 _) = x1< x2
    (Labeled' x1 _)<=(Labeled' x2 _) = x1<=x2

instance (ClassicalLogic x, Ord x) => Lattice (Labeled' x y) where
    sup (Labeled' x1 y1) (Labeled' x2 y2) = if x1 >= x2
        then Labeled' x1 y1
        else Labeled' x2 y2
    (Labeled' x1 _)> (Labeled' x2 _) = x1> x2
    (Labeled' x1 _)>=(Labeled' x2 _) = x1>=x2

instance (ClassicalLogic x, Ord x) => Ord (Labeled' x y) where

instance Semigroup x => Action (Labeled' x y) where
    (Labeled' x y) .+ x' = Labeled' (x'+x) y

instance Metric x => Metric (Labeled' x y) where
    distance (Labeled' x1 _) (Labeled' x2 _) = distance x1 x2
    distanceUB (Labeled' x1 _) (Labeled' x2 _) = distanceUB x1 x2

instance Normed x => Normed (Labeled' x y) where
    size (Labeled' x _) = size x

--------------------------------------------------------------------------------

mkMutable [t| POrdering |]
mkMutable [t| Ordering |]
mkMutable [t| forall a. Endo a |]
mkMutable [t| forall a. DualSG a |]
mkMutable [t| forall a. Maybe a |]
mkMutable [t| forall a. Maybe' a |]
mkMutable [t| forall a b. Labeled' a b |]

{-
instance FAlgebra IfThenElse
instance FAlgebra IsMutable
instance IsMutable (Free (Sig alg) t a)
instance Show (Sig IsMutable t a)
mkTagFromCnst ''Logic [t| forall a. IdempLogic a |]
mkTag ''Elem
mkFAlgebra ''Eq
mkFAlgebra ''POrd
mkFAlgebra ''MinBound
mkFAlgebra ''Lattice
mkFAlgebra ''Boolean
mkTagFromCnst ''Scalar [t| forall a. Scalar (Scalar a) ~ Scalar a |]
mkFAlgebra ''RationalField
mkFAlgebra ''Vector
mkFAlgebra ''Normed
mkFAlgebra ''Hilbert

type instance FreeConstraints t a
    = ( AppTags (ConsTag TScalar t) a
      ~ Scalar (AppTags t a)
--       , AppTags (ConsTag TScalar (ConsTag TLogic (ConsTag TLogic t))) a
--       ~ Scalar (AppTags (ConsTag_TLogic (ConsTag_TLogic t)) a)
      )

--------------------------------------------------------------------------------

data Law alg = Law
    { lawName :: String
    , lawLHS  :: AST alg '[] Var
    , lawRHS  :: AST alg '[] Var
    }

class FAlgebra alg => Variety alg where
    laws :: [Law alg]

instance Variety Semigroup where
    laws = [ Law "associative" ((var1+var2)+var3) (var1+(var2+var3)) ]
    -}
