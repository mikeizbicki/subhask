{-# LANGUAGE NoAutoDeriveTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | SubHask supports two ways to encode categories in Haskell.
--
-- **Method 1**
--
-- Create a data type of kind @k -> k -> *@,
-- and define an instance of the "Category" class.
-- Because our version of "Category" uses the ConstraintKinds extension,
-- we can encode many more categories than the standard "Data.Category" class.
--
-- There are many subclasses of "Category" for categories with extra features.
-- Most of this module is spent defining these categories
-- and instantiating appropriate instances for "Hask".
--
-- Unfortunately, many of the terms used in category theory are non-standard.
-- In this module, we try to follow the names used out in John Baez and Mike Stay's
-- <http://math.ucr.edu/home/baez/rosetta.pdf Rosetta Stone paper>.
-- This is a fairly accessible introduction to category theory for Haskeller's ready
-- to move beyond \"monads are monoids in the category of endofunctors.\"
--
-- FIXME:
-- Writing laws for any classes in this file requires at least the "Eq" class from "SubHask.Algebra".
-- Hence, the laws are not explicitly stated anywhere.
--
--
-- **Method 2**
--
-- For any subcategory of "Hask", we can define a type "ProofOf subcat".
-- Then any function of type @ProofOf subcat a -> ProofOf subcat b@ is an arrow within @subcat@.
-- This is essentially a generalization of automatic differentiation to any category.
--
-- TODO:
-- This needs a much better explanation and examples.
--
-- **Comparison**
-- Method 1 is the primary way to represent a category.
-- It's main advantage is that we have complete control over the representation in memory.
-- With method 2, everything must be wrapped within function calls.
-- Besides this layer of indirection, we also increase the chance for accidental space leaks.
--
-- Usually, it is easier to work with functions using method 1,
-- but it is easier to construct functions using method 2.
--
-- FIXME:
-- Currently, each category comes with its own mechanism for converting between the two representations.
-- We need something more generic.
module SubHask.Category
    (
    Category (..)
    , (<<<)
    , (>>>)
    , Cat

    -- * Hask
    , Hask
    , ($)
    , (&)
    , ($!)
    , embedHask
    , embedHask2
    , withCategory
    , embed2
    , fst
    , snd

    -- * Special types of categories
    , Concrete
    , Monoidal (..)
    -- FIXME: conflict with SubHask.Algebra
    -- , (><)
    , Braided (..)
    , Symmetric
    , Cartesian (..)
    , const
    , const2
    , Closed (..)

    , Groupoid (..)
    , Compact (..)
    , Dagger (..)

    -- * Proofs
    , Provable(..)
    , ProofOf_
    , ProofOf
    ) where

import SubHask.Internal.Prelude
import SubHask.SubType
import qualified Prelude as P

-------------------------------------------------------------------------------

-- | This 'Category' class modifies the one in the Haskell standard to include the 'ValidCategory' type constraint.
-- This constraint let's us make instances of arbitrary subcategories of Hask.
--
-- Subcategories are defined using the subtyping mechanism "(<:)".
-- Intuitively, arrows and objects in a subcategory satisfy additional properties that elements of the larger category do not necessarily satisfy.
-- Elements of a subcategory can always be embeded in the larger category.
-- Going in the other direction, however, requires a proof.
-- These proofs can (usually) not be verified by the type system and are therefore labeled unsafe.
--
-- More details available at <http://en.wikipedia.org/wiki/Subcategory wikipedia>
-- and <http://ncatlab.org/nlab/show/subcategory ncatlab>.

class Category (cat :: k -> k -> *) where

    type ValidCategory cat (a::k) :: Constraint
    id :: ValidCategory cat a => cat a a

    infixr 9 .
    (.) :: cat b c -> cat a b -> cat a c

-- | An alternative form of function composition taken from "Control.Arrow"
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
a >>> b = b.a

-- | An alternative form of function composition taken from "Control.Arrow"
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
a <<< b = a.b

-- | The category with Haskell types as objects, and functions as arrows.
--
-- More details available at the <http://www.haskell.org/haskellwiki/Hask Haskell wiki>.
type Hask = (->)

instance Category (->) where
    type ValidCategory (->) (a :: *) = ()
    id = P.id

    {-# INLINE (.) #-}
    (.) = (P..)

-- | The category with categories as objects and functors as arrows.
--
-- More details available at <https://en.wikipedia.org/wiki/Category_of_categories wikipedia>
-- and <http://ncatlab.org/nlab/show/Cat ncatlab>.
--
-- ---
--
-- TODO: can this be extended to functor categories?
-- http://ncatlab.org/nlab/show/functor+category

type Cat cat1 cat2 = forall a b. CatT (->) a b cat1 cat2

data CatT
    ( cat :: * -> * -> *)
    ( a :: k )
    ( b :: k )
    ( cat1 :: k -> k -> * )
    ( cat2 :: k -> k -> * )
    = CatT (cat1 a b `cat` cat2 a b)

instance Category cat => Category (CatT cat a b) where
    type ValidCategory (CatT cat a b) cat1 =
        ( ValidCategory cat1 a
        , ValidCategory cat1 b
        , ValidCategory cat (cat1 a b)
        )

    id = CatT id
    (CatT f).(CatT g) = CatT $ f.g

-- NOTE: We would rather have the definition of CatT not depend on the a and b
-- variables, as in the code below.  Unfortunately, GHC 7.8's type checker isn't
-- strong enough to handle forall inside of a type class.
--
-- data CatT
--     ( cat :: * -> * -> *)
--     ( cat1 :: * -> * -> * )
--     ( cat2 :: * -> * -> * )
--     = forall a b.
--         ( ValidCategory cat1 a
--         , ValidCategory cat2 a
--         , ValidCategory cat1 b
--         , ValidCategory cat2 b
--         ) => CatT (cat1 a b `cat` cat2 a b)
--
-- instance Category cat => Category (CatT cat) where
--     type ValidCategory (CatT cat) cat1 = forall a b.
--         ( ValidCategory cat1 a
--         , ValidCategory cat1 b
--         , ValidCategory cat (cat1 a b)
--         )
--
--     id = CatT id
--     (CatT f).(CatT g) = CatT $ f.g

---------------------------------------

-- | Technicaly, a concrete category is any category equiped with a faithful
-- functor to the category of sets.  This is just a little too platonic to
-- be represented in Haskell, but 'Hask' makes a pretty good approximation.
-- So we call any 'SubCategory' of 'Hask' 'Concrete'.  Importantly, not
-- all categories are concrete.   See the 'SubHask.Category.Slice.Slice'
-- category for an example.
--
-- More details available at <http://en.wikipedia.org/wiki/Concrete_category wikipedia>
-- and <http://ncatlab.org/nlab/show/concrete+category ncatlib>.
type Concrete cat = cat <: (->)

-- | We generalize the Prelude's definition of "$" so that it applies to any
-- subcategory of 'Hask' (that is, any 'Concrete' 'Category'.  This lets us
-- easily use these subcategories as functions. For example, given a polynomial
-- function
--
-- > f :: Polynomial Double
--
-- we can evaluate the polynomial at the number 5 by
--
-- > f $ 5
--
-- NOTE:
-- Base's implementation of '$' has special compiler support that let's it work with the RankNTypes extension.
-- This version does not.
-- See <http://stackoverflow.com/questions/8343239/type-error-with-rank-2-types-and-function-composition this stackoverflow question> for more detail.

infixr 0 $
($) :: Concrete subcat => subcat a b -> a -> b
($) = embedType2

-- | Like in lens "&" is just "flip ($)" for reverse application.
--
-- This allows us to take advantage of function-composition when working on a single object, i.e. given
--
-- > vector :: Vector 5 Int
--
-- we can update the 3rd and 4th entry by
--
-- > vector & 3 !~ 23 . 4 !~ 42
--
-- without traversing the whole structure as (!~) may have a more performant implementation then "updating by traversing"

infixr 1 &
(&) :: Concrete subcat => a -> subcat a b -> b
(&) = flip ($)

-- | A strict version of '$'
infixr 0 $!
($!) :: Concrete subcat => subcat a b -> a -> b
f $! x  = let !vx = x in f $ vx

-- | Embeds a unary function into 'Hask'
embedHask :: Concrete subcat => subcat a b -> a -> b
embedHask = embedType2

-- | Embeds a binary function into 'Hask'
embedHask2 ::  Concrete subcat => subcat a (subcat b c)  -> a -> b -> c
embedHask2 f = \a b -> (f $ a) $ b

-- | This is a special form of the 'embed' function which can make specifying the
-- category we are embedding into easier.
withCategory :: Concrete subcat => proxy subcat -> subcat a b -> a -> b
withCategory _ f = embedType2 f

-- | FIXME: This would be a useful function to have, but I'm not sure how to implement it yet!
embed2 :: (subcat <: cat) => subcat a (subcat a b) -> cat a (cat a b)
embed2 _ = undefined

-------------------------------------------------------------------------------

-- | The intuition behind a monoidal category is similar to the intuition
-- behind the 'SubHask.Algebra.Monoid' algebraic structure.  Unfortunately,
-- there are a number of rather awkward laws to work out the technical details.
-- The associator and unitor functions are provided to demonstrate the
-- required isomorphisms.
--
-- More details available at <http://en.wikipedia.org/wiki/Monoidal_category wikipedia>
class
    ( Category cat
    , ValidCategory cat (TUnit cat)
    ) => Monoidal (cat :: * -> * -> *)
        where

    type Tensor cat :: * -> * -> *
    tensor ::
        ( ValidCategory cat a
        , ValidCategory cat b
        ) => cat a (cat b (Tensor cat a b))

    type TUnit cat :: *
    tunit :: proxy cat -> TUnit cat

instance Monoidal (->) where
    type Tensor (->) = (,)
    tensor = \a b -> (a,b)

    type TUnit (->) = (() :: *)
    tunit _ = ()

-- | This is a convenient and (hopefully) suggestive shortcut for constructing
-- tensor products in 'Concrete' categories.
infixl 7 ><
(><) :: forall cat a b.
    ( Monoidal cat
    , Concrete cat
    , ValidCategory cat a
    , ValidCategory cat b
    ) => a -> b -> Proxy cat -> Tensor cat a b
(><) a b _ = embedHask2 (tensor::cat a (cat b (Tensor cat a b))) a b

-- | Braided categories let us switch the order of tensored objects.
--
-- More details available at <https://en.wikipedia.org/wiki/Braided_monoidal_category wikipedia>
-- and <http://ncatlab.org/nlab/show/braided+monoidal+category ncatlab>
class Monoidal cat => Braided cat where
    braid   :: cat (Tensor cat a b) (Tensor cat b a)
    unbraid :: cat (Tensor cat a b) (Tensor cat b a)

instance Braided (->) where
    braid (a,b) = (b,a)
    unbraid = braid

-- | In a symmetric braided category, 'braid' and 'unbraid' are inverses of each other.
--
-- More details available at <http://en.wikipedia.org/wiki/Symmetric_monoidal_category wikipedia>
class Braided cat => Symmetric cat
instance Symmetric (->)

-- | In a cartesian monoidal category, the monoid object acts in a particularly nice way where we can compose and decompose it.
-- Intuitively, we can delete information using the 'fst' and 'snd' arrows, as well as
-- duplicate information using the 'duplicate' arrow.
--
-- More details available at <http://ncatlab.org/nlab/show/cartesian+monoidal+category ncatlab>
class Symmetric cat => Cartesian cat where
    fst_ ::
        ( ValidCategory cat a
        , ValidCategory cat b
        , ValidCategory cat (Tensor cat a b)
        ) => cat (Tensor cat a b) a

    snd_ ::
        ( ValidCategory cat a
        , ValidCategory cat b
        , ValidCategory cat (Tensor cat a b)
        ) => cat (Tensor cat a b) b

    terminal ::
        ( ValidCategory cat a
        ) => a -> cat a (TUnit cat)

    initial ::
        ( ValidCategory cat a
        ) => a -> cat (TUnit cat) a

-- | "fst" specialized to Hask to aid with type inference
-- FIXME: this will not be needed with injective types
fst :: (a,b) -> a
fst (a,_) = a

-- | "snd" specialized to Hask to aid with type inference
-- FIXME: this will not be needed with injective types
snd :: (a,b) -> b
snd (_,b) = b

-- | Creates an arrow that ignores its first parameter.
const ::
    ( Cartesian cat
    , ValidCategory cat a
    , ValidCategory cat b
    ) => a -> cat b a
const a = const2 a undefined

-- | Based on the type signature, this looks like it should be the inverse of "embed" function.
-- But it's not.
-- This function completely ignores its second argument!
const2 ::
    ( Cartesian cat
    , ValidCategory cat a
    , ValidCategory cat b
    ) => a -> b -> cat b a
const2 a b = initial a . terminal b

instance Cartesian ((->) :: * -> * -> *) where
    fst_ (a,_) = a
    snd_ (_,b) = b
    terminal _ _ = ()
    initial a _ = a

-- | Closed monoidal categories allow currying, and closed braided categories allow flipping.
-- We combine them into a single "Closed" class to simplify notation.
--
-- In general, closed categories need not be either "Monoidal" or "Braided".
-- All a closed category requires is that all arrows are also objects in the category.
-- For example, `a +> (b +> c)` is a vallid arrow in the category `(+>)`.
-- But I don't know of any uses for this general definition of a closed category.
-- And this restricted definition seems to have a lot of uses.
--
-- More details available at <https://en.wikipedia.org/wiki/Closed_category wikipedia>
-- and <http://ncatlab.org/nlab/show/closed+category ncatlab>
class Braided cat => Closed cat where
    curry :: cat (Tensor cat a b) c -> cat a (cat b c)
    uncurry :: cat a (cat b c) -> cat (Tensor cat a b) c

    -- | The default definition should be correct for any category,
    -- but can be overridden with a more efficient implementation.
    --
    -- FIXME: does this actually need to be in a class?
    -- or should it be a stand-alone function like "const"?
    flip :: cat a (cat b c) -> cat b (cat a c)
    flip f = curry (uncurry f . braid)

instance Closed (->) where
    curry f = \a b -> f (a,b)
    uncurry f = \(a,b) -> f a b

-- | Groupoids are categories where every arrow can be reversed.
-- This generalizes bijective and inverse functions.
-- Notably, 'Hask' is NOT a Groupoid.
--
-- More details available at <http://en.wikipedia.org/wiki/Groupoid wikipedia>
-- <http://ncatlab.org/nlab/show/groupoid ncatlib>, and
-- <http://mathoverflow.net/questions/1114/whats-a-groupoid-whats-a-good-example-of-a-groupoid stack overflow>.
class Category cat => Groupoid cat where
    inverse :: cat a b -> cat b a

-- | Compact categories are another generalization from linear algebra.
-- In a compact category, we can dualize any object in the same way that we
-- can generate a covector from a vector.
-- Notably, 'Hask' is NOT compact.
--
-- More details available at <http://en.wikipedia.org/wiki/Compact_closed_category wikipedia>
-- and <http://ncatlab.org/nlab/show/dagger-compact+category ncatlab>.
class Symmetric cat => Compact cat where
    type Dual cat x
    unit :: cat x (Tensor cat x (Dual cat x))
    counit :: cat (Tensor cat (Dual cat x) x) x

-- | A dagger (also called an involution) is an arrow that is its own inverse.
-- Daggers generalize the idea of a transpose from linear algebra.
-- Notably, 'Hask' is NOT a dagger category.
--
-- More details avalable at <https://en.wikipedia.org/wiki/Dagger_category wikipedia>
-- and <http://ncatlab.org/nlab/show/dagger-category ncatlab>
class Category cat => Dagger cat where
    dagger :: cat a b -> cat b a

--------------------------------------------------------------------------------

-- | This data family can be used to provide proofs that an arrow in Hask (i.e. a function) can be embedded in some subcategory.
-- We're travelling in the opposite direction of the subtyping mechanism.
-- That's valid only in a small number of cases.
-- These proofs give a type safe way to capture some (but not all) of those cases.
data family ProofOf (cat :: k -> k -> *) a

newtype instance ProofOf Hask a = ProofOf { unProofOfHask :: a }

-- FIXME: which direction should the subtyping go?
instance Sup (ProofOf cat a) a (ProofOf cat a)
instance Sup a (ProofOf cat a) (ProofOf cat a)

instance a <: ProofOf Hask a where
    embedType_ = Embed0 ProofOf

-- | A provable category gives us the opposite ability as a Concrete category.
-- Instead of embedding a function in the subcategory into Hask,
-- we can embed certain functions from Hask into the subcategory.
class Concrete cat => Provable cat where

    -- | If you want to apply a function inside of a proof,
    -- you must use the "($$)" operator instead of the more commonly used "($)".
    --
    -- FIXME:
    -- This is rather inelegant.
    -- Is there any way to avoid this?
    infixr 0 $$
    ($$) :: cat a b -> ProofOf_ cat a -> ProofOf_ cat b

type family ProofOf_ cat a where
    ProofOf_ Hask a = a
    ProofOf_ cat  a = ProofOf cat a

