-- | This module defines the basic infrastructure for working with categories.
-- Because we are using the ConstraintKinds extension, we can encode many
-- more categories than the standard "Data.Category" class is capable of.
-- 
-- There are a number of unfortunate difficulties with encoding category theory.
-- The first is that nomenclature is often nonstandard.
-- In this module, we generally follow the names laid out in John Baez and Mike Stay's
-- <http://math.ucr.edu/home/baez/rosetta.pdf Rosetta Stone paper>.
-- This is a fairly accessible introduction to category theory for Haskeller's ready
-- to move beyond \"monads are monoids in the category of endofunctors.\"
--
-- A second unfortunate problem is that the laws for category classes are rather 
-- cumbersome to describe.
-- So I haven't explicitly stated the laws for any classes.
-- Instead, I have provided links to wikipedia and ncatlab that provide quite a bit
-- of detail of each class.
-- These resources should also be valuable to the Haskeller wishing to delve more
-- deeply into category theory.
--
module SubHask.Category
    ( 
    -- * Categories
    Category (..)
    , SubCategory (..)
    , embed2

    -- * Hask
    , Hask
    , ($)
    , embedHask
    , embedHask2
    , embedHask3
    , withCategory

    -- * Cat
    , Cat
    , CatT
    
    -- * Special types of categories
    , Concrete (..)
    , Monoidal (..)
    , (><)
    , Braided (..)
    , Symmetric (..)
    , Cartesian (..)
    , duplicate
    , Closed (..)

    , Groupoid (..)
    , Compact (..)
    , Dagger (..)
    ) where

import GHC.Prim
import SubHask.Internal.Prelude
import qualified Prelude as P

-------------------------------------------------------------------------------

-- | This 'Category' class modifies the one in the Haskell standard to include 
-- the 'ValidCategory' type constraint.  This constraint let's us make instances 
-- of arbitrary subcategories of Hask.
class Category (cat :: k1 -> k2 -> *) where

--     type ValidCategory cat a b :: Constraint
--     type ValidCategory cat a b = ()

    type ValidCategory cat (a::k1') (b::k2') :: Constraint
    type ValidCategory cat (a::k1') (b::k2') = ()

    id :: ValidCategory cat a a => cat a a

    infixr 9 .
    (.) :: 
        ( ValidCategory cat b c
        , ValidCategory cat a b
        , ValidCategory cat a c
        ) => cat b c -> cat a b -> cat a c

-- | The category with Haskell types as objects, and functions as arrows.
--
-- More details available at the <http://www.haskell.org/haskellwiki/Hask Haskell wiki>.
type Hask = (->)

instance Category (->) where
    id = P.id
    (.) = (P..)

-- | The category with categories as objects and functors as arrows.
--
-- More details available at <https://en.wikipedia.org/wiki/Category_of_categories wikipedia>
-- and <http://ncatlab.org/nlab/show/Cat ncatlab>.
type Cat cat1 cat2 = forall a b. CatT (->) a b cat1 cat2

newtype CatT 
    ( cat :: * -> * -> *)
    ( a :: k )
    ( b :: k )
    ( cat1 :: k -> k -> * ) 
    ( cat2 :: k -> k -> * )
    = CatT (cat1 a b `cat` cat2 a b)

instance Category cat => Category (CatT cat a b) where
    type ValidCategory (CatT cat a b) cat1 cat2 =
        ( ValidCategory cat1 a b
        , ValidCategory cat2 a b
        , ValidCategory cat (cat1 a b) (cat2 a b)
        )

    id = CatT id
    (CatT f).(CatT g) = CatT $ f.g

-- TODO: We would rather have the definition of CatT not depend on the a and b 
-- variables, as in the code below.  Unfortunately, GHC 7.8's type checker isn't
-- strong enough to handle forall inside of a type class.
-- 
-- data CatT 
--     ( cat :: * -> * -> *)
--     ( cat1 :: * -> * -> * ) 
--     ( cat2 :: * -> * -> * )
--     = forall a b. 
--         ( ValidCategory cat1 a b
--         , ValidCategory cat2 a b
--         ) => CatT (cat1 a b `cat` cat2 a b)
-- 
-- instance Category cat => Category (CatT cat) where
--     type ValidCategory (CatT cat) cat1 cat2 = forall a b.
--         ( ValidCategory cat1 a b
--         , ValidCategory cat2 a b
--         , ValidCategory cat (cat1 a b) (cat2 a b)
--         )
-- 
--     id = CatT id
--     (CatT f).(CatT g) = CatT $ f.g
    
-- TODO: can this be extended to functor categories?
-- http://ncatlab.org/nlab/show/functor+category

---------------------------------------

-- | Intuiitively, arrows and objects in a subcategory satisfy additional properties
-- that elements of the larger category do not necessarily satisfy.
-- Elements of a subcategory can always be embeded in the larger category.
-- Going in the other direction, however, requires a proof.  These proofs
-- can (usually) not be verified by the type system and are therefore labeled
-- unsafe.
--
-- More details available at <http://en.wikipedia.org/wiki/Subcategory wikipedia>
-- and <http://ncatlab.org/nlab/show/subcategory ncatlab>.

class (Category cat, Category subcat) => SubCategory cat subcat where
    embed :: ValidCategory subcat a b => subcat a b -> cat a b

instance Category c => SubCategory c c where
    embed = id

-- | This would be a useful function to have, but I'm not sure how to implement it yet!
embed2 :: SubCategory cat subcat => subcat a (subcat a b) -> cat a (cat a b)
embed2 f = undefined


-- | We generalize the Prelude's definition of "$" so that it applies to any 
-- subcategory of 'Hask' (that is, any 'Concrete' 'Category'.  This lets us 
-- easily use these subcategories as functions. For example, given a polynomial 
-- function
--
-- > f :: Polynomial Double Double
--
-- we can evaluate the polynomial at the number 5 by
--
-- > f $ 5

($) :: 
    ( Concrete subcat
    , ValidCategory subcat a b
    ) => subcat a b -> a -> b
($) = embed

infixr 0 $

-- | Embeds a unary function into 'Hask'
embedHask :: 
    ( Concrete cat
    , ValidCategory cat a b
    ) => cat a b -> a -> b
embedHask = embed

-- | Embeds a binary function into 'Hask'
embedHask2 :: 
    ( Concrete cat
    , ValidCategory cat b c
    , ValidCategory cat a (cat b c)
    ) => cat a (cat b c) -> a -> b -> c
embedHask2 f = \a b -> (f $ a) $ b

-- | Embeds a trinary function into 'Hask'
embedHask3 ::
    ( Concrete cat
    , ValidCategory cat c d
    , ValidCategory cat b (cat c d)
    , ValidCategory cat a (cat b (cat c d))
    ) => cat a (cat b (cat c d)) -> a -> b -> c -> d
embedHask3 f = \a b c -> ((f $ a) $ b) $ c

-- | This is a special form of the 'embed' function which can make specifying the 
-- category we are embedding into easier.
withCategory :: 
    ( ValidCategory cat a b
    , Concrete cat
    ) => proxy cat -> cat a b -> a -> b
withCategory _ f = embed f

-------------------------------------------------------------------------------

-- | Technicaly, a conrete category is any category equiped with a faithful 
-- functor to the category of sets.  This is just a little too platonic to 
-- be represented in Haskell, but 'Hask' makes a pretty good approximation.
-- So we call any 'SubCategory' of 'Hask' 'Concrete'.  Importantly, not
-- all categories are concrete.   See the 'SubHask.Category.Slice.Slice'
-- category for an example. 
--
-- More details available at <http://en.wikipedia.org/wiki/Concrete_category wikipedia>
-- and <http://ncatlab.org/nlab/show/concrete+category ncatlib>.
type Concrete cat = SubCategory (->) cat

-- | The intuition behind a monoidal category is similar to the intuition 
-- behind the 'SubHask.Algebra.Monoid' algebraic structure.  Unfortunately,
-- there are a number of rather awkward laws to work out the technical details.
-- The associator and unitor functions are provided to demonstrate the 
-- required isomorphisms.
--
-- More details available at <http://en.wikipedia.org/wiki/Monoidal_category wikipedia>
-- and <http://ncatlab.org/nlab/show/monoidal+category ncatlab>.

class 
    ( Category cat 
    ) => Monoidal cat 
        where

    data Tensor cat a b :: *
    data Unit cat :: *
    
    tensor :: cat a (cat b (Tensor cat a b)) 

    associatorL ::
        ( ValidCategory cat a b
        , ValidCategory cat b c
        , ValidCategory cat a c
        ) => Tensor cat (Tensor cat a b) c -> Tensor cat a (Tensor cat b c)

    associatorR ::
        ( ValidCategory cat a b
        , ValidCategory cat b c
        , ValidCategory cat a c
        ) => Tensor cat a (Tensor cat b c) -> Tensor cat (Tensor cat a b) c

    unitorL :: ValidCategory cat a a => Tensor cat (Unit cat) a -> a
    unitorR :: ValidCategory cat a a => Tensor cat a (Unit cat) -> a

-- | This is a convenient and (hopefully) suggestive shortcut for constructing
-- tensor cartesianProducts in 'Concrete' categories.
infixl 7 ><
(><) :: forall cat a b. 
    ( Monoidal cat
    , Concrete cat
    , ValidCategory cat a b
    , ValidCategory cat b (Tensor cat a b)
    , ValidCategory cat a (cat b (Tensor cat a b))
    ) => a -> b -> Tensor cat a b
(><) = embedHask2 tensor

instance Monoidal (->) where
    data Tensor (->) a b = TensorHask a b
        deriving (Read,Show,Eq,Ord)

    data Unit (->) = UnitHask
        deriving (Read,Show,Eq,Ord)

    tensor = TensorHask

    associatorL (TensorHask (TensorHask a b) c) = TensorHask a (TensorHask b c)
    associatorR (TensorHask a (TensorHask b c)) = TensorHask (TensorHask a b) c

    unitorL (TensorHask UnitHask a) = a
    unitorR (TensorHask a UnitHask) = a

-- | Braided categories let us switch the order of tensored objects.
--
-- More details available at <https://en.wikipedia.org/wiki/Braided_monoidal_category wikipedia> 
-- and <http://ncatlab.org/nlab/show/braided+monoidal+category ncatlab>
class Monoidal cat => Braided cat where
    braid :: Tensor cat a b -> Tensor cat b a
    unbraid :: Tensor cat b a -> Tensor cat a b

instance Braided (->) where
    braid (TensorHask a b) = TensorHask b a
    unbraid = braid

-- | In a symmetric braided category, 'braid' and 'unbraid' are inverses of each other.
--
-- More details available at <http://en.wikipedia.org/wiki/Symmetric_monoidal_category wikipedia>
class Braided cat => Symmetric cat
instance Symmetric (->)

-- | In a cartesian monoidal category, the monoid object acts in a particularly nice way
-- where we can compose and decompose it.
-- Intuitively, we can delete informationusing the 'fst' and 'snd' arrows, as well as 
-- duplicate information using the 'duplicate' arrow.
--
-- More details available at <http://ncatlab.org/nlab/show/cartesian+monoidal+category ncatlab>
class Symmetric cat => Cartesian cat where
    fst :: cat (Tensor cat a b) a
    snd :: cat (Tensor cat a b) b
    cartesianProduct :: cat a b -> cat a c -> cat a (Tensor cat b c)
    terminal :: a -> cat a (Unit cat)

instance Cartesian (->) where 
    fst (TensorHask a b) = a
    snd (TensorHask a b) = b
    cartesianProduct f g = \a -> TensorHask (f a) (g a)
    terminal a = \_ -> UnitHask

-- | In a 'Cartesian' category, we can duplicate information.
duplicate :: 
    ( ValidCategory cat a (Tensor cat a a)
    , ValidCategory cat a a
    , Cartesian cat
    ) => cat a (Tensor cat a a)
duplicate = cartesianProduct id id 

-- | Closed categories allow currying.
--
-- More details available at <https://en.wikipedia.org/wiki/Closed_category wikipedia> 
-- and <http://ncatlab.org/nlab/show/closed+category ncatlab>
class Monoidal cat => Closed cat where
    curry :: cat (Tensor cat a b) c -> cat a (cat b c)
    uncurry :: cat a (cat b c) -> cat (Tensor cat a b) c

instance Closed (->) where
    curry f = \a b -> f (TensorHask a b)
    uncurry f = \(TensorHask a b) -> f a b

-- | Groupoids are categories where every arrow can be reversed.  
-- This generalizes bijective and inverse functions.
-- Notably, 'Hask' is NOT a Groupoid.
--
-- More details available at <http://en.wikipedia.org/wiki/Groupoid wikipedia>
-- and <http://ncatlab.org/nlab/show/groupoid ncatlib>.
class Category cat => Groupoid cat where
    inverse :: ValidCategory cat a b => cat a b -> cat b a

-- | Compact categories are another generalization from linear algebra.
-- In a compact category, we can dualize any object in the same way that we 
-- can generate a covector from a vector.
-- Notably, 'Hask' is NOT compact.
--
-- More details available at <http://en.wikipedia.org/wiki/Compact_closed_category wikipedia>
-- and <http://ncatlab.org/nlab/show/dagger-compact+category ncatlab>.
class Closed cat => Compact cat where
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
