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

--     , Functor

    -- * Hask
    , Hask
    , ($)
    , embedHask
    , embedHask2
    , withCategory
    , embed2

    {-
    -- * Cat
    , Cat
    , CatT
    -}
    
    -- * Special types of categories
    , Concrete (..)
    , Monoidal (..)
--     , (><)
    , Braided (..)
    , Symmetric (..)
    , Cartesian (..)
    , const
    , const2
--     , duplicate
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
class Category (cat :: k -> k -> *) where

    type ValidCategory cat (a::k) :: Constraint

    id :: ValidCategory cat a => cat a a

    infixr 9 .
    (.) :: cat b c -> cat a b -> cat a c

-- | The category with Haskell types as objects, and functions as arrows.
--
-- More details available at the <http://www.haskell.org/haskellwiki/Hask Haskell wiki>.
type Hask = (->)

instance Category (->) where
    type ValidCategory (->) (a :: *) = ()
    id = P.id
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

-- | Intuiitively, arrows and objects in a subcategory satisfy additional properties
-- that elements of the larger category do not necessarily satisfy.
-- Elements of a subcategory can always be embeded in the larger category.
-- Going in the other direction, however, requires a proof.  These proofs
-- can (usually) not be verified by the type system and are therefore labeled
-- unsafe.
--
-- More details available at <http://en.wikipedia.org/wiki/Subcategory wikipedia>
-- and <http://ncatlab.org/nlab/show/subcategory ncatlab>.

class 
    ( Category cat
    , Category subcat
    ) => SubCategory 
        (subcat :: k1 -> k1 -> *) 
        (cat    :: k0 -> k0 -> *) 
        where

    embed :: subcat a b -> cat a b

instance Category c => SubCategory c c where
    embed = id

-- | Technicaly, a conrete category is any category equiped with a faithful 
-- functor to the category of sets.  This is just a little too platonic to 
-- be represented in Haskell, but 'Hask' makes a pretty good approximation.
-- So we call any 'SubCategory' of 'Hask' 'Concrete'.  Importantly, not
-- all categories are concrete.   See the 'SubHask.Category.Slice.Slice'
-- category for an example. 
--
-- More details available at <http://en.wikipedia.org/wiki/Concrete_category wikipedia>
-- and <http://ncatlab.org/nlab/show/concrete+category ncatlib>.
type Concrete cat = SubCategory cat (->) 

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

($) :: Concrete subcat => subcat a b -> a -> b
($) = embed

infixr 0 $

-- | Embeds a unary function into 'Hask'
embedHask :: Concrete subcat => subcat a b -> a -> b
embedHask = embed

-- | Embeds a binary function into 'Hask'
embedHask2 ::  Concrete subcat => subcat a (subcat b c)  -> a -> b -> c 
embedHask2 f = \a b -> (f $ a) $ b

-- | This is a special form of the 'embed' function which can make specifying the 
-- category we are embedding into easier.
withCategory :: Concrete subcat => proxy subcat -> subcat a b -> a -> b
withCategory _ f = embed f

-- | FIXME: This would be a useful function to have, but I'm not sure how to implement it yet!
embed2 :: SubCategory cat subcat => subcat a (subcat a b) -> cat a (cat a b)
embed2 f = undefined

-------------------------------------------------------------------------------

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
    , ValidCategory cat (TUnit cat)
    ) => Monoidal cat 
        where

    type Tensor cat :: k -> k -> k
    tensor :: 
        ( ValidCategory cat a 
        , ValidCategory cat b 
        ) => cat a (cat b (Tensor cat a b)) 

    type TUnit cat :: k
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
    braid :: Proxy cat -> Tensor cat a b -> Tensor cat b a
    unbraid :: Proxy cat -> Tensor cat b a -> Tensor cat a b

instance Braided (->) where
    braid _ (a,b) = (b,a)
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
    fst :: 
        ( ValidCategory cat a
        , ValidCategory cat b
        , ValidCategory cat (Tensor cat a b)
        ) => cat (Tensor cat a b) a

    snd :: 
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
    fst (a,b) = a
    snd (a,b) = b
    terminal a _ = ()
    initial a _ = a

-- | In a 'Cartesian' category, we can duplicate information.
-- duplicate :: 
--     ( ValidCategory cat a (Tensor cat a a)
--     , ValidCategory cat a a
--     , Cartesian cat
--     ) => cat a (Tensor cat a a)
-- duplicate = cartesianProduct id id 

-- | Closed categories allow currying.
--
-- More details available at <https://en.wikipedia.org/wiki/Closed_category wikipedia> 
-- and <http://ncatlab.org/nlab/show/closed+category ncatlab>
class Monoidal cat => Closed cat where
    curry :: cat (Tensor cat a b) c -> cat a (cat b c)
    uncurry :: cat a (cat b c) -> cat (Tensor cat a b) c

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
