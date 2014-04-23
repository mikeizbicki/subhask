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
    , withCategory
    
    -- * Special types of categories
    , Concrete (..)
    , Groupoid (..)
    , Monoidal (..)
    ) where

import GHC.Prim
import qualified Prelude as P

-------------------------------------------------------------------------------

-- | This 'Category' class modifies the one in the Haskell standard to include 
-- the 'ValidCategory' type constraint.  This constraint let's us make instances 
-- of arbitrary subcategories of Hask.

class Category cat where

--     type ValidCategory cat (a :: k1) (b :: k2) :: Constraint
--     type ValidCategory cat (a :: *)  (b :: *) = ()

    type ValidCategory cat a b :: Constraint
    type ValidCategory cat a b = ()

    id :: ValidCategory cat a a => cat a a

    infixr 9 .
    (.) :: 
        ( ValidCategory cat b c
        , ValidCategory cat a b
        , ValidCategory cat a c
        ) => cat b c -> cat a b -> cat a c

-------------------

-- data Cat a b cat1 cat2 = Cat (cat1 a b -> cat2 a b)
-- 
-- instance Category (Cat a b) where
--     type ValidCategory (Cat a b) cat1 cat2 =
--         ( ValidCategory cat1 a b
--         , ValidCategory cat2 a b
--         )
-- 
--     id = Cat id
--     (Cat f).(Cat g) = Cat $ f.g
--     
-- 
-- data Linear r a b = Linear (a r -> b r)
-- 
-- instance Category (Linear r) where
--     type ValidCategory (Linear r) (a :: * -> *) (b :: * -> *) = ()
--     id = Linear id
--     (Linear f1).(Linear f2) = Linear $ f1.f2

-- class NCategory cat where
-- 
--     type ValidNCategory cat (a :: * -> *) (b :: * -> *) :: Constraint
--     type ValidNCategory cat a b = ()
-- 
--     idN :: ValidCategory cat a a => cat a a
-- 
--     infixr 9 ...
--     (...) :: 
--         ( ValidCategory cat b c
--         , ValidCategory cat a b
--         , ValidCategory cat a c
--         ) => cat b c -> cat a b -> cat a c
-- 

---------------------------------------

-- | A subcategory is valid only for some of the arrows and objects.
-- Intuiitively, elements of a subcategory satisfy additional properties
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

embed2 :: SubCategory cat subcat => subcat a (subcat a b) -> cat a (cat a b)
embed2 f = P.undefined

-- | The category with Haskell types as objects, and functions as arrows.

type Hask = (->)

instance Category (->) where
    id = P.id
    (.) = (P..)


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

embedHask :: (Concrete subcat, ValidCategory subcat a b) => subcat a b -> a -> b
embedHask = embed

withCategory :: (ValidCategory cat a b, Concrete cat) => proxy cat -> cat a b -> a -> b
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

-- | Groupoids are categories where every arrow can be reversed.  This generalizes
-- bijective and inverse functions.
--
-- More details available at <http://en.wikipedia.org/wiki/Groupoid wikipedia>
-- and <http://ncatlab.org/nlab/show/groupoid ncatlib>.
class Category cat => Groupoid cat where
    inverse :: ValidCategory cat a b => cat a b -> cat b a

-- | The intuition behind a monoidal category is similar to the intuition 
-- behind the 'SubHask.Algebra.Monoid' algebraic structure.  Unfortunately,
-- there are a number of rather awkward laws to work out the technical details.
-- Even worse, the types for the tensor product don't work out very nicely
-- in Haskell syntax.  Anyone have any ideas how to make this prettier?!
--
-- More details available at <http://en.wikipedia.org/wiki/Monoidal_category wikipedia>
-- and <http://ncatlab.org/nlab/show/monoidal+category ncatlab>.

class Category cat => Monoidal cat where
    type Tensor cat :: * -> * -> *
    type Tensor cat = (,) 

    type Unit cat :: *
    type Unit cat = ()

    associatorL :: forall proxy a b c.
        ( ValidCategory cat a b
        , ValidCategory cat b c
        , ValidCategory cat a c
        ) => proxy cat -> Tensor cat (Tensor cat a b) c -> Tensor cat a (Tensor cat b c)

    associatorR :: 
        ( ValidCategory cat a b
        , ValidCategory cat b c
        , ValidCategory cat a c
        ) => proxy cat -> Tensor cat a (Tensor cat b c) -> Tensor cat (Tensor cat a b) c

    unitorL :: ValidCategory cat a a => proxy cat -> Tensor cat () a -> a
    unitorR :: ValidCategory cat a a => proxy cat -> Tensor cat a () -> a

