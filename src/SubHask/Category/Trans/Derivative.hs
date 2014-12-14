{-# LANGUAGE IncoherentInstances #-}

-- | This module provides a category transformer for automatic differentiation.
-- There are many notions of a generalized derivative.
-- Perhaps the most common is the differential Ring.
--
-- > class Field r => Differential r where
-- >    derivative :: r -> r
-- >
-- > type Diff cat = forall a b. (Category cat, Differential cat a b)
--
-- But this runs into problems with the lack of polymorphic constraints in GHC.
-- See, for example <https://ghc.haskell.org/trac/ghc/ticket/2893 GHC ticket #2893>.
--
-- References:
--
-- * <http://en.wikipedia.org/wiki/Differential_algebra wikipedia article on differntial algebras>
module SubHask.Category.Trans.Derivative
    where

import qualified Data.Vector.Generic as VG

import SubHask.Algebra
import SubHask.Category
import SubHask.SubType
import SubHask.Internal.Prelude

import qualified Prelude as P

import SubHask.Compatibility.Vector
import SubHask.Compatibility.HMatrix

infixr ><
type family (><) (a::k1) (b::k2) :: * where
    Double >< Double = Double
    Vector >< Double = Vector Double
    Vector Double >< Double = Vector Double
    Vector Double >< Vector Double = Matrix Double
    a >< (b +> c) = a >< b >< c

-- class C  (n::Nat) (cat :: * -> * -> *) | cat -> n where
--     type D cat :: * -> * -> *
--     derivative_ :: cat a b -> D cat a (a >< b)

-- data family a +> b
-- newtype instance Double +> Double = DD Double
-- newtype instance Vector Double +> Double = VD (Vector Double)
-- newtype instance Vector Double +> Vector Double = VV (Matrix Double)

newtype (+>) a b = Linear (a><b)

deriving instance Show (a><b) => Show (a +> b)

class C (dcat :: * -> * -> *) (n::Nat) (cat :: * -> * -> *) | dcat cat -> n where
    type D dcat cat :: * -> * -> *
    derivative_ :: proxy dcat -> cat a b -> D dcat cat a (dcat a b)

class (D dcat cat~cat) => Smooth dcat cat

--------------------------------------------------------------------------------

data CT dcat (n::Nat) cat a b where
    CT0 :: cat a b -> CT dcat 0 cat a b
    CTn :: cat a b -> CT dcat (n-1) cat a (dcat a b) -> CT dcat n cat a b

instance (1 <= n) => C dcat n (CT dcat n cat) where
    type D dcat (CT dcat n cat) = CT dcat (n-1) cat
    derivative_ _ (CTn _ f') = f'

-- instance
--     ( Semigroup b
--     ) => Semigroup (CT n Hask a b)
--         where
--     (CT0 f   )+(CT0 g   ) = CT0 (f+g)
--     (CTn f f')+(CTn g g') = CTn (f+g) (f'+g')

derivative :: C (+>) n cat => cat a b -> D (+>) cat a (a +> b)
derivative = derivative_ (Proxy::Proxy (+>))

unsafeProveC0 :: cat a b -> CT dcat 0 cat a b
unsafeProveC0 f = CT0 f

unsafeProveC1 :: cat a b -> cat a (dcat a b) -> CT dcat 1 cat a b
unsafeProveC1 f f' = CTn f $ unsafeProveC0 f'

unsafeProveC2 :: cat a b -> cat a (dcat a b) -> cat a (dcat a (dcat a b)) -> CT dcat 2 cat a b
unsafeProveC2 f f' f'' = CTn f $ unsafeProveC1 f' f''

-- derivative_ :: (1 <= n) => CT dcat n cat a b -> CT dcat (n-1) cat a (a+>b)
-- derivative_ (CTn _ f') = f'
--
-- derivative :: (1 <= n) => Ring a => CT dcat n Hask a b -> CT dcat 0 Hask a b
-- derivative (CTn _ (CT0 f'  )) = CT0 $ \a -> f' a 1
-- derivative (CTn _ (CTn f' _)) = CT0 $ \a -> f' a 1
--
-- derivative2 :: Ring a => CT dcat 2 Hask a b -> CT dcat 0 Hask a b
-- derivative2 (CTn _ (CTn _ (CT0 f'))) = CT0 $ \a -> f' a 1 1

-------------------

instance Sup (CT dcat n cat) cat cat
instance Sup cat (CT dcat n cat) cat

instance CT dcat 0 cat <: cat where
    embedType_ = Embed2 unCT0
        where
            unCT0 :: CT dcat 0 cat a b -> cat a b
            unCT0 (CT0 f) = f

instance CT dcat n cat <: cat where
    embedType_ = Embed2 unCTn
        where
            unCTn :: CT dcat n cat a b -> cat a b
            unCTn (CTn f f') = f

-- FIXME: these subtyping instance should be made more generic
-- the problem is that type families aren't currently powerful enough

instance Sup (CT dcat 0 cat) (CT dcat 1 cat) (CT dcat 0 cat)
instance Sup (CT dcat 1 cat) (CT dcat 0 cat) (CT dcat 0 cat)
instance CT dcat 1 cat <: CT dcat 0 cat where embedType_ = Embed2 m2n where m2n (CTn f f') = CT0 f

instance Sup (CT dcat 0 cat) (CT dcat 2 cat) (CT dcat 0 cat)
instance Sup (CT dcat 2 cat) (CT dcat 0 cat) (CT dcat 0 cat)
instance CT dcat 2 cat <: CT dcat 0 cat where embedType_ = Embed2 m2n where m2n (CTn f f') = CT0 f

instance Sup (CT dcat 1 cat) (CT dcat 2 cat) (CT dcat 1 cat)
instance Sup (CT dcat 2 cat) (CT dcat 1 cat) (CT dcat 1 cat)
instance CT dcat 2 cat <: CT dcat 1 cat where embedType_ = Embed2 m2n where m2n (CTn f f') = CTn f (embedType2 f')

-------------------

instance Category cat => Category (CT dcat 0 cat) where
    type ValidCategory (CT dcat 0 cat) a = ValidCategory cat a
    id = CT0 id
    (CT0 f).(CT0 g) = CT0 $ f.g

-- | FIXME: these instances could be made more generic if the <: instance were more generic
-- instance Category Hask => Category (CT dcat 1 Hask) where
--     type ValidCategory (CT dcat 1 Hask) a = (Rg a, ValidCategory Hask a)
--     id = CTn id (CT0 $ \x -> id)
--     (CTn f f') . g_@(CTn g g') = CTn (f.g) ((f'.embedType2 g_) *** g')
--         where
--             (***) :: CT dcat 0 Hask a (b -> c) -> CT dcat 0 Hask a (a -> b) -> CT dcat 0 Hask a (a -> c)
--             (***) f g = CT0 $ \a -> (f $ a) . (g $ a)

--------------------------------------------------------------------------------
-- tests

test1 :: CT (+>) 1 Hask Double Double
test1 = unsafeProveC1 (\x -> x**3) (\x -> Linear $ 3*x**2)

test2 :: CT (+>) 2 Hask (Vector Double) Double
test2 = unsafeProveC2
    (\x -> exp (- x<>w) )
    (\x -> Linear $ exp (- x<>w) *. (-w) )
    (\x -> Linear $ exp (- x<>w) *. w >< w )
    where
        w = VG.fromList [1,2] :: Vector Double

v = VG.fromList [1,3] :: Vector Double
