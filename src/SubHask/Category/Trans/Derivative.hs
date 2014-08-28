module SubHask.Category.Trans.Derivative
    ( Differentiable (..)
    , C
    , DerivativeT
    , unsafeProveDerivative
    , unsafeProveC1
    , unsafeProveC2
    )
    where

import qualified Data.Vector as V

import SubHask.Algebra
import SubHask.Category
import SubHask.Category.Trans.Common
import SubHask.Internal.Prelude
import GHC.Exts

import qualified Prelude as P

-------------------------------------------------------------------------------

class Category cat => Differentiable cat where
    type Derivative cat :: k -> k -> *
    derivative :: cat a b -> Derivative cat a b

type family C (n::Nat) (cat :: * -> * -> *) :: Constraint where
    C 0 cat = ()
    C n cat = (Differentiable cat, C (n-1) cat)

-------------------------------------------------------------------------------

newtype DerivativeT (n::Nat) (cat :: k -> k -> *) a b = DerivativeT (V.Vector (cat a b))

instance 
    ( KnownNat n
    , 1 <= n
    , Category cat
    ) => Category (DerivativeT n cat) 
        where

    type ValidCategory (DerivativeT n cat) a b = 
        ( ValidCategory cat a b
        , Rng (cat a b)
        , a ~ b
        )

    id = DerivativeT $ V.replicate n id
        where n = fromInteger $ natVal (Proxy::Proxy n)
            
    (DerivativeT v1).(DerivativeT v2) = DerivativeT $ V.generate n go
        where 
            go i = case i of
                0 -> f 0 . g 0
                1 -> g 1 . g 0 * g 1
                2 -> f 2 . g 0 * g 1 * g 1 + f 1 . g 0 * g 2

            f i = v1 V.! i
            g i = v1 V.! i
            n = fromInteger $ natVal (Proxy::Proxy n)

instance 
    ( KnownNat n
    , 1 <= n
    , SubCategory supercat cat
    ) => SubCategory supercat (DerivativeT n cat) 
        where

    embed (DerivativeT v) = embed $ V.head v

instance 
    ( KnownNat n
    , 1 <= n
    , Category cat
    ) => Differentiable (DerivativeT n cat) 
        where
    type Derivative (DerivativeT n cat) = DerivativeT (n-1) cat
    derivative (DerivativeT v) = DerivativeT $ V.tail v 

unsafeProveDerivative :: Proxy n -> V.Vector (cat a b) -> DerivativeT n cat a b
unsafeProveDerivative _ = DerivativeT

unsafeProveC1 :: cat a b -> cat a b -> DerivativeT 1 cat a b
unsafeProveC1 f f' = DerivativeT [f,f']

unsafeProveC2 :: cat a b -> cat a b -> cat a b -> DerivativeT 2 cat a b
unsafeProveC2 f f' f'' = DerivativeT [f,f',f'']

-------------------------------------------------------------------------------

instance Semigroup b => Semigroup (a -> b) where
    f + g = \x -> f x + g x

instance Monoid b => Monoid (a -> b) where
    zero = \x -> zero

instance Group b => Group (a -> b) where
    negate f = \x -> negate $ f x

instance Abelian b => Abelian (a -> b) 

instance Rng b => Rng (a -> b) where
    f * g = \x -> f x * g x

_sin :: DerivativeT 3 Hask Double Double
_sin = DerivativeT $ V.fromList [P.sin,P.cos,\x -> -P.sin x] --, \x -> -P.cos x]
