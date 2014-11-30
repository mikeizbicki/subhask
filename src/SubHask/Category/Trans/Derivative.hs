{-# LANGUAGE IncoherentInstances #-}

module SubHask.Category.Trans.Derivative
--     ( Differentiable (..)
--     , C
--     , DerivativeT
--     , unsafeProveDerivative
--     , unsafeProveC1
--     , unsafeProveC2
--     )
    where

import qualified Data.Vector as V

import SubHask.Algebra
import SubHask.Category
import SubHask.SubType
import SubHask.Category.Trans.Common
import SubHask.Internal.Prelude

import qualified Prelude as P

import SubHask.Algebra.Vector
import SubHask.Algebra.HMatrix

-- | Ideally, we would define the category Diff in terms of a differential field like:
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

class Diff cat where
    type DiffCat cat :: k -> k -> *
    derivative :: cat a a -> DiffCat cat a a

----

type family Rank (r::Nat) a

type instance Rank n (Vector r) = Rank_Vector n r
type family Rank_Vector (n::Nat) r where
    Rank_Vector 0 r = r
    Rank_Vector 1 r = Vector r
    Rank_Vector n r = Matrix r

type instance Rank n Double = Rank_Double n
type family Rank_Double (n::Nat) where
    Rank_Double n = Double

----

box ::
    (
    ) => Proxy a
      -> Proxy b
      -> Proxy c
      -> Proxy r
      -> (Rank a r -> Rank (b+c) r)
      -> (Rank a r -> Rank (a+b) r)
      -> (Rank a r -> Rank (a+c) r)
box _ _ _ _ f g = undefined

----

{-
data CT a (r::Nat) (s::Nat) where
    CT ::
        ( VectorSpace (Rank r a)
        , VectorSpace (Rank s a)
        , VectorSpace (Rank (r+s) a)
        ) => !(Rank r a -> Rank s a)
          -> !(Rank r a -> Rank (r+s) a)
          -> CT a r s

instance Category (CT a) where
    type ValidCategory (CT a) r =
        ( VectorSpace (Rank r a)
        , VectorSpace (Rank (r+r) a)
        )

    id = CT id zero

    (.) = dot

dot :: forall a b c t.
    (
    ) => CT t b c -> CT t a b -> CT t a c
dot ( CT
        ( f  :: Rank b t -> Rank c t)
        ( f' :: Rank b t -> Rank (b+c) t)
    )
    ( CT
        ( g  :: Rank a t -> Rank b t)
        ( g' :: Rank a t -> Rank (a+b) t)
    )
    = CT
        ( f.g :: Rank a t -> Rank c t )
        ( (f'.g) `box'` g' :: Rank a t -> Rank (a+c) t )
    where
        box' = box
            (Proxy::Proxy a)
            (Proxy::Proxy b)
            (Proxy::Proxy c)
            (Proxy::Proxy t)
-}

-------------------

data Cask a b = Cask (a -> b) (a -> a -> b)

instance Category Cask where
    type ValidCategory Cask a = ()

    id = Cask id (const id)

    (Cask f f').(Cask g g') = Cask (f.g) (\a -> (f'.g) a . g' a)
--     (Cask f f').(Cask g g') = Cask (f.g) (fmap (.) $ (f'.g) g')
--     (Cask f f').(Cask g g') = Cask (f.g) ((f'.g) <=< g')

-------------------

data C1T cat a b where
    C1T :: Rng (cat a a) => cat a a -> cat a a -> C1T cat a a

instance Category cat => Category (C1T cat) where
    type ValidCategory (C1T cat) a =
        ( ValidCategory cat a
        , Rng (cat a a)
        )

    id = C1T id id

    (C1T f f').(C1T g g') = C1T (f.g) ( (f'.g)*g )

instance Sup (C1T cat) cat cat
instance Sup cat (C1T cat) cat
instance (C1T cat <: cat) where
    embedType_ = Embed2 (\ (C1T f f') -> embedType2 f)

instance Category cat => Diff (C1T cat) where
    type DiffCat (C1T cat) = cat
    derivative (C1T _ f') = f'

---------

instance Eq (cat a a) => Eq (C1T cat a a) where
    (C1T f f')==(C1T g g') = f==g && f'==g'

instance Ring (cat a a) => Semigroup (C1T cat a a) where
    (C1T f f')+(C1T g g') = C1T (f+g) (f'+g')

instance Ring (cat a a) => Monoid (C1T cat a a) where
    zero = C1T zero zero

instance Ring (cat a a) => Cancellative (C1T cat a a) where
    (C1T f f')-(C1T g g') = C1T (f-g) (f'-g')

instance Ring (cat a a) => Group (C1T cat a a) where
    negate (C1T f f') = C1T (negate f) (negate f')

instance Ring (cat a a) => Abelian (C1T cat a a)

instance Ring (cat a a) => Rg (C1T cat a a) where
    (C1T f f')*(C1T g g') = C1T (f*g) (f'*g + f*g')

instance Ring (cat a a) => Rig (C1T cat a a) where
    one = C1T one one

---------

sin' :: C1T Hask Double Double
sin' = C1T P.sin P.cos

-- sin'' = C1T P.sin (C1T P.cos (negate P.sin) )

-------------------------------------------------------------------------------

{-
type family Rank (n::Nat) a
type instance Rank 0 (Vector r) = r
type instance Rank 1 (Vector r) = Vector r
type instance Rank 2 (Vector r) = Matrix r

class Category cat => Differentiable cat where
    type Derivative cat :: k -> k -> *

    derivative
        :: Proxy (n :: Nat)
        -> Proxy (b :: *)
        -> cat a (Rank n     b)
        -> cat a (Rank (n+1) b)

-------------------

newtype C0 a (r1::Nat) (r2::Nat) = C0 (Rank r1 a -> Rank r2 a)

instance Category (C0 a) where
    type ValidCategory (C0 a) r1 r2 = ()
    id = C0 id
    (C0 f).(C0 g) = C0 $ f.g

data C1 a (r1::Nat) (r2::Nat) = C1
    (Rank r1 a -> Rank r2 a)
    (Rank r1 a -> Rank (r2+1) a)

instance Category (C1 a) where
    type ValidCategory (C1 a) r1 r2 =
        ( Rng (Rank r1 a)
        , Rng (Rank r2 a)
        , Rng (Rank (r2+1) a)
--         , r1 ~ r2
        )

    id = C1 id zero
-}

-- tensorMult
--     :: Proxy r1
--     -> Proxy r2
--     -> Proxy r3
--     -> Proxy a
--     -> Rank r1 a
--     -> Rank r2 a
--     -> Rank r3 a
-- tensorMult _ _ _ _ m1 m2 = undefined
--
-- c1dot :: forall r1 r2 r3 a.
--     ( Rank r2 a ~ Rank (r3+1) a
--     , Rng (Rank r2 a)
--     ) => C1 a r2 r3 -> C1 a r1 r2 -> C1 a r1 r3
-- c1dot (C1 f f') (C1 g g') = C1
--     (f.g)
--     ( tensorMult
--         ( undefined :: Proxy r1 )
--         ( undefined :: Proxy r2 )
--         ( undefined :: Proxy r3 )
--         ( undefined :: Proxy a )
--         (f'.g)
--         g
--     )


{-
newtype C0 a b = C0 (a -> b)

instance Category C0 where
    type ValidCategory C0 a b = ()
    id = C0 id
    (C0 f).(C0 g) = C0 $ f.g

instance SubCategory C0 (->) where
    embed (C0 f) = f

---

data C1 a b = C1 (a -> b) (a -> a)

instance Category C1 where
    type ValidCategory C1 a b = (a ~ b, Rng a)
    id = C1 id id
    (C1 f f').(C1 g g') = C1 (f.g) ( (f'.g) * g )

instance SubCategory C1 (->) where
    embed (C1 f f') = f

-- instance Differentiable C1 where
--     type Derivative C1 a b = C0 a a
--     derivative (C1 f f') = C0 f'

---

data C2 a b = C2 (a -> b) (a -> a) (a -> Outer a)

instance Category C2 where
    type ValidCategory C2 a b = (a ~ b, Rng a, OuterProductSpace a, a ~ Outer a)
    id = C2 id id id
    (C2 f f' f'').(C2 g g' g'') = C2 (f.g) ((f'.g)*g) ((f''.g)*g'*g + (f'.g)*g*g')

instance SubCategory C2 (->) where
    embed (C2 f f' f'') = f

-- instance Differentiable C2 where
--     type Derivative C2 a b = C1 a a
--     derivative (C2 f f' f'') = C1 f' f''
-}

-------------------------------------------------------------------------------

{-
type family C (n::Nat) (cat :: * -> * -> *) :: Constraint where
    C 0 cat = ()
    C n cat = (Differentiable cat, C (n-1) cat)

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
    , SubCategory cat supercat
    ) => SubCategory (DerivativeT n cat) supercat
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

_sin :: DerivativeT 3 Hask Double Double
_sin = DerivativeT $ V.fromList [P.sin,P.cos,\x -> -P.sin x] --, \x -> -P.cos x]
-}
