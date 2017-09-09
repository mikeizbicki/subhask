{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | This module provides a category transformer for automatic differentiation.
--
-- There are many alternative notions of a generalized derivative.
-- Perhaps the most common is the differential Ring.
-- In Haskell, this might be defined as:
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

import SubHask.Algebra
import SubHask.Category
import SubHask.SubType
import SubHask.Internal.Prelude

--------------------------------------------------------------------------------
-- | This is essentially just a translation of the "Numeric.AD.Forward.Forward" type
-- for use with the SubHask numeric hierarchy.
--
-- FIXME:
--
-- Add reverse mode auto-differentiation for vectors.
-- Apply the "ProofOf" framework from Monotonic
data Forward a = Forward
    { val  :: !a
    , val' ::  a
    }
    deriving (Typeable,Show)

mkMutable [t| forall a. Forward a |]

instance Semigroup a => Semigroup (Forward a) where
    (Forward a1 a1')+(Forward a2 a2') = Forward (a1+a2) (a1'+a2')

instance Cancellative a => Cancellative (Forward a) where
    (Forward a1 a1')-(Forward a2 a2') = Forward (a1-a2) (a1'-a2')

instance Monoid a => Monoid (Forward a) where
    zero = Forward zero zero

instance Group a => Group (Forward a) where
    negate (Forward a b) = Forward (negate a) (negate b)

instance Abelian a => Abelian (Forward a)

instance Rg a => Rg (Forward a) where
    (Forward a1 a1')*(Forward a2 a2') = Forward (a1*a2) (a1*a2'+a2*a1')

instance Rig a => Rig (Forward a) where
    one = Forward one zero

instance Ring a => Ring (Forward a) where
    fromInteger x = Forward (fromInteger x) zero

instance Field a => Field (Forward a) where
    reciprocal (Forward a a') = Forward (reciprocal a) (-a'/(a*a))
    (Forward a1 a1')/(Forward a2 a2') = Forward (a1/a2) ((a1'*a2+a1*a2')/(a2'*a2'))
    fromRational r = Forward (fromRational r) 0

---------

proveC1 :: (a ~ (a><a), Rig a) => (Forward a -> Forward a) -> C1 (a -> a)
proveC1 f = Diffn (\a -> val $ f $ Forward a one) $ Diff0 $ \a -> val' $ f $ Forward a one

proveC2 :: (a ~ (a><a), Rig a) => (Forward (Forward a) -> Forward (Forward a)) -> C2 (a -> a)
proveC2 f
    = Diffn (\a -> val  $ val  $ f $ Forward (Forward a one) one)
    $ Diffn (\a -> val' $ val  $ f $ Forward (Forward a one) one)
    $ Diff0 (\a -> val' $ val' $ f $ Forward (Forward a one) one)

--------------------------------------------------------------------------------

class C (cat :: * -> * -> *) where
    type D cat :: * -> * -> *
    derivative :: cat a b -> D cat a (a >< b)

data Diff (n::Nat) a b where
    Diff0 :: (a -> b) -> Diff 0 a b
    Diffn :: (a -> b) -> Diff (n-1) a (a >< b) -> Diff n a b

---------

instance Sup (->) (Diff n) (->)
instance Sup (Diff n) (->) (->)

instance Diff 0 <: (->) where
    embedType_ = Embed2 unDiff0
        where
            unDiff0 :: Diff 0 a b -> a -> b
            unDiff0 (Diff0 f) = f
            unDiff0 (Diffn _ _) = undefined

instance Diff n <: (->) where
    embedType_ = Embed2 unDiffn
        where
            unDiffn :: Diff n a b -> a -> b
            unDiffn (Diffn f _) = f
            unDiffn (Diff0 _) = undefined

--
-- FIXME: these subtyping instance should be made more generic
-- the problem is that type families aren't currently powerful enough
--
instance Sup (Diff 0) (Diff 1) (Diff 0)
instance Sup (Diff 1) (Diff 0) (Diff 0)
instance Diff 1 <: Diff 0
    where embedType_ = Embed2 m2n
              where m2n (Diffn f _) = Diff0 f
                    m2n (Diff0 _) = undefined

instance Sup (Diff 0) (Diff 2) (Diff 0)
instance Sup (Diff 2) (Diff 0) (Diff 0)
instance Diff 2 <: Diff 0
    where embedType_ = Embed2 m2n
              where m2n (Diffn f _) = Diff0 f
                    m2n (Diff0 _) = undefined

instance Sup (Diff 1) (Diff 2) (Diff 1)
instance Sup (Diff 2) (Diff 1) (Diff 1)
instance Diff 2 <: Diff 1
    where embedType_ = Embed2 m2n
              where m2n (Diffn f f') = Diffn f (embedType2 f')
                    m2n (Diff0 _) = undefined

---------

instance (1 <= n) => C (Diff n) where
    type D (Diff n) = Diff (n-1)
    derivative (Diffn _ f') = f'
    -- doesn't work, hence no non-ehaustive pattern ghc option
    -- derivative (Diff0 _) = undefined

unsafeProveC0 :: (a -> b) -> Diff 0 a b
unsafeProveC0 f = Diff0 f

unsafeProveC1
    :: (a -> b)     -- ^ f(x)
    -> (a -> a><b)  -- ^ f'(x)
    -> C1 (a -> b)
unsafeProveC1 f f' = Diffn f $ unsafeProveC0 f'

unsafeProveC2
    :: (a -> b)         -- ^ f(x)
    -> (a -> a><b)      -- ^ f'(x)
    -> (a -> a><a><b)   -- ^ f''(x)
    -> C2 (a -> b)
unsafeProveC2 f f' f'' = Diffn f $ unsafeProveC1 f' f''

type C0 a = C0_ a
type family C0_ (f :: *) :: * where
    C0_ (a -> b) = Diff 0 a b

type C1 a = C1_ a
type family C1_ (f :: *) :: * where
    C1_ (a -> b) = Diff 1 a b

type C2 a = C2_ a
type family C2_ (f :: *) :: * where
    C2_ (a -> b) = Diff 2 a b

---------------------------------------
-- algebra

mkMutable [t| forall n a b. Diff n a b |]

instance Semigroup b => Semigroup (Diff 0 a b) where
    (Diff0 f1    )+(Diff0 f2    ) = Diff0 (f1+f2)
    _ + _ = undefined

instance (Semigroup b, Semigroup (a><b)) => Semigroup (Diff 1 a b) where
    (Diffn f1 f1')+(Diffn f2 f2') = Diffn (f1+f2) (f1'+f2')

instance (Semigroup b, Semigroup (a><b), Semigroup (a><a><b)) => Semigroup (Diff 2 a b) where
    (Diffn f1 f1')+(Diffn f2 f2') = Diffn (f1+f2) (f1'+f2')

instance Monoid b => Monoid (Diff 0 a b) where
    zero = Diff0 zero

instance (Monoid b, Monoid (a><b)) => Monoid (Diff 1 a b) where
    zero = Diffn zero zero

instance (Monoid b, Monoid (a><b), Monoid (a><a><b)) => Monoid (Diff 2 a b) where
    zero = Diffn zero zero

--------------------------------------------------------------------------------
-- test

-- v = unsafeToModule [1,2,3,4,5] :: SVector 5 Double
--
-- sphere :: Hilbert v => C0 (v -> Scalar v)
-- sphere = unsafeProveC0 f
--     where
--         f v = v<>v
