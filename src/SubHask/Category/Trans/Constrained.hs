module SubHask.Category.Trans.Constrained
    ( ConstrainedT(..)
    , proveConstrained

    -- ** Common type synonyms
    , EqHask
    , proveEqHask

    , OrdHask
    , proveOrdHask
    )
    where

import GHC.Prim

import SubHask.Algebra
import SubHask.Category
import SubHask.SubType
import SubHask.Internal.Prelude

type EqHask  = ConstrainedT '[Eq_ ] Hask
type OrdHask = ConstrainedT '[Ord_] Hask

type family AppConstraints (f :: [* -> Constraint]) (a :: *) :: Constraint
type instance AppConstraints '[] a = (ClassicalLogic a)
type instance AppConstraints (x ': xs) a = (x a, AppConstraints xs a)

data ConstrainedT (xs :: [* -> Constraint]) cat (a :: *) (b :: *) where
    ConstrainedT ::
        ( AppConstraints xs a
        , AppConstraints xs b
        , ValidCategory cat a
        , ValidCategory cat b
        ) => cat a b -> ConstrainedT xs cat a b

proveConstrained ::
    ( ValidCategory (ConstrainedT xs cat) a
    , ValidCategory (ConstrainedT xs cat) b
    ) => cat a b -> ConstrainedT xs cat a b
proveConstrained = ConstrainedT

proveEqHask :: (Eq a, Eq b) => (a -> b) -> (a `EqHask` b)
proveEqHask = proveConstrained

proveOrdHask :: (Ord a, Ord b) => (a -> b) -> (a `OrdHask` b)
proveOrdHask = proveConstrained

instance Category cat => Category (ConstrainedT xs cat) where

    type ValidCategory (ConstrainedT xs cat) (a :: *) =
        ( AppConstraints xs a
        , ValidCategory cat a
        )

    id = ConstrainedT id

    (ConstrainedT f).(ConstrainedT g) = ConstrainedT (f.g)

instance Sup a b c => Sup (ConstrainedT xs a) b c
instance Sup b a c => Sup a (ConstrainedT xs b) c
instance (subcat <: cat) => ConstrainedT xs subcat <: cat where
    embedType_ = Embed2 (\ (ConstrainedT f) -> embedType2 f)

instance (AppConstraints xs (TUnit cat), Monoidal cat) => Monoidal (ConstrainedT xs cat) where
    type Tensor (ConstrainedT xs cat) = Tensor cat
    tensor = error "FIXME: need to add a Hask Functor instance for this to work"

    type TUnit (ConstrainedT xs cat) = TUnit cat
    tunit _ = tunit (Proxy::Proxy cat)
