module SubHask.SubType
    where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import SubHask.Internal.Prelude
import Prelude (($),id,Eq (..))

import Debug.Trace

-------------------------------------------------------------------------------

-- | Subtypes are partially ordered.
-- Unfortunately, there's no way to use the machinery of the "POrd"/"Lattice" classes.
-- The "Sup" type family is a promotion of the "sup" function to the type level.
--
-- It must obey the laws:
--
-- > Sup a b c   <===>   ( a <: c, b <: c )
--
-- > Sub a b c   <===>   Sup b a c
--
-- And there is no smaller value of "c" that can be used instead.
--
-- FIXME: it would be nicer if this were a type family; is that possible?
class Sup a b c | a b -> c

instance Sup a a a

-- | We use `a <: b` to denote that a is a subtype of b.
-- The "embedType" function must be a homomorphism from a to b.
class (Sup a b b, Sup b a b) => a <: b where
    embedType :: a -> b

-- | FIXME: can these laws be simplified at all?
-- In particular, can we automatically infer ctx from just the function parameter?
law_Subtype_f1 ::
    ( a <: b
    , Eq b
    , ctx a
    , ctx b
    ) => proxy ctx  -- ^ this parameter is only for type inference
      -> b          -- ^ this parameter is only for type inference
      -> (forall c. ctx c => c -> c)
      -> a
      -> Bool
law_Subtype_f1 _ b f a = embedType (f a) == f (embedType a) `asTypeOf` b

law_Subtype_f2 ::
    ( a <: b
    , Eq b
    , ctx a
    , ctx b
    ) => proxy ctx  -- ^ this parameter is only for type inference
      -> b          -- ^ this parameter is only for type inference
      -> (forall c. ctx c => c -> c -> c)
      -> a
      -> a
      -> Bool
law_Subtype_f2 _ b f a1 a2 = embedType (f a1 a2) == f (embedType a1) (embedType a2) `asTypeOf` b

instance a <: a where
    embedType = id

-------------------

type family a == b :: Bool where
    a == a = True
    a == b = False

type family If (a::Bool) (b::k) (c::k) :: k where
    If True  b c = b
    If False b c = c

type family When (a::Bool) (b::Constraint) :: Constraint where
    When True  b = b
    When False b = ()

-------------------

instance Sup Double Int Double
instance Sup Int Double Double

apEmbedType1 ::
    ( a1 <: b1
    ) => (b1 -> c) -> a1 -> c
apEmbedType1 f a = f (embedType a)

apEmbedType2 ::
    ( a1 <: b1
    , a2 <: b2
    , When (b1==b2) (Sup a1 a2 b1)
    ) => (b1 -> b2 -> c)
      -> (a1 -> a2 -> c)
apEmbedType2 f a b = f (embedType a) (embedType b)

--------------------------------------------------------------------------------
-- template haskell
-- FIXME: move this into the template haskell folder?

-- |
--
-- FIXME: This should automatically search for other subtypes that can be inferred from t1 and t2
--
mkSubtype :: Q Type -> Q Type -> Name -> Q [Dec]
mkSubtype qt1 qt2 f = do
    t1 <- liftM stripForall qt1
    t2 <- liftM stripForall qt2
    return $ mkSubtypeInstance t1 t2 f:mkSup t1 t2 t2

-- | converts types created by `[t| ... |]` into a more useful form
stripForall :: Type -> Type
stripForall (ForallT _ _ t) = stripForall t
stripForall (VarT t) = VarT $ mkName $ nameBase t
stripForall (ConT t) = ConT t
stripForall (AppT t1 t2) = AppT (stripForall t1) (stripForall t2)

-- | Calling:
--
-- > mkSubtypeInstance a b f
--
-- generates the following code:
--
-- > instance a <: b where
-- >    embedType = f
--
mkSubtypeInstance :: Type -> Type -> Name -> Dec
mkSubtypeInstance t1 t2 f = InstanceD
    []
    ( AppT
        ( AppT
            ( ConT $ mkName "<:" )
            t1
        )
        t2
    )
    [ FunD
        ( mkName "embedType" )
        [ Clause
            []
            ( NormalB $ VarE f )
            []
        ]
    ]

-- | Calling:
--
-- > mkSup a b c
--
-- generates the following code:
--
-- > instance Sup a b c
-- > instance Sup b a c
--
mkSup :: Type -> Type -> Type -> [Dec]
mkSup t1 t2 t3 =
    [ InstanceD [] (AppT (AppT (AppT (ConT $ mkName "Sup") t1) t2) t3) []
    , InstanceD [] (AppT (AppT (AppT (ConT $ mkName "Sup") t2) t1) t3) []
    ]

-- | This is a not-quite quasiquoter for using subtypes.
-- It requires the TemplateHaskell extension, but not the QuasiQuoter extension.
-- Use it like:
--
-- > var = $(sub[| 1+2*4 |])
--
sub :: Q Exp -> Q Exp
sub qe = do
    e <- qe
    trace ("\nsubtype substitution\n  orig="++show e++"\n") $ return ()
    return $ subExp e

-- | This is a helper for "sub" that performs the actual substitution.
--
-- FIXME: There's lots of missing cases right now!
--
subExp :: Exp -> Exp
subExp (InfixE (Just a1) f (Just a2)) = subExp $ AppE (AppE f a1) a2
subExp (UInfixE a1 f a2) = subExp $ AppE (AppE f a1) a2
subExp (AppE (AppE f a1) a2) = AppE (AppE (AppE (VarE $ mkName "apEmbedType2") f) a1) a2
subExp xs = xs
