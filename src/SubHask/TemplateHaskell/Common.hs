module SubHask.TemplateHaskell.Common
    where

import Prelude
import Language.Haskell.TH.Syntax

bndr2type :: TyVarBndr -> Type
bndr2type (PlainTV n) = VarT n
bndr2type (KindedTV n _) = VarT n

isStar :: TyVarBndr -> Bool
isStar (PlainTV _) = True
isStar (KindedTV _ StarT) = True
isStar _ = False

apply2varlist :: Type -> [TyVarBndr] -> Type
apply2varlist contype xs = go $ reverse xs
    where
        go (x:[]) = AppT contype (mkVar x)
        go (x:xs') = AppT (go xs') (mkVar x)
        go [] = undefined

        mkVar (PlainTV n) = VarT n
        mkVar (KindedTV n _) = VarT n

