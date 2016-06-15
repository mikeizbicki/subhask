-- | Template Haskell functions for deriving "Mutable" instances.
module SubHask.TemplateHaskell.Mutable
    ( mkMutable
    , mkMutablePrimRef
    , mkMutableNewtype
    )
    where

import SubHask.TemplateHaskell.Common

import Prelude
import Control.Monad
import Language.Haskell.TH

showtype :: Type -> String
showtype t = map go (show t)
    where
        go ' ' = '_'
        go '.' = '_'
        go '[' = '_'
        go ']' = '_'
        go '(' = '_'
        go ')' = '_'
        go '/' = '_'
        go '+' = '_'
        go '>' = '_'
        go '<' = '_'
        go x   = x

type2name :: Type -> Name
type2name t = mkName $ "Mutable_"++showtype t

-- | Inspects the given type and creates the most efficient "Mutable" instance possible.
--
-- FIXME: implement properly
mkMutable :: Q Type -> Q [Dec]
mkMutable = mkMutablePrimRef


-- | Create a "Mutable" instance for newtype wrappers.
-- The instance has the form:
--
-- > newtype instance Mutable m (TyCon t) = Mutable_TyCon (Mutable m t)
--
-- Also create the appropriate "IsMutable" instance.
--
-- FIXME:
-- Currently uses default implementations which are slow.
mkMutableNewtype :: Name -> Q [Dec]
mkMutableNewtype typename = do
    typeinfo <- reify typename
    (conname,typekind,typeapp) <- case typeinfo of
        TyConI (NewtypeD [] _ typekind _ (NormalC conname [(  _,typeapp)]) _)
            -> return (conname,typekind,typeapp)
        TyConI (NewtypeD [] _ typekind _ (RecC    conname [(_,_,typeapp)]) _)
            -> return (conname,typekind,typeapp)
        _ -> error $ "\nderiveSingleInstance; typeinfo="++show typeinfo

    let mutname = mkName $ "Mutable_" ++ nameBase conname

    nameexists <- lookupValueName (show mutname)
    return $ case nameexists of
        Just x -> []
        Nothing ->
            [ NewtypeInstD
                [ ]
                ( mkName $ "Mutable" )
                [ VarT (mkName "m"), apply2varlist (ConT typename) typekind ]
                Nothing
                ( NormalC
                    mutname
                    [( Bang NoSourceUnpackedness NoSourceStrictness
                     , AppT
                        ( AppT
                            ( ConT $ mkName "Mutable" )
                            ( VarT $ mkName "m" )
                        )
                        typeapp
                     )]
                )
                [ ]
            , InstanceD
                Nothing
                ( map (\x -> AppT (ConT $ mkName "IsMutable") (bndr2type x)) $ filter isStar $ typekind )
                ( AppT
                    ( ConT $ mkName "IsMutable" )
                    ( apply2varlist (ConT typename) typekind )
                )
                [ FunD (mkName "freeze")
                    [ Clause
                        [ ConP mutname [ VarP $ mkName "x" ] ]
                        ( NormalB $ AppE
                            ( AppE (VarE $ mkName "helper_liftM") (ConE conname) )
                            ( AppE (VarE $ mkName "freeze") (VarE $ mkName "x") )
                        )
                        []
                    ]
                , FunD (mkName "thaw")
                    [ Clause
                        [ ConP conname [ VarP $ mkName "x" ] ]
                        ( NormalB $ AppE
                            ( AppE (VarE $ mkName "helper_liftM") (ConE mutname) )
                            ( AppE (VarE $ mkName "thaw") (VarE $ mkName "x") )
                        )
                        []
                    ]
                , FunD (mkName "write")
                    [ Clause
                        [ ConP mutname [ VarP $ mkName "x" ]
                        , ConP conname [ VarP $ mkName "x'" ]
                        ]
                        ( NormalB $
                            AppE ( AppE (VarE $ mkName "write") (VarE $ mkName "x") ) (VarE $ mkName "x'" )
                        )
                        []
                    ]
                ]
            ]

-- | Create a "Mutable" instance that uses "PrimRef"s for the underlying implementation.
-- This method will succeed for all types.
-- But certain types can be implemented for efficiently.
mkMutablePrimRef :: Q Type -> Q [Dec]
mkMutablePrimRef qt = do
    _t <- qt
    let (cxt,t) = case _t of
            (ForallT _ cxt t) -> (cxt,t)
            _                 -> ([],_t)

    return $
        [ NewtypeInstD
            cxt
            ( mkName $ "Mutable" )
            [ VarT (mkName "m"), t ]
            Nothing
            ( NormalC
                ( type2name t )
                [( Bang NoSourceUnpackedness NoSourceStrictness
                 , AppT (AppT (ConT $ mkName "PrimRef") (VarT $ mkName "m")) t
                 )]
            )
            [ ]
        , InstanceD
            Nothing
            cxt
            ( AppT ( ConT $ mkName "IsMutable" ) t )
            [ FunD (mkName "freeze")
                [ Clause
                    [ ConP (type2name t) [ VarP $ mkName "x"] ]
                    ( NormalB $ AppE (VarE $ mkName "readPrimRef") (VarE $ mkName "x"))
                    []
                ]
            , FunD (mkName "thaw")
                [ Clause
                    [ VarP $ mkName "x" ]
                    ( NormalB $ AppE
                        ( AppE (VarE $ mkName "helper_liftM") (ConE $ type2name t) )
                        ( AppE (VarE $ mkName "newPrimRef") (VarE $ mkName "x") )
                    )
                    []
                ]
            , FunD (mkName "write")
                [ Clause
                    [ ConP (type2name t) [VarP $ mkName "x"], VarP $ mkName "x'" ]
                    ( NormalB $ AppE
                        ( AppE (VarE $ mkName "writePrimRef") (VarE $ mkName "x") )
                        ( VarE $ mkName "x'" )
                    )
                    []
                ]
            ]
        ]

