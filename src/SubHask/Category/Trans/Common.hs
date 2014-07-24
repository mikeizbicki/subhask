module SubHask.Category.Trans.Common
    where

import Prelude 
import Language.Haskell.TH.Syntax

-- import SubHask.Internal.Prelude
-- import SubHask.Category

mkCatTrans :: Name -> Name -> Q [Dec]
mkCatTrans cat constraint = do
    catinst <- deriveCategory cat constraint
    subcatinst <- deriveSubCategory cat
    return $ catinst ++ subcatinst

-- | Given a transformer name, construct a "SubCategory" insance of the form:
--
-- > instance SubCategory supercat cat => SubCategory supercat (MonT cat) where
-- >     embed (MonT f) = embed f
deriveSubCategory :: Name -> Q [Dec]
deriveSubCategory cat = return
    [ InstanceD
        [ ClassP 
            (mkName "SubCategory")
            [ VarT $ mkName "supercat" 
            , VarT $ mkName "cat"
            ]
        ]
        ( AppT
            ( AppT
                ( ConT $ mkName "SubCategory" )
                ( VarT $ mkName "supercat" )
            )
            ( AppT
                ( ConT cat )
                ( VarT $ mkName "cat" )
            )
        )
        [ FunD
            ( mkName "embed" )
            [ Clause
                [ ConP constructor [ VarP $ mkName "f" ] ]
                ( NormalB
                    ( AppE
                        ( VarE $ mkName "embed" )
                        ( VarE $ mkName "f" )
                    )
                )
                [ ]
            ]
        ]
    ]
    where
        constructor = mkName $ nameBase cat

-- | Given a transformer name (e.g. @MonT@), construct a "Category" instance of the
-- following generic form:
--
-- > instance Category cat => Category (MonT cat) where
-- >     type ValidCategory (MonT cat) a b = (Monoid a, Monoid b, ValidCategory cat a b)
-- >     id = MonT id
-- >     (MonT f).(MonT g) = MonT (f.g)
--
-- Note: assumes the value and type constructors of the cat have the same name.
deriveCategory :: Name -> Name -> Q [Dec]
deriveCategory cat constraint = return
    [ InstanceD 
        [ ClassP (mkName "Category") [VarT $ mkName "cat"] ]
        ( AppT
            ( ConT $ mkName "Category" )
            ( AppT ( ConT cat ) ( VarT $ mkName "cat" ) )
        )
        [ TySynInstD
            ( mkName "ValidCategory" )
            ( TySynEqn
                [ AppT ( ConT cat ) ( VarT $ mkName "cat" )
                , VarT $ mkName "a"
                , VarT $ mkName "b"
                ]
                ( AppT
                    ( AppT
                        ( AppT
                            ( TupleT 3 )
                            ( AppT ( ConT constraint ) ( VarT $ mkName "a" ) )
                        )
                        ( AppT ( ConT constraint ) ( VarT $ mkName "b" ) )
                    )
                    ( AppT
                        ( AppT 
                            ( AppT
                                ( ConT $ mkName $ "ValidCategory" )
                                ( VarT $ mkName "cat" )
                            )
                            ( VarT $ mkName "a" )
                        )
                        ( VarT $ mkName "b" )
                    )
                )
            )
        , FunD
            ( mkName "id" )
            [ Clause
                [ ]
                ( NormalB
                    ( AppE
                        ( ConE constructor )
                        ( VarE $ mkName "id" )
                    )
                )
                [ ]
            ]
        , FunD
            ( mkName "." )
            [ Clause
                [ ConP constructor [ VarP $ mkName "f" ]
                , ConP constructor [ VarP $ mkName "g" ]
                ]
                ( NormalB
                    ( AppE
                        ( ConE constructor )
                        ( AppE
                            ( AppE
                                ( VarE $ mkName "." )
                                ( VarE $ mkName "f" )
                            )
                            ( VarE $ mkName "g" )
                        )
                    )
                )
                [ ]
            ]
        ]
    ]  
    where
        constructor = mkName $ nameBase cat
    
