{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- |
--
-- FIXME: doesn't handle multiparameter classes like Integral and Vector
--
-- FIXME: should this be separated out into another lib when finished?
module SubHask.TemplateHaskell.Deriving
    (
    -- * template haskell functions
    deriveHierarchy
    , deriveHierarchyFiltered
    , deriveSingleInstance
    , deriveTypefamilies
    , mkMutableNewtype
    , listSuperClasses

    -- ** compatibility functions
    , fromPreludeEq

    -- ** helpers
    , BasicType
    , helper_liftM
    , helper_id

    -- ** misc
    , substituteNewtype
    )
    where

import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Common
import SubHask.TemplateHaskell.Mutable
import Prelude
import Data.List (nub)

import Language.Haskell.TH.Syntax
import Control.Monad

-- | This class provides an artificial hierarchy that defines all the classes that a "well behaved" data type should implement.
-- All newtypes will derive them automatically.
type BasicType t = (Show t, Read t, Arbitrary t, NFData t)

-- | We need to export this function for deriving of Monadic functions to work
helper_liftM :: Monad m => (a -> b) -> m a -> m b
helper_liftM = liftM

helper_id :: a -> a
helper_id x = x

-- | List all the superclasses of a one parameter class.
-- This does not include:
--   * constraints involving types other than the parameter (e.g. made with type families).
--   * type synonyms (although these will get substituted in the recursion)
--
-- For example, convert ''Group into [''Semigroup, ''Monoid, ''Cancellative, ''Group]
listSuperClasses :: Name -> Q [Name]
listSuperClasses className = do
    info <- reify className
    case info of

        ClassI (ClassD ctx _ bndrs _ _) _ ->
            liftM (className:) $ liftM concat $ mapM (go $ bndrs2var bndrs) ctx

        TyConI (TySynD _ bndrs t) ->
            liftM concat $ mapM (go $ bndrs2var bndrs) $ tuple2list t

        info' -> error $ "type "++nameBase className++" not a unary class\n\ninfo="++show info'

    where
        bndrs2var bndrs = case bndrs of
            [PlainTV var       ] -> var
            [KindedTV var StarT] -> var

        go var (AppT (ConT name) (VarT var')) = if var==var'
            then listSuperClasses name
            else return [] -- class depends on another type tested elsewhere
        go _ _ = return []

tuple2list :: Type -> [Type]
tuple2list (AppT (AppT (TupleT 2) t1) t2) = [t1,t2]
tuple2list (AppT (AppT (AppT (TupleT 3) t1) t2) t3) = [t1,t2,t3]
tuple2list (AppT (AppT (AppT (AppT (TupleT 4) t1) t2) t3) t4) = [t1,t2,t3,t4]
tuple2list (AppT (AppT (AppT (AppT (AppT (TupleT 5) t1) t2) t3) t4) t5) = [t1,t2,t3,t4,t5]
tuple2list t = [t]

-- | creates the instance:
--
-- > type instance Scalar (Newtype s) = Scalar s
--
deriveTypefamilies :: [Name] -> Name -> Q [Dec]
deriveTypefamilies familynameL typename = do
    info <- reify typename
    let (tyvarbndr,tyvar) = case info of
            TyConI (NewtypeD _ _ xs _ (NormalC _ [(  _,t)]) _) -> (xs,t)
            TyConI (NewtypeD _ _ xs _ (RecC    _ [(_,_,t)]) _) -> (xs,t)
    return $ map (go tyvarbndr tyvar) familynameL
    where
        go tyvarbndr tyvar familyname = TySynInstD familyname $ TySynEqn
            [ apply2varlist (ConT typename) tyvarbndr ]
            ( AppT (ConT familyname) tyvar )

-- | This is the main TH function to call when deriving classes for a newtype.
-- You only need to list the final classes in the hierarchy that are supposed to be derived.
-- All the intermediate classes will be derived automatically.
deriveHierarchy :: Name -> [Name] -> Q [Dec]
deriveHierarchy typename classnameL = deriveHierarchyFiltered typename classnameL []

-- | Like "deriveHierarchy" except classes in the second list will not be derived.
deriveHierarchyFiltered :: Name -> [Name] -> [Name] -> Q [Dec]
deriveHierarchyFiltered typename classnameL filterL = do
    classL <- liftM concat $ mapM listSuperClasses $ mkName "BasicType":classnameL
    instanceL <- mapM (deriveSingleInstance typename) $ filter (\x -> not (elem x filterL)) $ nub classL
    mutableL <- mkMutableNewtype typename
    return $ mutableL ++ concat instanceL

-- | Given a single newtype and single class, constructs newtype instances
deriveSingleInstance :: Name -> Name -> Q [Dec]
deriveSingleInstance typename classname = if show classname == "SubHask.Mutable.IsMutable"
    then return [] -- this special case is handled by mkMutableNewtype
    else do
        typeinfo <- reify typename
        (conname,typekind,typeapp) <- case typeinfo of
            TyConI (NewtypeD [] _ typekind _ (NormalC conname [(  _,typeapp)]) _)
                -> return (conname,typekind,typeapp)

            TyConI (NewtypeD [] _ typekind _ (RecC    conname [(_,_,typeapp)]) _)
                -> return (conname,typekind,typeapp)

            _ -> error $ "\nderiveSingleInstance; typeinfo="++show typeinfo

        typefamilies <- deriveTypefamilies
            [ mkName "Scalar"
            , mkName "Elem"
    --         , mkName "Index"
            , mkName "Logic"
            , mkName "Actor"
            ] typename

        classinfo <- reify classname
        liftM ( typefamilies++ ) $ case classinfo of

            -- if the class has exactly one instance that applies to everything,
            -- then don't create an overlapping instance
            -- These classes only exist because TH has problems with type families
            -- FIXME: this is probably not a robust solution
            ClassI (ClassD _ _ _ _ _) [InstanceD _ _ (VarT _) _] -> return []
            ClassI (ClassD _ _ _ _ _) [InstanceD _ _ (AppT (ConT _) (VarT _)) _] -> return []

            -- otherwise, create the instance
            ClassI (ClassD ctx _ [bndr] [] decs) _ -> do
                let varname = case bndr of
                        PlainTV v -> v
                        KindedTV v StarT -> v

                alreadyInstance <- isNewtypeInstance typename classname
                if alreadyInstance
                    then return []
                    else do
                        let notDefaultSigD (DefaultSigD _ _) = False
                            notDefaultSigD _ = True

                        funcL <- forM (filter notDefaultSigD decs) $ \dec -> do
                            let (f,sigtype) = case dec of
                                    SigD f_ sigtype_ -> (f_,sigtype_)
                                    DefaultSigD f_ sigtype_ -> (f_,sigtype_)
                            body <- returnType2newtypeApplicator conname varname
                                (last (arrow2list sigtype))
                                (list2exp $ (VarE f):(typeL2expL $ init $ arrow2list sigtype ))

                            return
                                [ FunD f $
                                    [ Clause
                                        ( typeL2patL conname varname $ init $ arrow2list sigtype )
                                        ( NormalB body )
                                        []
                                    ]
                                , PragmaD $ InlineP f Inline FunLike AllPhases
                                ]

                        return
                            [ InstanceD
                                Nothing
                                ( AppT (ConT classname) typeapp : map (subVarT varname typeapp) ctx )
                                ( AppT (ConT classname) $ apply2varlist (ConT typename) typekind )
                                ( concat funcL )
                             ]

expandTySyn :: Type -> Q Type
expandTySyn (AppT (ConT tysyn) vartype) = do
    info <- reify tysyn
    case info of
        TyConI (TySynD _ [PlainTV var] syntype) ->
            return $ subVarT var vartype syntype

        TyConI (TySynD _ [KindedTV var StarT] syntype) ->
            return $ subVarT var vartype syntype

        qqq -> error $ "expandTySyn: qqq="++show qqq

subVarT :: Name -> Type -> Type -> Type
subVarT varname vartype t = go t
    where
        go (VarT e) = if e==varname
            then vartype
            else VarT e
        go (ConT e) = ConT e
        go (AppT e1 e2) = AppT (go e1) (go e2)
        go ArrowT = ArrowT
        go ListT = ListT
        go (TupleT n) = TupleT n
        go zzz = error $ "subVarT: zzz="++show zzz

returnType2newtypeApplicator :: Name -> Name -> Type -> Exp -> Q Exp
returnType2newtypeApplicator conname varname t exp' = do
    ret <- go t
    return $ AppE ret exp'

    where

        id' = return $ VarE $ mkName "helper_id"

        go (VarT v) = if v==varname
            then return $ ConE conname
            else id'
        go (ConT _) = id'

        -- | FIXME: The cases below do not cover all the possible functions we might want to derive
        go (TupleT 0) = id'
        go (AppT (ConT c) t2) = do
            info <- reify c
            case info of
                TyConI (TySynD _ _ _) -> expandTySyn t >>= go
                FamilyI (OpenTypeFamilyD _) _ -> id'
                TyConI (NewtypeD _ _ _ _ _ _) -> liftM (AppE (VarE $ mkName "helper_liftM")) $ go t2
                TyConI (DataD _ _ _ _ _ _) -> liftM (AppE (VarE $ mkName "helper_liftM")) $ go t2
                qqq -> error $ "returnType2newtypeApplicator: qqq="++show qqq

        go (AppT ListT t2) = liftM (AppE (VarE $ mkName "helper_liftM")) $ go t2
        go (AppT (AppT ArrowT _) t2) = liftM (AppE (VarE $ mkName "helper_liftM")) $ go t2
        go (AppT (AppT (TupleT 2) t1) t2) = do
            e1 <- go t1
            e2 <- go t2
            return $ LamE
                [ TupP [VarP $ mkName "v1", VarP $ mkName "v2"] ]
                ( TupE
                    [ AppE e1 (VarE $ mkName "v1")
                    , AppE e2 (VarE $ mkName "v2")
                    ]
                )

        -- FIXME: this is a particularly fragile deriving clause only designed for the mutable operators
        go (AppT (VarT _) (TupleT 0)) = id'

        go xxx = error $ "returnType2newtypeApplicator:\n xxx="++show xxx++"\n t="++show t++"\n exp="++ show exp'

isNewtypeInstance :: Name -> Name -> Q Bool
isNewtypeInstance typename classname = do
    info <- reify classname
    case info of
        ClassI _ inst -> return $ or $ map go inst
    where
        go (InstanceD _ _ (AppT _ (AppT (ConT n) _)) _) = n==typename
        go _ = False


substituteNewtype :: Name -> Name -> Name -> Type -> Type
substituteNewtype conname varname _ = go
    where
        go (VarT v) = if varname==v
            then AppT (ConT conname) (VarT varname)
            else VarT v
        go (AppT t1 t2) =  AppT (go t1) (go t2)
        go (ConT t) = ConT t

typeL2patL :: Name -> Name -> [Type] -> [Pat]
typeL2patL conname varname xs = map go $ zip (map (\a -> mkName [a]) ['a'..]) xs
    where
        go :: (Name, Type) -> Pat
        go (newvar,VarT v) = if v==varname
            then ConP conname [VarP newvar]
            else VarP newvar
        go (newvar,AppT (AppT (ConT c) _) _) = if nameBase c=="Mutable"
            then ConP (mkName $ "Mutable_"++nameBase conname) [VarP newvar]
            else VarP newvar
        go (newvar,AppT (ConT _) (VarT _)) = VarP newvar
        go (newvar,AppT ListT (VarT _)) = VarP newvar
        go (newvar,AppT ListT (AppT (ConT _) (VarT _))) = VarP newvar
        go (newvar,ConT _) = VarP newvar
        go (newvar,_) = VarP newvar

typeL2expL :: [Type] -> [Exp]
typeL2expL xs = map fst $ zip (map (\a -> VarE $ mkName [a]) ['a'..]) xs

arrow2list :: Type -> [Type]
arrow2list (ForallT _ _ xs) = arrow2list xs
arrow2list (AppT (AppT ArrowT t1) t2) = t1:arrow2list t2
arrow2list x = [x]

list2exp :: [Exp] -> Exp
list2exp xs = go $ reverse xs
    where
        go (x:[]) = x
        go (x:xs') = AppE (go xs') x

-- | Generate an Eq_ instance from the Prelude's Eq instance.
-- This requires that Logic t = Bool, so we also generate this type instance.
fromPreludeEq :: Q Type -> Q [Dec]
fromPreludeEq qt = do
    t<-qt
    return
        [ TySynInstD
            ( mkName "Logic" )
            ( TySynEqn [t] (ConT $ mkName "Bool" ))
        , InstanceD
            Nothing
            []
            ( AppT ( ConT $ mkName "Eq_" ) t )
            [ FunD
                ( mkName "==" )
                [ Clause [] (NormalB $ VarE $ mkName "P.==") [] ]
            ]
        ]
