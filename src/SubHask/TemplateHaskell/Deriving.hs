-- |
--
-- FIXME: doesn't handle multiparameter classes like Integral and Vector
--
-- FIXME: this should be separated out into another lib when finished
module SubHask.TemplateHaskell.Deriving
    (
    -- * template haskell functions
    deriveHierarchy
    , deriveSingleInstance
    , deriveTypefamilies
    , listSuperClasses

    -- ** helpers
    , BasicType
    , helper_liftM
    , helper_id
    )
    where

import Prelude (zip,reverse,ReadS)
import Data.List (init,last,nub)

import SubHask.Internal.Prelude
import SubHask.Category
import SubHask.Algebra

import Language.Haskell.TH.Syntax
import Control.Monad
import Debug.Trace


-- | This class provides an artificial hierarchy that defines all the classes that a "well behaved" data type should implement.
-- All newtypes will derive them automatically.
class
    ( Show t
    , Read t
    , Arbitrary t
    , NFData t
    ) => BasicType t

instance
    ( Show t
    , Read t
    , Arbitrary t
    , NFData t
    ) => BasicType t

-- | We need to export this function for deriving of Monadic functions to work
helper_liftM :: Monad m => (a -> b) -> m a -> m b
helper_liftM = liftM

helper_id :: a -> a
helper_id x = x

-- | Convert ''Group into [''Semigroup, ''Monoid, ''Cancellative, ''Group]
listSuperClasses :: Name -> Q [Name]
listSuperClasses className = do
    info <- reify className
    case info of
        ClassI (ClassD ctx _ [PlainTV var] _ _) _ -> liftM (className:) $ liftM concat $ mapM (go var) ctx
        _ -> error $ "class "++nameBase className++" not a unary class"
    where
        go var (ClassP name [VarT var']) = if var==var'
            then listSuperClasses name
            else return [] -- ^ class depends on another type tested elsewhere
        go var _ = return []

-- | creates the instance:
--
-- > type instance Scalar (Newtype s) = Scalar s
--
deriveTypefamilies :: [Name] -> Name -> Q [Dec]
deriveTypefamilies familynameL typename = do
    info <- reify typename
    let (tyvarbndr,tyvar) = case info of
            TyConI (NewtypeD _ _ xs (NormalC _ [(  _,t)]) _) -> (xs,t)
            TyConI (NewtypeD _ _ xs (RecC    _ [(_,_,t)]) _) -> (xs,t)
    return $ map (go tyvarbndr tyvar) familynameL
    where
        go tyvarbndr tyvar familyname = TySynInstD familyname $ TySynEqn
            [ apply2varlist (ConT typename) tyvarbndr ]
            ( AppT (ConT familyname) tyvar )


-- | This is the main TH function to call when deriving classes for a newtype.
-- You only need to list the final classes in the hierarchy that are supposed to be derived.
-- All the intermediate classes will be derived automatically.
deriveHierarchy :: Name -> [Name] -> Q [Dec]
deriveHierarchy typename classnameL = do
    classL <- liftM concat $ mapM listSuperClasses $ mkName "BasicType":classnameL
    instanceL <- mapM (deriveSingleInstance typename) $ nub classL
    return $ concat instanceL

-- | Given a single newtype and single class, constructs newtype instances
deriveSingleInstance :: Name -> Name -> Q [Dec]
deriveSingleInstance typename classname = do
    typeinfo <- reify typename
    (conname,typekind,typeapp) <- case typeinfo of
        TyConI (NewtypeD [] _ typekind (NormalC conname [(  _,typeapp)]) _) -> return (conname,typekind,typeapp)
        TyConI (NewtypeD [] _ typekind (RecC    conname [(_,_,typeapp)]) _) -> return (conname,typekind,typeapp)
        _ -> error $ "\nderiveSingleInstance; typeinfo="++show typeinfo

    typefamilies <- deriveTypefamilies
        [ mkName "Scalar"
        , mkName "Elem"
        , mkName "Index"
        ] typename

    classinfo <- reify classname
    liftM ( typefamilies++ ) $ case classinfo of

        -- | if the class has exactly one instance that applies to everything,
        -- then don't create an overlapping instance
        -- These classes only exist because TH has problems with type families
        -- FIXME: this is probably not a robust solution
        ClassI (ClassD _ _ _ _ _) [InstanceD _ (VarT _) _] -> return []
        ClassI (ClassD _ _ _ _ _) [InstanceD _ (AppT (ConT _) (VarT _)) _] -> return []

        -- | otherwise, create the instance
        ClassI classd@(ClassD ctx classname [PlainTV varname] [] decs) _ -> do
--           trace ("\nconname="++show conname) $ return ()
--           trace ("typekind="++show typekind) $ return ()
--           trace ("typeapp="++show typeapp) $ return ()
--           trace ("\nclassd="++show classd) $ return ()
            alreadyInstance <- isNewtypeInstance typename classname
            if alreadyInstance
                then return []
                else do
--                     trace ("aaa="++show (apply2varlist (ConT typename) typekind)) $ return ()
                    funcL <- mapM subNewtype decs
                    return
                        [ InstanceD
                            ( ClassP classname [typeapp] : map (substitutePat varname typeapp) ctx )
                            ( AppT (ConT classname) $ apply2varlist (ConT typename) typekind )
--                             ( AppT (ConT classname) (AppT (ConT typename) typeapp ))
--                             ( ClassP classname [VarT varname] : ctx )
--                             ( AppT (ConT classname) (AppT (ConT typename) (VarT varname) ))
                            funcL
                        ]
            where

                subNewtype (SigD f sigtype) = {-trace ("\n\n\nfunction="++show (nameBase f)) $ -} do
                    body <- returnType2newtypeApplicator conname varname
                        (last (arrow2list sigtype))
                        (list2exp $ (VarE f):(typeL2expL $ init $ arrow2list sigtype ))

                    return $ FunD f $
                        [ Clause
                            ( typeL2patL conname varname $ init $ arrow2list sigtype )
                            ( NormalB body )
                            []
                        ]

apply2varlist :: Type -> [TyVarBndr] -> Type
apply2varlist contype xs = go $ reverse xs
    where
        go (x:[]) = AppT contype (mkVar x)
        go (x:xs) = AppT (go xs) (mkVar x)

        mkVar (PlainTV n) = VarT n
        mkVar (KindedTV n _) = VarT n

expandTySyn :: Type -> Q Type
expandTySyn (AppT (ConT tysyn) vartype) = do
    info <- reify tysyn
    case info of
        TyConI (TySynD _ [PlainTV var] syntype) ->
            return $ substituteVarE var vartype syntype

        qqq -> error $ "expandTySyn: qqq="++show qqq

substitutePat :: Name -> Type -> Pred -> Pred
substitutePat n t (ClassP classname xs) = ClassP classname $ map (substituteVarE n t) xs
substitutePat n t (EqualP t1 t2) = EqualP (substituteVarE n t t1) (substituteVarE n t t2)

substituteVarE :: Name -> Type -> Type -> Type
substituteVarE varname vartype = go
    where
        go (VarT e) = if e==varname
            then vartype
            else VarT e
        go (ConT e) = ConT e
        go (AppT e1 e2) = AppT (go e1) (go e2)
        go ArrowT = ArrowT
        go ListT = ListT
        go (TupleT n) = TupleT n
        go zzz = error $ "substituteVarE: zzz="++show zzz

returnType2newtypeApplicator :: Name -> Name -> Type -> Exp -> Q Exp
returnType2newtypeApplicator conname varname t exp = do
    ret <- go t
--     trace ("\nt="++show t++"\nret="++show ret) $
    return $ AppE ret exp

    where

        id = return $ VarE $ mkName "helper_id"

        go (VarT v) = if v==varname
            then return $ ConE conname
            else id
        go (ConT c) = id

        -- | FIXME: The cases below do not cover all the possible functions we might want to derive
        go (TupleT 0) = id
        go t@(AppT (ConT c) t2) = do
            info <- reify c
            case info of
                TyConI (TySynD _ _ _) -> expandTySyn t >>= go
                FamilyI (FamilyD TypeFam _ _ _) _ -> id
                TyConI (NewtypeD _ _ _ _ _) -> liftM (AppE (VarE $ mkName "helper_liftM")) $ go t2
                TyConI (DataD _ _ _ _ _) -> liftM (AppE (VarE $ mkName "helper_liftM")) $ go t2
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

        go xxx = error $ "returnType2newtypeApplicator: xxx="++show xxx

isNewtypeInstance :: Name -> Name -> Q Bool
isNewtypeInstance typename classname = do
    info <- reify classname
    case info of
        ClassI _ inst -> return $ or $ map go inst
    where
        go (InstanceD _ (AppT _ (AppT (ConT n) _)) _) = n==typename
        go _ = False


substituteNewtype :: Name -> Name -> Name -> Type -> Type
substituteNewtype conname varname newvar = go
    where
        go (VarT v) = if varname==v
            then AppT (ConT conname) (VarT varname)
            else VarT v
        go (AppT t1 t2) =  AppT (go t1) (go t2)
        go (ConT t) = ConT t

typeL2patL :: Name -> Name -> [Type] -> [Pat]
typeL2patL conname varname xs = map go $ zip (map (\a -> mkName [a]) ['a'..]) xs
    where
        go (newvar,VarT v) = if v==varname
            then ConP conname [VarP newvar]
            else VarP newvar
        go (newvar,AppT (ConT _) (VarT v)) = VarP newvar
        go (newvar,AppT ListT (VarT v)) = VarP newvar
        go (newvar,AppT ListT (AppT (ConT _) (VarT v))) = VarP newvar
        go (newvar,ConT c) = VarP newvar
        go (newvar,_) = VarP newvar

        go qqq = error $ "qqq="++show qqq

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
        go (x:xs) = AppE (go xs) x
