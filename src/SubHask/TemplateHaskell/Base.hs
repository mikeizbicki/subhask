{-# LANGUAGE NoRebindableSyntax #-}

-- | This file contains the template haskell code for deriving SubHask class instances from Base instances.
-- All of the standard instances are created in "SubHask.Compatibility.Base".
-- This module is exported so that you can easily make instances for your own types without any extra work.
-- To do this, just put the line
--
-- > deriveAll
--
-- at the bottom of your file.
-- Any types in scope that do not already have SubHask instances will have them created automatically.
--
-- FIXME:
-- Most classes aren't implemented yet.
-- I don't want to go through the work until their definitions stabilize somewhat.
module SubHask.TemplateHaskell.Base
    where

import qualified Prelude as P
import qualified Control.Applicative as A
import qualified Control.Monad as M
import Language.Haskell.TH
import System.IO

import SubHask.Category
import SubHask.Algebra
import SubHask.Monad
import SubHask.Internal.Prelude

import Debug.Trace

--------------------------------------------------------------------------------
-- We need these instances to get anything done

type instance Logic Name = Bool
instance Eq_ Name where (==) = (P.==)

type instance Logic Dec = Bool
instance Eq_ Dec where (==) = (P.==)

type instance Logic Type = Bool
instance Eq_ Type where (==) = (P.==)

--------------------------------------------------------------------------------
-- generic helper functions

-- | Derives instances for all data types in scope.
-- This is the only function you should need to use.
-- The other functions are exported only for debugging purposes if this function should fail.
deriveAll :: Q [Dec]
deriveAll = M.liftM concat $ M.mapM go
    [ (''P.Eq, mkPreludeEq)
    , (''P.Functor, mkPreludeFunctor)
    , (''A.Applicative,mkPreludeApplicative)
    , (''P.Monad,mkPreludeMonad)
    ]
    where
        go (n,f) = forAllInScope n f

-- | Constructs an instance using the given function for everything in scope.
forAllInScope :: Name -> (Cxt -> Type -> Q [Dec]) -> Q [Dec]
forAllInScope preludename f = do
    info <- reify preludename
    case info of
        ClassI _ xs -> M.liftM concat $ M.sequence $ map go xs
            where
                go (InstanceD ctx (AppT _ t) _) = f ctx t

-- | This is an internal helper function.
-- It prevents us from defining two instances for the same class/type pair.
runIfNotInstance :: Name -> Type -> Q [Dec] -> Q [Dec]
runIfNotInstance n t q = do
    inst <- alreadyInstance n t
    if inst
        then M.return []
        else {-trace ("deriving instance: "++show n++" / "++show t) $-} q
    where
        alreadyInstance :: Name -> Type -> Q Bool
        alreadyInstance n t = do
            info <- reify n
            M.return $ case info of
                ClassI _ xs -> or $ map (genericTypeEq t.rmInstanceD) xs

        -- FIXME:
        -- This function was introduced to fix a name capture problem where `Eq a` and `Eq b` are not recognized as the same type.
        -- The current solution is not correct, but works for some cases.
        genericTypeEq (AppT s1 t1) (AppT s2 t2) = genericTypeEq s1 s2 && genericTypeEq t1 t2
        genericTypeEq (ConT n1) (ConT n2) = n1==n2
        genericTypeEq (VarT _) (VarT _) = true
        genericTypeEq (SigT _ _) (SigT _ _) = true
        genericTypeEq (TupleT n1) (TupleT n2) = n1==n2
        genericTypeEq ArrowT ArrowT = true
        genericTypeEq ListT ListT = true
        genericTypeEq _ _ = false


        rmInstanceD (InstanceD _ (AppT _ t) _) = t

--------------------------------------------------------------------------------
-- comparison hierarchy

-- | Create an "Eq" instance from a "Prelude.Eq" instance.
mkPreludeEq :: Cxt -> Type -> Q [Dec]
mkPreludeEq ctx t = runIfNotInstance ''Eq_ t $ M.return
    [ TySynInstD
        ( mkName "Logic" )
        ( TySynEqn
            [ t ]
            ( ConT $ mkName "Bool" )
        )
    , InstanceD
        ctx
        ( AppT ( ConT $ mkName "Eq_" ) t )
        [ FunD ( mkName "==" ) [ Clause [] (NormalB $ VarE $ mkName "P.==") [] ]
        ]
    ]

--------------------------------------------------------------------------------
-- monad hierarchy


-- | Create a "Functor" instance from a "Prelude.Functor" instance.
mkPreludeFunctor :: Cxt -> Type -> Q [Dec]
mkPreludeFunctor ctx t = runIfNotInstance ''Functor t $ M.return
    [ InstanceD
        ctx
        ( AppT
            ( AppT
                ( ConT $ mkName "Functor" )
                ( ConT $ mkName "Hask" )
            )
            t
        )
        [ FunD ( mkName "fmap" ) [ Clause [] (NormalB $ VarE $ mkName "M.fmap") [] ]
        ]
    ]

-- | Create an "Applicative" instance from a "Prelude.Applicative" instance.
mkPreludeApplicative :: Cxt -> Type -> Q [Dec]
mkPreludeApplicative cxt t =  runIfNotInstance ''Applicative t $ M.return
    [ InstanceD
        cxt
        ( AppT
            ( AppT
                ( ConT $ mkName "Applicative" )
                ( ConT $ mkName "Hask" )
            )
            t
        )
        [ FunD ( mkName "pure" ) [ Clause [] (NormalB $ VarE $ mkName "A.pure") [] ]
        , FunD ( mkName "<*>" ) [ Clause [] (NormalB $ VarE $ mkName "A.<*>") [] ]
        ]
    ]

-- | Create a "Monad" instance from a "Prelude.Monad" instance.
mkPreludeMonad :: Cxt -> Type -> Q [Dec]
mkPreludeMonad cxt t = {-runIfNotInstance ''Monad t $-} if cannotDeriveMonad t
    then M.return []
    else M.return
        [ InstanceD
            cxt
            ( AppT
                ( ConT $ mkName "Then" )
                t
            )
            [ FunD ( mkName ">>" ) [ Clause [] (NormalB $ VarE $ mkName "M.>>") [] ]
            ]
        , InstanceD
            cxt
            ( AppT
                ( AppT
                    ( ConT $ mkName "Monad" )
                    ( ConT $ mkName "Hask" )
                )
                t
            )
            [ FunD ( mkName "return_" ) [ Clause [] (NormalB $ VarE $ mkName "M.return") [] ]
            , FunD ( mkName "join"    ) [ Clause [] (NormalB $ VarE $ mkName "M.join"  ) [] ]
            , FunD ( mkName ">>="     ) [ Clause [] (NormalB $ VarE $ mkName "M.>>="   ) [] ]
            , FunD ( mkName ">=>"     ) [ Clause [] (NormalB $ VarE $ mkName "M.>=>"   ) [] ]
            , FunD ( mkName "=<<"     ) [ Clause [] (NormalB $ VarE $ mkName "M.=<<"   ) [] ]
            , FunD ( mkName "<=<"     ) [ Clause [] (NormalB $ VarE $ mkName "M.<=<"   ) [] ]
            ]
        ]
    where
        -- | This helper function "filters out" monads for which we can't automatically derive an implementation.
        -- This failure can be due to missing Functor instances or weird type errors.
        cannotDeriveMonad t = elem (show t') badmonad
            where
                t' :: Name
                t' = case t of
                    (AppT (ConT t) _) -> t
                    (ConT t) -> t
                    ListT -> mkName "[]"
                    (AppT (AppT (ConT t) _) _) -> t
                    t -> error ("pop="++show t)

                badmonad =
                    [ "Text.ParserCombinators.ReadP.P"
                    , "Control.Monad.ST.Lazy.Imp.ST"
                    , "Data.Proxy.Proxy"
                    ]
