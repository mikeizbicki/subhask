{-# LANGUAGE NoRebindableSyntax #-}

-- | Template haskell functions for converting base classes in the monad hierarchy into their equivalent subhask classes.
-- For these functions to work, you must have the following imports in your file:
--
-- > import qualified Control.Applicative as A
-- > import qualified Control.Monad as M
--
module SubHask.TemplateHaskell.Monad
    where

import qualified Prelude as P
import qualified Control.Exception as Exc
import qualified Control.Monad as M
import Language.Haskell.TH
import System.IO

import SubHask.Category
import SubHask.Algebra
import SubHask.Monad
import SubHask.Internal.Prelude

import Debug.Trace

--------------------------------------------------------------------------------

type instance Logic Dec = Bool
instance Eq_ Dec where (==) = (P.==)

--------------------------------------------------------------------------------
-- derive monad instances

deriveAllInScope :: Name -> (Cxt -> Type -> Q [Dec]) -> Q [Dec]
deriveAllInScope preludename f = do
    info <- reify preludename
    case info of
        ClassI _ xs -> M.liftM concat $ M.sequence $ map go xs
            where
                go (InstanceD ctx (AppT _ t) _) = f ctx t

-- | Create a "Functor" instance from a "Prelude.Functor" instance.
mkPreludeFunctor :: Cxt -> Type -> Q [Dec]
mkPreludeFunctor ctx t = M.return
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
mkPreludeApplicative cxt t = M.return
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
mkPreludeMonad cxt t = validType t $ M.return
    [ InstanceD
        cxt
        ( AppT
            ( ConT $ mkName "Then" )
            t
        )
        [ FunD ( mkName ">>" ) [ Clause [] (NormalB $ VarE $ mkName ">>") [] ]
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
        -- | we can't derive monads for certain Prelude Monads that don't have functor instances
        validType t xs = if elem (show t') badmonad then M.return [] else xs
            where
                t' :: Name
                t' = case t of
                    (AppT (ConT t) _) -> t
                    (ConT t) -> t
                    ListT -> mkName "[]"
                    t -> error ("pop="++show t)

                badmonad =
                    [ "Text.ParserCombinators.ReadP.P"
                    , "Control.Monad.ST.Lazy.Imp.ST"
                    ]
