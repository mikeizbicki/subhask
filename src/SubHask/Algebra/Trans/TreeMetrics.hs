module SubHask.Algebra.Trans.TreeMetrics
    where

import SubHask

import Data.Maybe

indicator True = 1
indicator False = 0

class Tree t where
    node :: t -> Maybe (Elem t)

    children :: t -> [t]

    isLeaf :: t -> Bool
    isLeaf t = case children t of
        [] -> True
        _  -> False

instance Tree [a] where
    node [] = Nothing
    node (x:_) = Just x

    children [] = []
    children (x:xs) = [xs]

descendents :: Tree t => t -> [Elem t]
descendents t = (catMaybes $ map node $ children t) ++ concat (map descendents $ children t)

numDescendents :: Tree t => t -> Int
numDescendents = length . descendents

height :: Tree t => t -> Int
height t = 1 + maximum (0:(map height $ children t))

width :: Tree t => t -> Int
width t = maximum $ (length $ children t) : (map width $ children t)

-------------------------------------------------------------------------------

newtype TreeEditDistance t = TreeEditDistance t
    deriving (Read,Show,Eq,Tree,Semigroup,Monoid)

type instance Scalar (TreeEditDistance t) = Scalar t

instance Container t => Container (TreeEditDistance t) where
    type Elem (TreeEditDistance t) = Elem t
    elem e (TreeEditDistance t) = elem e t

instance
    ( Tree t
    , Ord (Scalar t)
    , Ring (Scalar t)
    , Eq (Elem t)
    , Eq t
    ) => MetricSpace (TreeEditDistance t) where
    distance t1 t2
        | isLeaf t1 && isLeaf t2 = indicator $ not $ node t1 == node t2
        | isLeaf t1 = fromIntegral $ 1 + numDescendents t1
        | isLeaf t2 = fromIntegral $ 1 + numDescendents t2
        | otherwise = minimum
            [ -- FIXME
            ]
