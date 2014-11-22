module SubHask.Algebra.Trans.MiscMetrics
    where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import qualified Prelude as P
import SubHask
import SubHask.TemplateHaskell.Deriving

-------------------------------------------------------------------------------
-- Match

newtype Match v a = Match { unMatch :: v a }

deriveHierarchy ''Match
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    , ''Ring
    ]

instance VG.Vector v a => VG.Vector (Match v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MatchM v) = liftM Match $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (Match v) = liftM MatchM $ VG.basicUnsafeThaw v
    basicLength (Match v) = VG.basicLength v
    basicUnsafeSlice s t (Match v) = Match $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (Match v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MatchM vm) (Match v) = VG.basicUnsafeCopy vm v
    elemseq (Match v) a b = VG.elemseq v a b

newtype MatchM v s a = MatchM { unMatchM :: v s a }

instance VGM.MVector v a => VGM.MVector (MatchM v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (MatchM v) = VGM.basicLength v
    basicUnsafeSlice s t (MatchM v) = MatchM $ VGM.basicUnsafeSlice s t v
    basicOverlaps (MatchM v1) (MatchM v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM MatchM $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM MatchM $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (MatchM v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (MatchM v) i a = VGM.basicUnsafeWrite v i a

    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeCopy (MatchM v1) (MatchM v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MatchM v1) (MatchM v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MatchM v) i = MatchM `liftM` VGM.basicUnsafeGrow v i

type instance VG.Mutable (Match v) = MatchM (VG.Mutable v)

---------------------------------------

instance
    ( VG.Vector v r
    , Ord (Scalar (v r))
    , r ~ Scalar (v r)
    , Eq (v r)
    , Normed r
    , Ord r
    , IsScalar r
    ) => MetricSpace (Match v r)
        where

    distance (Match v1) (Match v2) = go 0 0 0 0
        where
            go tot cdf1 cdf2 i = if i < VG.length v1
                then go (abs $ cdf1' - cdf2') cdf1' cdf2' (i+1)
                else tot
                    where
                        cdf1'=cdf1+v1 `VG.unsafeIndex` i
                        cdf2'=cdf2+v2 `VG.unsafeIndex` i

-------------------------------------------------------------------------------
-- HistogramIntersection

newtype HistogramIntersection v a = HistogramIntersection { unHistogramIntersection :: v a }

deriveHierarchy ''HistogramIntersection
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    , ''Ring
    ]

instance VG.Vector v a => VG.Vector (HistogramIntersection v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (HistogramIntersectionM v) = liftM HistogramIntersection $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (HistogramIntersection v) = liftM HistogramIntersectionM $ VG.basicUnsafeThaw v
    basicLength (HistogramIntersection v) = VG.basicLength v
    basicUnsafeSlice s t (HistogramIntersection v) = HistogramIntersection $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (HistogramIntersection v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (HistogramIntersectionM vm) (HistogramIntersection v) = VG.basicUnsafeCopy vm v
    elemseq (HistogramIntersection v) a b = VG.elemseq v a b

newtype HistogramIntersectionM v s a = HistogramIntersectionM { unHistogramIntersectionM :: v s a }

instance VGM.MVector v a => VGM.MVector (HistogramIntersectionM v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (HistogramIntersectionM v) = VGM.basicLength v
    basicUnsafeSlice s t (HistogramIntersectionM v) = HistogramIntersectionM $ VGM.basicUnsafeSlice s t v
    basicOverlaps (HistogramIntersectionM v1) (HistogramIntersectionM v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM HistogramIntersectionM $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM HistogramIntersectionM $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (HistogramIntersectionM v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (HistogramIntersectionM v) i a = VGM.basicUnsafeWrite v i a

    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeCopy (HistogramIntersectionM v1) (HistogramIntersectionM v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (HistogramIntersectionM v1) (HistogramIntersectionM v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (HistogramIntersectionM v) i = HistogramIntersectionM `liftM` VGM.basicUnsafeGrow v i

type instance VG.Mutable (HistogramIntersection v) = HistogramIntersectionM (VG.Mutable v)

---------------------------------------

instance
    ( VG.Vector v r
    , Ord (Scalar (v r))
    , r ~ Scalar (v r)
    , Eq (v r)
    , Normed r
    , Ord r
    , IsScalar r
    ) => MetricSpace (HistogramIntersection v r)
        where

    {-# INLINE distance #-}
    distance (HistogramIntersection !v1) (HistogramIntersection !v2) =
        {-# SCC histogramintersection_distance #-}
        (VG.foldl1' (+) v1) + (VG.foldl1' (+) v2) - go 0 0
        where
            go !tot !i = if i>= VG.length v1
                then tot
                else go (tot+add) (i+1)
                where
                    add = {-max 0 $-} min (v1 `VG.unsafeIndex` i) (v2 `VG.unsafeIndex` i)

-------------------------------------------------------------------------------
-- Xi2

newtype Xi2 v a = Xi2 { unXi2 :: v a }

deriveHierarchy ''Xi2
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    , ''Ring
    ]

deriving instance Functor v => Functor (Xi2 v)

instance VG.Vector v a => VG.Vector (Xi2 v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (Xi2M v) = liftM Xi2 $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (Xi2 v) = liftM Xi2M $ VG.basicUnsafeThaw v
    basicLength (Xi2 v) = VG.basicLength v
    basicUnsafeSlice s t (Xi2 v) = Xi2 $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (Xi2 v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (Xi2M vm) (Xi2 v) = VG.basicUnsafeCopy vm v
    elemseq (Xi2 v) a b = VG.elemseq v a b

newtype Xi2M v s a = Xi2M { unXi2M :: v s a }

instance VGM.MVector v a => VGM.MVector (Xi2M v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (Xi2M v) = VGM.basicLength v
    basicUnsafeSlice s t (Xi2M v) = Xi2M $ VGM.basicUnsafeSlice s t v
    basicOverlaps (Xi2M v1) (Xi2M v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM Xi2M $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM Xi2M $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (Xi2M v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (Xi2M v) i a = VGM.basicUnsafeWrite v i a

    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeCopy (Xi2M v1) (Xi2M v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (Xi2M v1) (Xi2M v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (Xi2M v) i = Xi2M `liftM` VGM.basicUnsafeGrow v i

type instance VG.Mutable (Xi2 v) = Xi2M (VG.Mutable v)

---------------------------------------

instance
    ( VG.Vector v r
    , Eq (v r)
    , Floating r
    , Field r
    , Normed r
    , Ord r
    , IsScalar r
    , r ~ Scalar (v r)
    , Ord (Scalar (v r))
    ) => MetricSpace (Xi2 v r)
        where

    {-# INLINE distance #-}
    distance (Xi2 !v1) (Xi2 !v2) = {-# SCC xi2_distance #-} sqrt $ go 0 0
        where
            go !tot !i = if i>= VG.length v1
                then tot
                else go (tot+add) (i+1)
                where
                    add = if v1 `VG.unsafeIndex` i == v2 `VG.unsafeIndex` i
                        then 0
                        else (v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                            *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                            /(abs (v1 `VG.unsafeIndex` i)+abs(v2 `VG.unsafeIndex` i))

-------------------------------------------------------------------------------
-- JensenShannonDivergence

newtype JensenShannonDivergence v a = JensenShannonDivergence { unJensenShannonDivergence :: v a }

deriveHierarchy ''JensenShannonDivergence
    [ ''Ord
    , ''Boolean
    , ''VectorSpace
    , ''Ring
    ]

instance VG.Vector v a => VG.Vector (JensenShannonDivergence v) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (JensenShannonDivergenceM v) = liftM JensenShannonDivergence $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (JensenShannonDivergence v) = liftM JensenShannonDivergenceM $ VG.basicUnsafeThaw v
    basicLength (JensenShannonDivergence v) = VG.basicLength v
    basicUnsafeSlice s t (JensenShannonDivergence v) = JensenShannonDivergence $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (JensenShannonDivergence v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (JensenShannonDivergenceM vm) (JensenShannonDivergence v) = VG.basicUnsafeCopy vm v
    elemseq (JensenShannonDivergence v) a b = VG.elemseq v a b

newtype JensenShannonDivergenceM v s a = JensenShannonDivergenceM { unJensenShannonDivergenceM :: v s a }

instance VGM.MVector v a => VGM.MVector (JensenShannonDivergenceM v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (JensenShannonDivergenceM v) = VGM.basicLength v
    basicUnsafeSlice s t (JensenShannonDivergenceM v) = JensenShannonDivergenceM $ VGM.basicUnsafeSlice s t v
    basicOverlaps (JensenShannonDivergenceM v1) (JensenShannonDivergenceM v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM JensenShannonDivergenceM $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM JensenShannonDivergenceM $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (JensenShannonDivergenceM v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (JensenShannonDivergenceM v) i a = VGM.basicUnsafeWrite v i a

    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeMove #-}
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeCopy (JensenShannonDivergenceM v1) (JensenShannonDivergenceM v2) = VGM.basicUnsafeCopy v1 v2
    basicUnsafeMove (JensenShannonDivergenceM v1) (JensenShannonDivergenceM v2) = VGM.basicUnsafeMove v1 v2
    basicUnsafeGrow (JensenShannonDivergenceM v) i = JensenShannonDivergenceM `liftM` VGM.basicUnsafeGrow v i

type instance VG.Mutable (JensenShannonDivergence v) = JensenShannonDivergenceM (VG.Mutable v)

---------------------------------------

instance
    ( VG.Vector v r
    , Eq (v r)
    , Floating r
    , Field r
    , Normed r
    , Ord r
    , IsScalar r
    , r ~ Scalar (v r)
    , Ord (Scalar (v r))
    ) => MetricSpace (JensenShannonDivergence v r)
        where

    {-# INLINE distance #-}
    distance (JensenShannonDivergence !v1) (JensenShannonDivergence !v2) = {-# SCC js_distance #-} sqrt $ go 0 0
        where
            go !tot !i = if i>= VG.length v1
                then tot
                else go (tot+add) (i+1)
                where
                    add = if x==0
                        then if y==0
                            then 0
                            else (-log 0.5)*y
                        else if y==0
                            then (-log 0.5)*x
                            else x*log(2*x/(x+y)) + y*log(2*y/(x+y))
                    x=abs $ v1 `VG.unsafeIndex` i
                    y=abs $ v2 `VG.unsafeIndex` i

