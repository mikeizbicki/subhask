module SubHask.Compatibility.ByteString
    where

import SubHask
-- import SubHask.Compatibility.Base
-- import SubHask.Monad
import SubHask.Algebra.Parallel
import SubHask.TemplateHaskell.Deriving

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Prelude as P
import Control.Monad

--------------------------------------------------------------------------------

-- | Promoted type for differentiating lazy and strict byte strings
data Strictness
    = Lazy
    | Strict

-- |
--
-- FIXME:
-- Add all the other ByteStrings.
-- Add all the BS. functions into the Foldable/Unfoldable classes
data family ByteString (lazy::Strictness) elem

type instance Scalar (ByteString a b) = Int
type instance Logic (ByteString a b) = Bool
type instance Elem (ByteString a b) = b

----------------------------------------

newtype instance ByteString Lazy Char = BSLC { unBSLC :: BS.ByteString }
    deriving (NFData,Read,Show)

instance Arbitrary (ByteString Lazy Char) where
    arbitrary = fmap fromList arbitrary

instance Eq_ (ByteString Lazy Char) where
    (BSLC b1)==(BSLC b2) = b1 P.== b2

instance POrd_ (ByteString Lazy Char) where
    inf (BSLC b1) (BSLC b2) = fromList $ map fst $ P.takeWhile (\(a,b) -> a==b) $ BS.zip b1 b2
    (BSLC b1) < (BSLC b2) = BS.isPrefixOf b1 b2

instance MinBound_ (ByteString Lazy Char) where
    minBound = zero

instance Semigroup (ByteString Lazy Char) where
    (BSLC b1)+(BSLC b2) = BSLC $ BS.append b1 b2

instance Monoid (ByteString Lazy Char) where
    zero = BSLC BS.empty

instance Container (ByteString Lazy Char) where
    elem x (BSLC xs) = BS.elem x xs
    notElem x (BSLC xs) = BS.notElem x xs

instance Constructible (ByteString Lazy Char) where
    fromList1 x xs = BSLC $ BS.pack (x:xs)
    singleton = BSLC . BS.singleton

instance Normed (ByteString Lazy Char) where
    size (BSLC xs) = fromIntegral $ P.toInteger $ BS.length xs

instance Foldable (ByteString Lazy Char) where
    uncons (BSLC xs) = case BS.uncons xs of
        Nothing -> Nothing
        Just (x,xs) -> Just (x,BSLC xs)

    toList (BSLC xs) = BS.unpack xs

    foldr   f a (BSLC xs) = BS.foldr   f a xs
--     foldr'  f a (BSLC xs) = BS.foldr'  f a xs
    foldr1  f   (BSLC xs) = BS.foldr1  f   xs
--     foldr1' f   (BSLC xs) = BS.foldr1' f   xs

    foldl   f a (BSLC xs) = BS.foldl   f a xs
    foldl'  f a (BSLC xs) = BS.foldl'  f a xs
    foldl1  f   (BSLC xs) = BS.foldl1  f   xs
    foldl1' f   (BSLC xs) = BS.foldl1' f   xs

instance Partitionable (ByteString Lazy Char) where
    partition n (BSLC xs) = go xs
        where
            go xs = if BS.null xs
                then []
                else BSLC a:go b
                where
                    (a,b) = BS.splitAt len xs

            n' = P.fromIntegral $ toInteger n
            size = BS.length xs
            len = size `P.div` n'
              P.+ if size `P.rem` n' P.== (P.fromInteger 0) then P.fromInteger 0 else P.fromInteger 1

--------------------------------------------------------------------------------

-- |
--
-- FIXME:
-- Make generic method "readFile" probably using cereal/binary
readFileByteString :: FilePath -> IO (ByteString Lazy Char)
readFileByteString = fmap BSLC . BS.readFile

--------------------------------------------------------------------------------

-- | FIXME:
-- Make this generic by moving some of the BS functions into the Foldable/Unfoldable type classes.
-- Then move this into Algebra.Containers
newtype PartitionOnNewline a = PartitionOnNewline a

deriveHierarchy ''PartitionOnNewline [''Monoid,''Boolean,''Foldable]

instance (a~ByteString Lazy Char, Partitionable a) => Partitionable (PartitionOnNewline a) where
    partition n (PartitionOnNewline xs) = map PartitionOnNewline $ go $ partition n xs
        where
            go []  = []
            go [x] = [x]
            go (x1:x2:xs) = (x1+BSLC a):go (BSLC b:xs)
                where
                    (a,b) = BS.break (=='\n') $ unBSLC x2

