{-# LANGUAGE FlexibleContexts #-}
-- | Homework 3

module Archivers where

import qualified Base                  as B (Show (..))
import           Control.Exception     (assert)
import           Control.Monad.Writer  (MonadWriter, Writer, runWriter, tell)
import           Data.Bifunctor        (second)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as M
import           Data.Ratio            ((%))
import qualified Data.Text             as T
import           Universum             hiding ((%))

type String = [Char]

log2 :: Double -> Double
log2 k = log k / log 2

proverb :: ByteString
proverb =
    encodeUtf8
        ("Love the heart that hurts you, but never hurt the heart that loves you." :: Text)

newtype Logger w a = Logger
    { getLogger :: Writer [w] a
    } deriving (Functor, Applicative, Monad, MonadWriter [w])

data HuffmanTrace = HuffmanTrace
    { hCurChar   :: Char
    , hProb      :: Ratio Int
    , hCodeWord  :: String
    , hMsgLength :: Int
    }

instance Show HuffmanTrace where
    show HuffmanTrace {..} =
        intercalate
            ", "
            [show hCurChar, show hProb, show hCodeWord, show hMsgLength]

huffman :: ByteString -> Logger HuffmanTrace ((Map Char (String,Int)),String)
huffman input = encode 0 []
  where
    encode i s | i >= BS.length input = pure (table1, s)
    encode i s = do
        let c = input `BS.index` i
            (codeWord,m) = table1 M.! c
            cl = length codeWord
            s' = s ++ codeWord
            hProb = m % n
            hMsgLength = length s'
        tell $ [HuffmanTrace {hCurChar = c, hCodeWord = codeWord, ..}]
        encode (i+1) s'
    n = BS.length input
    firstPass :: Map Char Int
    firstPass = BS.foldr' (M.alter (pure . maybe 1 (+1))) M.empty input
    calcWords :: [(Double,[(Char,String)])]
    calcWords =
        map (\(k, x) -> (fromIntegral x / fromIntegral n,[(k,[])])) $ M.assocs firstPass
    calcCodeWordDo [(p,x)] = assert (p == 1) x
    calcCodeWordDo ((p0,lefts):(p1,rights):xs) =
        let append a = map (second (a:))
        in calcCodeWordDo $ sort $ (p0 + p1, append '0' lefts ++ append '1' rights):xs
    codeWords :: [(Char, String)]
    codeWords = calcCodeWordDo $ sort calcWords
    table1 = M.fromList $ map (\(c,s) -> (c,(s,firstPass M.! c))) codeWords

runHuffman x = do
    let ((tbl1, str), tbl2) = runWriter $ getLogger $ huffman x
    forM_ (M.assocs tbl1) print
    forM_ tbl2 print
