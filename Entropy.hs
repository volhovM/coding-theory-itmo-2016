{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Codec.Compression.GZip (CompressParams (..))
import qualified Codec.Compression.GZip as Z
import           Control.Monad          (forM_)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.FileEmbed         (embedFile)
import qualified Data.Map               as M

file :: ByteString
file = $(embedFile "./PIC")
m = BS.length file

compress :: BSL.ByteString -> BSL.ByteString
compress =
    Z.compressWith $
    Z.defaultCompressParams
    { compressLevel = Z.bestCompression
    , compressMemoryLevel = Z.maxMemoryLevel
    }

-- (513216,52218)
sizes = (BS.length file, BSL.length . compress $ BSL.fromStrict file)

intcast = fromInteger . toInteger

-- Returns all substrings of length l of `file`
subBlocks :: Int -> [ByteString]
subBlocks bSize =
    map (\i -> BS.take bSize $ BS.drop i file)
        [0..(m - bSize - 1)]

-- Given a block size returns a map from substring to number of occurences
distributionMap :: Int -> M.Map ByteString Int
distributionMap bSize =
    foldr (M.alter $ Just . maybe 1 (+1)) M.empty $ subBlocks bSize

-- Generates an entropy from block size
entropy :: Int -> Double
entropy bSize =
  let dm = distributionMap bSize
      --n = intcast $ sum $ M.elems dm
      log2 x = log x / log 2
      n = intcast $ m - bSize
      distribution = M.map (\x -> intcast x / n) dm
      entr = negate $ sum $ map (\x -> x * log x) $ M.elems distribution
  in entr / intcast bSize

main :: IO ()
main = forM_ [1..4] $ \i -> putStr (show i ++ ": ") >> print (entropy i)
