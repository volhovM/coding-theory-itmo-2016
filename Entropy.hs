{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad   (forM_)
import qualified Data.ByteString as BS
import           Data.FileEmbed  (embedFile, embedStringFile)
import qualified Data.Map        as M
import           Debug.Trace

file :: BS.ByteString
file = $(embedFile "/home/volhovm/code/coding-theory/PIC")

intcast = fromInteger . toInteger

subBlocks :: Int -> [BS.ByteString]
subBlocks bSize | bSize <= 0 = error "kek"
subBlocks bSize =
    map (\i -> BS.take bSize $ BS.drop i file)
        [0..(BS.length file - bSize - 1)]

distributionMap :: Int -> M.Map BS.ByteString Int
distributionMap bSize =
    foldr (M.alter $ Just . maybe 1 (+1)) M.empty $ subBlocks bSize

entropy bSize =
  let dm = distributionMap bSize
      n = intcast $ sum $ M.elems dm
      distribution :: M.Map BS.ByteString Double
      distribution = M.map (\x -> intcast x / n) dm
      entr = negate $ sum $ map (\x -> x * log x) $ M.elems distribution
  in
--    trace (show n) $
--    trace (show $ take 15 $ M.elems dm) $
    entr / intcast bSize

main = forM_ [1..5] $ \i -> putStr (show i ++ ": ") >> print (entropy i)
