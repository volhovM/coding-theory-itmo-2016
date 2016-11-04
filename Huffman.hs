{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Main (main) where

import           Control.Lens         (ix, preview, (&), (+~), (^.))
import           Control.Monad        (forM, forM_)
import           Data.Array.IO        (IOUArray (..))
import           Data.Array.MArray    (MArray, freeze, getBounds, newArray, readArray,
                                       writeArray)
import           Data.Bifunctor       (bimap)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed       (embedFile)
import           Data.Ix              (Ix, range)
import           Data.Maybe           (fromJust)
import           Data.Vector          ((!))
import qualified Data.Vector          as V
import           Linear.Matrix        ((!*!))
import           Linear.Metric        (dot)

log2 :: Double -> Double
log2 k = log k / log 2


-- | This function takes matrix and raises it to the power n.
-- (!*!) is matrix multiplication.
(!*^) :: Matrix -> Int -> Matrix
(!*^) m 0 = m
(!*^) m i = m !*! (m !*^ (i-1))

-- Type aliases for vector and matrix
type Vector = V.Vector Double
type Matrix = V.Vector Vector

-- | Calculate the entropy
entropy :: Vector -> Double
entropy = sum . V.map (\p -> - p * log2 p)

-- | Given matrix
matrix :: Matrix
matrix =
    V.fromList $ map V.fromList $
    [[1/4,  0, 3/4],
    [  0, 1/4, 3/4],
    [1/4, 1/2, 1/4]]

-- | Solution vector calculated from raising matrix E to the power
-- 5000.
p :: Vector
p = (matrix !*^ 5000) ! 0

hv :: Int -> Double
hv v = sum $ map (\i -> case fromJust $ preview (ix v . ix i) matrix of
                     0 -> 0
                     p -> - p * log2 p)
                 [0..2]
hvVec = V.fromList $ map hv [0..2]

hh = p `dot` hvVec

hxx :: Double
hxx =
    sum $ (flip map [0..2]) $ \i ->
    - (p ! i) *
    sum ((flip map [0..2]) $ \j ->
      case matrix ! i ! j of
        0   -> 0
        pij -> pij * log2 (pij))



main :: IO ()
main = undefined
