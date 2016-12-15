{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Homework 3

module Archivers () where

import qualified Base                  as B (Show (..))
import           Control.Exception     (assert)
import           Control.Lens          (makeLenses, use, uses, (%=), (.=), (<>=))
import           Control.Monad.Writer  (MonadWriter, Writer, runWriter, tell)
import           Data.Bifunctor        (first, second)
import           Data.Bits             (testBit)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.List             (dropWhileEnd, nub)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Data.Number.BigFloat  (BigFloat (..), Prec50, PrecPlus20)
import           Data.Ord              (comparing)
import           Data.Ratio            ((%))
import qualified Data.Text             as T
import           Data.Word             (Word16)
import           Numeric               (showGFloat)
import           Universum             hiding ((%))

type String = [Char]

dropEnd :: Int -> [a] -> [a]
dropEnd i xs = take (length xs - i) xs

log2 :: Floating a => a -> a
log2 k = log k / log 2
log2' = log2 . fromIntegral

proverb :: ByteString
proverb =
    encodeUtf8
        ("Love the heart that hurts you, but never hurt the heart that loves you." :: Text)

newtype Logger w a = Logger
    { getLogger :: Writer [w] a
    } deriving (Functor, Applicative, Monad, MonadWriter [w])

fromWord8 :: [Word8] -> [Char]
fromWord8 = map (chr . fromIntegral)

----------------------------------------------------------------------------
-- Huffman
----------------------------------------------------------------------------

data HuffmanTrace = HuffmanTrace
    { hCurChar   :: Char
    , hProb      :: Ratio Int
    , hCodeWord  :: String
    , hMsgLength :: Int
    }

instance Show HuffmanTrace where
    show HuffmanTrace {..} =
        intercalate
            "|"
            [[hCurChar], show hProb, hCodeWord, show hMsgLength]

huffman :: BSC.ByteString -> Logger HuffmanTrace ((Map Char (String,Int)),String)
huffman input = encode 0 []
  where
    encode i s | i >= BSC.length input = pure (table1, s)
    encode i s = do
        let c = input `BSC.index` i
            (codeWord,m) = table1 M.! c
            cl = length codeWord
            s' = s ++ codeWord
            hProb = m % n
            hMsgLength = length s'
        tell $ [HuffmanTrace {hCurChar = c, hCodeWord = codeWord, ..}]
        encode (i+1) s'
    n = BSC.length input
    firstPass :: Map Char Int
    firstPass = BSC.foldr' (M.alter (pure . maybe 1 (+1))) M.empty input
    calcWords :: [(Double,[(Char,Int)])]
    calcWords =
        map (\(k, x) -> (fromIntegral x / fromIntegral n,[(k,0)])) $ M.assocs firstPass
    calcCodeWordDo [(p,x)] = assert (p == 1) x
    calcCodeWordDo (sortOn fst -> ((p0,lefts):(p1,rights):xs)) =
        let inc = map (second (+1))
        in calcCodeWordDo $ sortOn fst $ (p0 + p1, inc lefts ++ inc rights):xs
    codeWords :: [(Char, String)]
    codeWords = let res = sortOn snd $ calcCodeWordDo calcWords
                in foldl' codeNext [] res
    codeNext [] (c,l) = [(c, replicate l '0')]
    codeNext xs@((_,pr):_) (c,l) =
        let -- generates next codeword after pr of length l
            nextWord = let pr' = dropEnd 1 $ dropWhileEnd (== '1') pr
                       in pr' ++ "1" ++ replicate (l - length pr' - 1) '0'
        in (c,nextWord):xs
    table1 = M.fromList $ map (\(c,s) -> (c,(s,firstPass M.! c))) codeWords

runHuffman x = do
    let ((tbl1, str), tbl2) = runWriter $ getLogger $ huffman x
    forM_ (M.assocs tbl1) print
    forM_ tbl2 print


----------------------------------------------------------------------------
-- Adaptive arithmetic fixed precision
----------------------------------------------------------------------------

data ArithmState = ArithmState
    { _aLow     :: Word16
    , _aHigh    :: Word16
    , _aWord    :: [Bool]
    , _aLetters :: [Word8]
    }

makeLenses ''ArithmState

data ArithmTrace = ArithmTrace
    { aCurChar   :: [Char]
    , aProb      :: Double
    , aCodeWord  :: String
    , aMsgLength :: Int
    }

type ArithM a = StateT ArithmState (Writer [ArithmTrace]) a

instance Show ArithmTrace where
    show ArithmTrace {..} =
        intercalate
            "|"
            [aCurChar, showGFloat (Just 4) aProb "", aCodeWord, show aMsgLength]

convertToBits :: (Bits a) => a -> Int -> [Bool]
convertToBits x i = reverse $ map (\i -> testBit x i) [0 .. i-1]

arithmStep :: Map Word8 Double -> Word8 -> ArithM ()
arithmStep prob w = do
    low <- use aLow
    (delta :: Double) <- uses aHigh $ fromIntegral . (\x -> x - low)
    let letter = bool 0xff w (M.member w prob) -- choose escape if not present
        p, p' :: Word16
        p = round $
            delta *
            M.foldrWithKey
                (\w' pr acc -> bool acc (acc + pr) (w' < letter))
                0.0
                prob
        p' = p + round (delta * prob M.! letter)
        matches =
            maximum $ 0 :
            filter
                (\i -> all (\j -> testBit p j == testBit p' j) [16-i .. 15])
                [1 .. 16]
        sameBits = take matches $ convertToBits p 16
        low', high' :: Word16
        low' = shiftL p matches
        high' | matches == 0 = p'
              | otherwise = let s = shiftL p' matches
                            in s .|. (s - 1)
    aLow .= low'
    aHigh .= high'
    aWord <>= sameBits
    aLetters %= (letter:)
    l <- uses aWord length
    tell $ [ArithmTrace [chr' letter] (prob M.! letter) (map (bool '0' '1') sameBits) l]

    newLetters <- uses aLetters $ \letters -> filter (not . (`elem` letters)) [0..0xff]
    let probWithEscape =
            M.fromList $ map (\i -> (i, 1/(fromIntegral $ length newLetters))) newLetters
    when (letter /= w) $ arithmStep probWithEscape w
  where
    chr' 0xff = '\\'
    chr' x    = chr $ fromIntegral x

finalizeArith :: ArithM ()
finalizeArith = do
    high <- uses aHigh fromIntegral
    low <- uses aLow fromIntegral
    let delta, deltaP :: Double
        delta = high - low
        deltaP = delta / 0xffff
        bits = take (ceiling (- (log2 $ deltaP))) $
            convertToBits @Word16 (round $ low + delta / 2) 16
    aWord <>= bits
    l <- uses aWord length
    tell $ [ArithmTrace "final" 0 (map (bool '0' '1') bits) l]

runAdaptiveArithm :: ByteString -> ArithM ()
runAdaptiveArithm input = do
    forM_ [0..BS.length input-1] $ \k -> do
        letters <- use aLetters
        let n = fromIntegral $ length letters
            probM = M.fromList $
                map (second (/(n+1))) $
                (0xff, 1):
                (map (\l -> (l, fromIntegral $ length $ filter (==l) letters)) $ nub letters)
        arithmStep probM $ BS.index input k
    finalizeArith

execAdaptiveArithm x = runWriter $ (runStateT (runAdaptiveArithm x) (ArithmState 0 0xffff [] []))

----------------------------------------------------------------------------
-- Enumerative
----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial 1 = 1
factorial 0 = 1
factorial x = x * factorial (x-1)

enumerative :: BS.ByteString -> Integer
enumerative input = l1+l2
  where
    n = fromIntegral $ BS.length input
    chars = BS.unpack input
    unique = nub chars
    occurences =
        M.fromList $
        map (\i -> (i, fromIntegral $ length $ filter (== i) chars)) unique
    comp, compcomp, comp' :: [Integer]
    comp = reverse $
           sort $ map (\i -> fromMaybe 0 $ M.lookup i occurences) [0 .. 0xff]
    m = length comp
    compcomp = map (fromIntegral . length) $ group comp
    comp' = filter (> 0) comp
    l2 = ceiling $
         log2' $ foldr (\x acc -> acc `div` (factorial x)) (factorial n) comp'
    l11 = ceiling $ log2' $ n * product comp'
    l12 = ceiling $
          log2' $
          foldr (\x acc -> acc `div` (factorial x))
                (factorial $ fromIntegral $ length comp)
                compcomp
    l1 = l11 + l12


----------------------------------------------------------------------------
-- Universal coding
----------------------------------------------------------------------------

bin' :: Int -> [Bool]
bin' x = drop 1 $ dropWhile not $ convertToBits x 32

unar :: Int -> [Bool]
unar n = replicate (n-1) True ++ [False]

elias :: Int -> [Bool]
elias n = p1 ++ p2 ++ p3
  where
    p1 = unar $ 2 + length p2
    p2 = bin' $ length p3
    p3 = bin' n

mon :: Int -> [Bool]
mon n = p1 ++ p2
  where
    p1 = unar $ length p2 + 1
    p2 = bin' n

----------------------------------------------------------------------------
-- LZ-77
----------------------------------------------------------------------------


data LZ77State = LZ77State
    { _lzDict :: [Word8]
    , _lzWord :: [Bool]
    } deriving Show

makeLenses ''LZ77State

data LZ77Trace = LZ77Trace
    { lzFlag      :: Bool
    , lzCurString :: String
    , lzDist      :: Maybe Int
    , lzLength    :: Int
    , lzCodeWord  :: [Bool]
    , lzBits      :: Int
    , lzMsgLength :: Int
    }

instance Show LZ77Trace where
    show LZ77Trace {..} =
        intercalate
            "|"
            [ ""
            , showBool lzFlag
            , lzCurString
            , showMaybe lzDist
            , show lzLength
            , concatMap showBool lzCodeWord
            , show lzBits
            , show lzMsgLength
            , ""
            ]
      where
        showBool False = "0"
        showBool True  = "1"
        showMaybe Nothing  = ""
        showMaybe (Just a) = show a

type LZ77M a = StateT LZ77State (Writer [LZ77Trace]) a

lz77Do :: BS.ByteString -> Int -> Int -> LZ77M ()
lz77Do input _ i | i >= BS.length input = pure ()
lz77Do input window i = do
    bestMatch <- uses lzDict workingInputs
--    traceShowM bestMatch
--    traceShowM =<< uses lzDict (map (first fromWord8) . subwords)
    maybe onNewWord onMatch bestMatch
    stripDictionary
    lz77Do input window $ i + maybe 1 (length . fst) bestMatch
  where
    onMatch (match,i) = do
        let lzFlag = True
            lzCurString = fromWord8 match
            lzLength = length match
        lzDist <- uses lzDict $ \d -> length d - i
        dictSizeLog <-
            uses lzDict $ ceiling . log2' . (+1) . fromIntegral . length
--        traceShowM lzCurString
--        traceShowM =<< uses lzDict length
        let lzCodeWord =
                lzFlag :
                convertToBits lzDist dictSizeLog ++
                mon lzLength
            lzBits = length lzCodeWord
        lzWord <>= (lzCodeWord)
        lzMsgLength <- uses lzWord length
        tell $ [LZ77Trace {lzDist = Just lzDist,..}]
        lzDict <>= match
    onNewWord = do
        let lzCodeWord = lzFlag : convertToBits (fromJust $ head input') 8
            lzFlag = False
            lzCurString :: String
            lzCurString = fromWord8 $ take 1 input'
            lzDist = Nothing
            lzLength = 0
            lzBits = length lzCodeWord
        lzWord <>= lzCodeWord
        lzMsgLength <- uses lzWord length
        tell $ [LZ77Trace {..}]
        lzDict <>= [fromJust $ head input']
    workingInputs :: [Word8] -> Maybe ([Word8], Int)
    workingInputs dict =
        let filtered = filter (\(pr, _) -> pr `isPrefixOf` input') $
                subwords dict
        in bool (Just $ maximumBy (comparing (length . fst)) filtered)
                Nothing
                (null filtered)
    stripDictionary = do
        l <- uses lzDict length
        when (l > window) $ lzDict %= drop (l - window)
    input' = BS.unpack $ BS.drop i input
    tails' xs = dropEnd 1 (tails xs) `zip` [0 ..]
    inits' xs = concatMap (\(str, i) -> drop 1 $ (,i) <$> inits str) xs
    subwords :: [a] -> [([a],Int)]
    subwords = reverse . inits' . tails'

lz77Encode :: BS.ByteString -> Int -> LZ77M ()
lz77Encode bs w = lz77Do bs w 0

execLz77 :: ByteString -> Int -> (((), LZ77State), [LZ77Trace])
execLz77 x w = runWriter $ (runStateT (lz77Encode x w) (LZ77State [] []))
