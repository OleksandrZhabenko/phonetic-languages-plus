-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Analyzes a poetic text in Ukrainian, for every line prints statistic data and
-- then for the whole poem prints the hypothesis evaluation information.
-- Is used in pair with some other programs, e. g. with propertiesText from uniqueness-periods-vector-exampls package
-- or with a new phonetic-languages-ukrainian series.
-- The program tries to be more accurate in cases of the lines consisting entirely of the words
-- which are unique in phonetic meaning alongside the line. Another hypothesis is for the seventh command line
-- argument equal to \"y0\" that the distribution
-- of the placement of the actual poetic text in Ukrainian is not one of the standard distributions.
-- It can probably have approximately a form of and is different for different authors:
--
-- >    --   --   --
-- >   /  \_/  \_/  \
--
-- To enable parallel computations (potentially, they can speed up the work), please, run the @distributionText@ executable with
-- @+RTS -threaded -RTS@ command line options with possibly @-N@ option inside.
--

{-# OPTIONS_GHC -threaded -rtsopts #-}

{-# LANGUAGE CPP, BangPatterns #-}

module Main where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Control.Parallel.Strategies
import Data.Maybe (fromMaybe,mapMaybe)
import Text.Read (readMaybe)
import System.Environment
import Numeric (showFFloat)
import Data.List (sort)
import Numeric.Stats
import qualified Data.ByteString.Char8 as B
import Data.Lists.FLines hiding (mconcat)
import Data.Statistics.RulesIntervals
import Data.Statistics.RulesIntervalsPlus
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

main :: IO ()
main = do
  args0 <- getArgs
  let !args = filter (/= "+W") args0
      !gzS = concat . take 1 $ args
      !printInput = concat . drop 1 . take 2 $ args
      !whitelines = any (== "+W") args0 -- Usually, if specified -- the third argument
  contents <- B.getContents
  innerProc whitelines gzS printInput contents

innerProc :: Bool -> String -> String -> B.ByteString -> IO ()
innerProc whitelines gzS printInput contents = do
  if printInput == "1" then B.putStr contents else B.putStr B.empty
  (!data31,!wordsCnt0_data32) <- processContents whitelines contents
  let !gz = getIntervalsN gzS data31 -- Obtained from the first command line argument except those ones that are for RTS
      !pair2s = zip data31 wordsCnt0_data32
      !data4 = mapMaybe (\(!x,(!y,_)) -> if y > 1 then Just x else Nothing) pair2s
  if null data4 then putStrLn (replicate 102 '-') >> putStrLn "1.000+-0.000\tALL!" >> putStrLn (replicate 102 '=') -- Well, this means that all the text consists of the lines that have no variativity from the program perspective and, therefore, they cannot be analyzed effectively by it. Nevertheless, you can accurately exclude them from the consideration. A rather rare occurrence.
  else do
      let (!mean1,!disp) = meanWithDispD2 data4
          !pairs = sort . filter ((/= 0) . snd) $ wordsCnt0_data32
          g !m !n = (length . takeWhile (\(_,v) -> v == n) . dropWhile (\(_,v) -> v /= n) . takeWhile (\(u,_) -> u == m) . dropWhile (\(u,_) -> u /= m) $ pairs) `using` rdeepseq
          h !y !x = mconcat [mconcat . map (\m1 -> mconcat [mconcat . map (\n1 ->  (if y then show (g m1 n1) else if g m1 n1 == 0 then "." else show (g m1 n1)) ++ "\t") $ [1..gz],newLineEnding]) $ [2..7],replicate 102 x]
      putStrLn . generalInfo1 gz pairs (mean1, disp) $ data31
      putStrLn (h False '~')
      putStrLn (h True '=')

processContents :: Bool -> B.ByteString -> IO ([Double],[(Int,Int)])
processContents whitelines contents = do
    let !anlines = B.lines contents
        !anStrs
          | whitelines = filter (not . null) . map (drop 6 . take 9 . B.words) $ anlines
          | otherwise = map (drop 6 . take 9 . B.words) anlines
        !ratioStrs = map (B.unpack . head) anStrs
        !wordsNStrs = map (B.unpack . (!! 1)) anStrs
        !intervalNStrs = map (B.unpack . last) anStrs
        !ratios = map (\xs -> fromMaybe 1.0 (readMaybe xs::Maybe Double)) ratioStrs
        !wordsNs = map (\xs -> fromMaybe 0 (readMaybe xs::Maybe Int)) wordsNStrs
        !intervalNs = map (\xs -> fromMaybe 0 (readMaybe xs::Maybe Int)) intervalNStrs
    return (ratios,zip wordsNs intervalNs)

generalInfo1 :: Int -> [(Int,Int)] -> (Double,Double) -> [Double] -> String
generalInfo1 gz pairs (mean1, disp) data31 =
 let !ks = map (\r -> length . takeWhile (== r) . dropWhile (/= r) . sort . map snd $ pairs) [1..gz]
     !s = sum ks in
       mconcat [replicate 102 '-', newLineEnding, mconcat . map (\r -> show r ++ "\t") $ [1..gz], newLineEnding, mconcat . map (\r ->  show r ++ "\t") $ ks,
         newLineEnding, mconcat . map (\r -> showFFloat (Just 2) (fromIntegral (r * 100) / fromIntegral s)  "%\t") $ ks,newLineEnding,
          mconcat [showFFloat (Just 4) mean1 "+-", showFFloat (Just 4) (sqrt disp) "\t", show (length . filter ((<= 1) . fst) $ pairs),
           '\t':show (length data31)], newLineEnding, mconcat . map (\r -> show r ++ "\t") $ [2..7], newLineEnding, mconcat .
              map (\r ->  (show . length . takeWhile (== r) . dropWhile (/= r) . map fst $ pairs) ++ "\t") $ [2..7], newLineEnding, replicate 102 '*']
{-# INLINE generalInfo1 #-}

