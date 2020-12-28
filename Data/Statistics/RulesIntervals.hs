-- |
-- Module      :  Data.Statistics.RulesIntervals
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Additional statistic rules to choose the number of the intervals.

{-# LANGUAGE BangPatterns #-}

module Data.Statistics.RulesIntervals where

import Data.Lists.FLines (newLineEnding)
import GHC.Real (ceiling)
import GHC.Float (int2Float)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

sturgesH :: Int -> Int
sturgesH n
  | compare n 0 == GT = ceiling (logBase 2 (int2Float n))
  | otherwise = error $ "sturgesH: undefined for the argument " ++ show n ++ newLineEnding
{-# INLINE sturgesH #-}

-- | According to @В. П. Левинський@ (V. P. Levynskyi) from @Опря А. Т. Статистика (модульний варіант з програмованою формою контролю знань).
-- Навч. посіб. --- К.: Центр учбової літератури, 2012. --- 448 с. ISBN 978-611-01-0266-7@) page 60. Always return odd values.
levynskyiMod :: Int -> Int
levynskyiMod n
  | compare n 100 == LT = g n
  | compare n 200 == LT = 11
  | compare n 300 == LT = 13
  | compare n 400 == LT = 15
  | compare n 500 == LT = 17
  | otherwise = let !k = sturgesH n in if even k then k + 7 else k + 8
       where g n
              | compare n 60 == GT = 9
              | compare n 40 == GT = 7
              | compare n 20 == GT = 5
              | otherwise = 3
{-# INLINABLE levynskyiMod #-}
