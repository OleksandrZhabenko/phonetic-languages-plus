-- |
-- Module      :  Data.Statistics.RulesIntervalsPlus
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Additional statistic rules to choose the number of the intervals.

{-# LANGUAGE BangPatterns #-}

module Data.Statistics.RulesIntervalsPlus where

import  Data.Statistics.RulesIntervals
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

getIntervalsNS :: Bool -> String -> [String] -> Int
getIntervalsNS lstW xs yss
  | xs == "s" = sturgesH z
  | xs == "l" = levynskyiMod z
  | otherwise = fromMaybe 9 (readMaybe xs::(Maybe Int))
     where k = if lstW then 2 else 1
           z = length . filter ((> k) . length . words) $ yss
{-# INLINE getIntervalsNS #-}

getIntervalsN :: String -> [a] -> Int
getIntervalsN xs yss
  | xs == "s" = sturgesH (length yss)
  | xs == "l" = levynskyiMod (length yss)
  | otherwise = fromMaybe 9 (readMaybe xs::(Maybe Int))
{-# INLINE getIntervalsN #-}
