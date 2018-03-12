-- |
-- Module:      Math.NumberTheory.SmoothNumbers
-- Copyright:   (c) 2018 Frederick Schneider
-- Licence:     MIT
-- Maintainer:  Frederick Schneider <frederick.schneider2011@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Functions for generating smooth numbers
--
-- Step 1: Obtain a SmoothBasis using the fromSet, fromList, or fromSmoothUpperBound functions.
--         A SmoothBasis is a wrapper for a sorted, increasing, integral list of numbers > 1.
--         The numbers do not need to all be prime but they must all be relatively prime with each other.
-- 
-- Step 2: Pass the SmoothBasis param (along with other params as needed) to 
--         the smoothOver, smoothOverInRange or smoothOverInRangeBF functions.
--
-- Example Usage:  
--
-- 1) Compute the first 20 numbers whose prime factors are limited to 2, 7 and/or 11.
--    s1 n l = case sb of
--               Just x -> take n $ smoothOver x
--               _      -> error "Invalid input for smooth basis."
--             where sb = fromList l
--
--    > s1 20 [2,7,11]
--    [1,2,4,7,8,11,14,16,22,28,32,44,49,56,64,77,88,98,112,121]
--
-- 2) Compute the numbers between 10000 and 20000 which are 7-smooth.
--    s2 s lb ub = case sb of
--                 Just x -> smoothOverInRange x lb ub
--                 _      -> error "Invalid input for smooth basis."
--                 where sb = fromSmoothUpperBound s
--
--    > s2 7 10000 12000
--    [10000,10080,10125,10206,10240,10290,10368,10500,10584,10752,10800,10935,10976,11025,11200,11250,11340,11520,11664,11760,11907,12000]
  
module Math.NumberTheory.SmoothNumbers
  ( smoothOver
  , smoothOverInRange
  , smoothOverInRangeBF
  , fromSet
  , fromList
  , fromSmoothUpperBound
  , SmoothBasis
  ) where

import Data.List (sort, nub)
import qualified Data.Set as S
import Math.NumberTheory.Primes.Sieve (primes)

-- | A list of unique, increasing, relatively prime integral numbers > 1.
--   Note: Prime powers such as 4 are allowed.  A number of the form 8 * (2x + 1) would not be considered
--   smooth if 4 was part of the smooth basis.
newtype SmoothBasis a = SmoothBasis [a] deriving Show

-- Returns a SmoothBasis from a Set if it is valid.
fromSet :: Integral a => S.Set a -> Maybe (SmoothBasis a)
fromSet s = if isValid l then Just (SmoothBasis l) else Nothing where l = S.elems s

-- Returns a SmoothBasis from a list if it is valid.  Duplicates in the list are ignored.
fromList :: Integral a => [a] -> Maybe (SmoothBasis a)
fromList l = if isValid l' then Just (SmoothBasis l') else Nothing where l' = nub $ sort l

-- | Return the primes <= the upper bound param.
--
--   > fromSmoothUpperBound 20
--   Just (SmoothBasis [2,3,5,7,11,13,17,19])
--
fromSmoothUpperBound :: Integral a => a -> Maybe (SmoothBasis a)
fromSmoothUpperBound n = if (n < 2)
                         then Nothing
                         else Just $ SmoothBasis $ map fromInteger $ takeWhile (<= nI) primes
                         where nI = toInteger n

-- | Return the list from a SmoothBasis.
fromSmoothBasis :: Integral a => SmoothBasis a -> [a]
fromSmoothBasis (SmoothBasis l) = l

-- | smoothOver is the workhorse for this module. It elegantly generates an infinite list
--   of all numbers whose factors are limited to products of integral powers of the elements in the SmoothBasis. 
--   The powers are >= 0.
smoothOver :: Integral a => SmoothBasis a -> [a]
smoothOver pl = foldr (\p l -> mergeListLists $ iterate (map (p*)) l) [1] (fromSmoothBasis pl)
                where
                      {-# INLINE mergeListLists #-}
                      mergeListLists      = foldr go1 []
                        where go1 (h:t) b = h:(go2 t b)
                              go1 _     b = b
   
                              go2 a@(ah:at) b@(bh:bt)
                                | bh < ah   = bh : (go2 a bt)
                                | otherwise = ah : (go2 at b) -- no possibility of duplicates
                              go2 a b = if null a then b else a

-- | Filters results of smoothOver by range
smoothOverInRange   :: Integral a => SmoothBasis a -> a -> a -> [a]
smoothOverInRange s lo hi = takeWhile (<= hi) $ dropWhile (< lo) (smoothOver s)

-- | BF stands for Brute Force.  This function will manually factor over the input list.
smoothOverInRangeBF :: Integral a => SmoothBasis a -> a -> a -> [a]
smoothOverInRangeBF prs lo hi = filter (mf prs') [lo..hi]
                                where mf []     n    = (n == 1) -- mf means manually factor
                                      mf pl@(p:ps) n = if (mod n p == 0) 
                                                       then mf pl (div n p)
                                                       else mf ps n
                                      prs'           = fromSmoothBasis prs

-- isValid assumes that the list is sorted and unique and then checks if the list is suitable to be a SmoothBasis. 
isValid :: (Integral a) => [a] -> Bool
isValid pl = if (length pl == 0) then False else v' pl
             where v' []        = True
                   v' (x:xs)    = if (x < 2 || (not $ rpl x xs)) then False else v' xs
                   rpl _ []     = True  -- rpl means relatively prime to the rest of the list
                   rpl n (x:xs) = if (gcd n x > 1) then False else rpl n xs

