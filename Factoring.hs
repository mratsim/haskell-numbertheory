-- factoring.hs

module Factoring where

import List (partition)
import MergeSort
import NumberTheoryFundamentals (splitWith)
import Primes
import FactoringECM

import FactoringCFRAC

-- In tests, ECM comes out faster than CFRAC, but not hugely so.

factors n
	| isPrime n = [n]
	| otherwise = merge (factors d) (factors (n `div` d))
		where d = findFactorECM n

-- version of primePowerFactors which finds large factors too
primePowerFactorsL :: Integer -> [(Integer,Int)]
primePowerFactorsL n | n > 0 = takeOutFactors n primesTo10000
	where
		takeOutFactors n (p:ps)
--			| n == 1    = [] -- unnecessary, caught by following test
			| p*p > n   = finish n
			| otherwise =
				let (s,n') = n `splitWith` p
				in if s > 0 then (p,s) : takeOutFactors n' ps else takeOutFactors n ps
		takeOutFactors n [] = finish n
		finish 1 = []
		finish n =
			if n < 100000000 -- we already know it's a trial division prime up to 10000
			then [(n,1)]
			else counts (factors n)
		counts [] = []
		counts fs@(x:_) = let (xs,ys) = partition (==x) fs in (x, length xs) : counts ys


pairProducts [] = []
pairProducts (x:xs) = map (x*) xs ++ pairProducts xs