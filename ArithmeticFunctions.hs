-- arithmeticfunctions.hs

module ArithmeticFunctions where

import NumberTheoryFundamentals (divides)
import Primes
-- import Bits ( (.&.) )



-- EULER'S TOTIENT FUNCTION

eulerTotient n =
	let factors = primePowerFactors n
	in product (map (\(p,a) -> p^(a-1) * (p-1)) factors)

-- much less efficient - only used for testing
eulerTotient2 n = length [x | x <- [1..n], gcd n x == 1]


-- MOBIUS FUNCTION

mobius n
	| n <= 0     = error "mobius: not defined for n <= 0"
	| otherwise  = doMobius n (primesTo10000 ++ [p | p <- [10001,10003..], isPrime p])
	where
		doMobius n (p:ps)
			| n == 1           = 1
			| (p*p) `divides` n  = 0
			| p `divides` n    = negate (doMobius (n `div` p) ps)
			| otherwise        = doMobius n ps


-- SIGMA FUNCTIONS

-- see Hardy and Wright p239 for full explanation

numDivisors :: Integer -> Integer
-- the number of divisors of n ( = sigma 0 n)
numDivisors n = product [ toInteger (a+1) | (p,a) <- primePowerFactors n]
-- we return an Integer, not an Int
-- for example, the product of the first 32 primes has 2^32 factors

sigma1 :: Integer -> Integer
-- sigma1 n is the sum of the divisors of n
sigma1 n = product [((p^(a+1)) - 1) `div` (p - 1) | (p,a) <- primePowerFactors n]

sigma :: Int -> Integer -> Integer
-- sigma k n is the sum of the kth powers of the divisors of n
sigma k n
	| n <= 0    = error "sigma: n <= 0"
	| k < 0     = error "sigma: k < 0"
	| k == 0    = numDivisors n
	| otherwise = product [(p^((a+1)*k) - 1) `div` (p^k - 1) | (p,a) <- primePowerFactors n]


sigma1' n = sum [d | d <- [1..n], n `mod` d == 0]
-- very inefficient for large n - defined for testing purposes only

sigma' k n = sum [d^k | d <- [1..n], n `mod` d == 0]
-- very inefficient for large n - defined for testing purposes only

sigma1primePowerProduct :: [(Integer,Int)] -> Integer
-- sigma1primeProduct takes its input already factorised
sigma1primePowerProduct factors = product [(p^(a+1) - 1) `div` (p - 1) | (p,a) <- factors]


-- HARMONIC FUNCTION

gamma = 0.5772156649015328606065 :: Double
-- Euler's constant, == lim (n -> inf) (sum [h k | k <- [1..n]] - log n)
-- Source: Gamma by Julian Havil

harmonicFunction :: Integer -> Double
harmonicFunction n
	| n <= 0    = error "harmonicFunction: not defined for n <= 0"
	| n <= 20   = harmonicFunction1 n 
	| otherwise = harmonicFunction2 n
-- for n <= 20, harmonicFunction1 probably faster to calculate, and of course it is more accurate
-- for n > 20, the error in harmonicFunction2 is less than 1 part in 10^10

harmonicFunction1 n = sum [1.0 / (fromInteger k) | k <- [1..n]]
-- too slow to use for large n

harmonicFunction2 n = 
		let n' = fromInteger n
		in log n' + gamma + (1 / (2*n')) - (1 / (12*n'*n')) + (1 / (120*n'*n'*n'*n'))
-- this estimate is accurate to O(1/n^6)
-- see Aigner, Ziegler, p11

-- Doubles valid up to about 1.75e308

