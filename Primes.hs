-- primes.hs

module Primes (primesTo100, primesTo10000, isPrime, nextPrime,
               primePowerFactors, -- primeFactors,
			   fromPrimePowerFactors, isMillerRabinPrime,
			   isSquareFree, isPrimePower) where


import NumberTheoryFundamentals (divides, splitWith, power)
import FF


-- sieve of Eratosthenes - much slower than below
primesTo10000' = doPrimesTo10000 (2:[3,5..10000])
	where
		doPrimesTo10000 (p:ps) = p : doPrimesTo10000 (filter (\n -> not (p `divides` n)) ps)
		doPrimesTo10000 [] = []

primes = sieve [2..]
	where sieve (p:ns) = p : sieve (filter (notdiv p) ns)
	      notdiv p n = n `mod` p /= 0


-- PRIMES

primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

trialDivision ps n = doTrialDivision ps
	where doTrialDivision (p:ps) = let (q,r) = n `quotRem` p in if r == 0 then False else if q < p then True else doTrialDivision ps
	      doTrialDivision [] = True
-- q < p => p > sqrt n

primesTo10000 = primesTo100 ++ filter (trialDivision primesTo100) [101,103..9999]

isTrialDivisionPrime 2 = True -- special case, not caught by above code
isTrialDivisionPrime n = trialDivision (primesTo10000 ++ [10001,10003..]) n


-- MILLER-RABIN TEST
-- Cohen, A Course in Computational Algebraic Number Theory
-- Koblitz, A Course in Number Theory and Cryptography


-- Let n-1 = 2^s * t
-- Then n is a strong pseudoprime to base b if
-- either b^t == 1 (mod n)
-- or b^(2^r * t) == -1 (mod n) for some 0 <= r < s
-- (For we know that if n is prime, then b^(n-1) == 1 (mod n)

isStrongPseudoPrime :: Integer -> (Int,Integer) -> Integer -> Bool
-- call with n the modulus, (s,t) such that 2^s * t == n-1, b the base
-- (s,t) then used internally for iteration
isStrongPseudoPrime n (s,t) b =
	-- let b' = powerMod n b t
	let b' = power (1, \x y -> x*y `mod` n) b t
	in if b' == 1 then True else doSquaring s b'
	where
		doSquaring 0 x = False
		doSquaring s x
			| x == n-1  = True
			| x == 1    = False
			| otherwise = doSquaring (s-1) (x*x `mod` n)

-- this is my own variant on the Miller-Rabin test
-- the Rabin test involves selecting random bases b and checking that p is a strong pseudoprime to that base
-- if p passes the strong pseudoprime test for k bases, the probability of a false positive is 4^-k.
-- Rather than using random bases, I simply use the first 25 primes

isMillerRabinPrime :: Integer -> Bool
isMillerRabinPrime n
	| n < 100   = n `elem` primesTo100  -- the test below would not be valid
	| otherwise = all (isStrongPseudoPrime n (s,t)) primesTo100
		where (s,t) = (n-1) `splitWith` 2 -- so n-1 == 2^s * t

-- to test the algorithm, following set should be empty (replace numbers as required)
-- [n | n<-[100000..101000], isTrialDivisionPrime n /= isMillerRabinPrime n]


isPrime :: Integer -> Bool
isPrime n
	| n < 2          = False
	| n < 500000000  = isTrialDivisionPrime n
	| n >= 500000000 = isMillerRabinPrime n
-- 5*10^8 appears to be about the point at which on average Miller Rabin becomes faster than trial division


nextPrime :: Integer -> Integer
nextPrime n = head [p | p <- [n..], isPrime p]




smallMersennePrimes = map (\p -> 2^p-1) [2,3,5,7,13,17,19,31,61,89,107,127]
-- useful in testing



-- FACTORING (SMALL PRIMES)

-- Factoring by trial division of primes up to 10000

primePowerFactors :: Integer -> [(Integer,Int)]
primePowerFactors n | n > 0 = takeOutFactors n primesTo10000
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
			if n < 100000000 || isMillerRabinPrime n -- we already know it's a trial division prime up to 10000
			then [(n,1)]
			else error ("primePowerFactors: unable to factor " ++ show n)


-- !! what's the point of this
primeFactors :: Integer -> [Integer]
primeFactors n = concat (map (\(p,a) -> replicate a p) (primePowerFactors n))
-- it would probably be slightly faster to do this directly

fromPrimePowerFactors :: [(Integer,Int)] -> Integer
fromPrimePowerFactors factors = product [p^a | (p,a) <- factors]



-- SIMPLE APPLICATIONS

isSquareFree n = all (\(p,a)-> a == 1) (primePowerFactors n)

isPrimePower n = length (primePowerFactors n) == 1
