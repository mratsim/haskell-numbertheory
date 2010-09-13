-- randomgenerator.hs

module RandomGenerator where

import Bits (shiftL, shiftR, testBit)

-- !! WARNING
-- If using this code in GHCi < 6.4.1, then compile using -fvia-c option

one29 = 1 `shiftL` 29 :: Int

-- Schneier, Applied Cryptography, p376
bitGenerator :: Int -> (Int,Int)
bitGenerator seed =
	let
		feedback = if (seed + seed `shiftR` 2 + seed `shiftR` 29) `testBit` 0 then one29 else 0
		seed' = seed `shiftR` 1 + feedback
		bit = if seed' `testBit` 0 then 1 else 0
	in (bit, seed')


randomBitset :: Int -> Int -> (Integer,Int)
randomBitset bitLength seed = doRandomBitset (bitLength, 0, seed)
	where
		doRandomBitset (0, n, seed) = (n,seed)
		doRandomBitset (bitLength, n, seed) =
			let (bit, seed') = bitGenerator seed
			in doRandomBitset $! (bitLength - 1, n `shiftL` 1 + toInteger bit, seed')

randomInteger :: Integer -> Int -> (Integer, Int)
randomInteger n seed
	| n <= 0     = error "randomInteger: n <= 0"
	| otherwise  = doRandomInteger n (0,1,seed)
	where
		doRandomInteger n (i,m,seed) = 
			if m >= n -- i is now a random integer between 0 and m-1, where m is a power of 2
			then
				if i < n
				then (i,seed)
				else (doRandomInteger n) $! (0,1,seed) -- we have to start again
			else
				let (bit,seed') = bitGenerator seed
				in (doRandomInteger n) $! (i `shiftL` 1 + toInteger bit, m `shiftL` 1, seed')


randomInt :: Int -> Int -> (Int,Int)
randomInt n seed
	| n <= 0     = error "randomInt: n <= 0"
	| otherwise  = doRandomInt n (0,1,seed)
	where
		doRandomInt n (i,m,seed) = 
			if m >= n -- i is now a random integer between 0 and m-1, where m is a power of 2
			then
				if i < n
				then (i,seed)
				else doRandomInt n (0,1,seed) -- we have to start again
				-- else (doRandomInt n) $! (0,1,seed) -- we have to start again
			else
				let (bit,seed') = bitGenerator seed
				in doRandomInt n (i `shiftL` 1 + bit, m `shiftL` 1, seed')
				-- in (doRandomInt n) $! (i `shiftL` 1 + bit, m `shiftL` 1, seed')

two48 = fromInteger (2^48) :: Double

random01 :: Int -> (Double,Int)
random01 seed =
	let (n,seed') = randomBitset 48 seed
	-- in (fromInteger n / (fromInteger (2^48)), seed')
	in (fromInteger n / two48, seed')
-- doubles appear to have a 49 bit mantissa



-- NORMAL (GAUSSIAN) DISTRIBUTION

-- returns two N(0,1) random variables
-- Box-Muller algorithm, from Robert and Casella p62

randomNormal01 seed =
	let
		(u1,seed') = random01 seed
		(u2,seed'') = random01 seed'
		u1' = 2 * u1 - 1
		u2' = 2 * u2 - 1
		s = u1' * u1' + u2' * u2'
	in
		if s <= 1
		then
			let z = sqrt (-2 * log s / s)
			in (z * u1', z * u2', seed'')
		else randomNormal01 seed''


-- FAST RANDOM (LOWER QUALITY)

a = 8121 :: Int
b = 28411 :: Int
m = 134456 :: Int
-- from Schneier p370
-- won't overflow, since 134455 * 8121 + 28411 = 1091937466 < 2^31

fastRandomInt :: Int -> Int -> (Int,Int)
fastRandomInt n seed =
	let seed' = (a*seed+b) `mod` m
	in (seed' `mod` n, seed')


