-- numbertheoryfundamentals.hs

module NumberTheoryFundamentals where

-- Ch 1 of Cohen, A Course in Computational Algebraic Number Theory


import Bits (shiftL, shiftR)
import RandomGenerator (randomInteger) -- for sqrts mod p


-- BASICS

d `divides` n = n `mod` d == 0


-- splitWith :: Integer -> Integer -> (Int, Integer)
-- n `splitWith` p = (s,t), where n = p^s * t
n `splitWith` p = doSplitWith 0 n
	where doSplitWith s t
		| p `divides` t = doSplitWith (s+1) (t `div` p)
		| otherwise     = (s, t)


-- calculate x^n in a (semi)group
power (idG,multG) x n = doPower idG x n
	where
		doPower y _ 0 = y
		doPower y x n =
			let y' = if odd n then (y `multG` x) else y
			    x' = x `multG` x
			    n' = n `div` 2
			in doPower y' x' n'


-- EXTENDED EUCLID

-- extendedEuclid a b returns (u,v,d) such that u*a + v*b = d

extendedEuclid a b | a >= 0 && b >= 0 = extendedEuclid' a b

extendedEuclid' a b = doExtendedEuclid a b []
	where
		doExtendedEuclid d 0 qs = let (u,v) = unwind 1 0 qs in (u,v,d)
		doExtendedEuclid a b qs = let (q,r) = quotRem a b in doExtendedEuclid b r (q:qs)
		unwind u v [] = (u,v)
		unwind u v (q:qs) = unwind v (u-v*q) qs


-- from Cohen, p16
-- appears to be slightly slower than the above
extendedEuclidCohen d 0 | d > 0 = (1,0,d)
extendedEuclidCohen a b | a >= 0 && b >= 0 = doExtendedEuclid a b a 1 0 b
	where
		doExtendedEuclid a b d u _ 0 = (u, (d-a*u) `div` b, d)
		doExtendedEuclid a b d u v1 v3 =
				let
					(q,t3) = quotRem d v3
					t1 = u - q * v1
				in doExtendedEuclid a b v3 v1 t1 t3



-- INTEGER SQUARE ROOT

intSqrt :: Integer -> Integer
intSqrt 0 = 0
intSqrt n = newtonianIteration n (findx0 n 1)
	where
		-- find x0 == 2^(a+1), such that 4^a <= n < 4^(a+1).
		findx0 a b = if a == 0 then b else findx0 (a `shiftR` 2) (b `shiftL` 1)
		newtonianIteration n x =
			let x' = (x + n `div` x) `div` 2
			in if x' < x then newtonianIteration n x' else x



minus1to n = if even n then 1 else -1


-- LEGENDRE SYMBOL

-- from Koblitz, A Course in Number Theory and Cryptography 47-8
-- see also Cohen p29,30

-- Legendre symbol, via Jacobi symbol
-- returns 0 if p divides a, 1 if a is a square mod p, -1 if a is not a square mod p
legendreSymbol a p | p > 0 && odd p = legendreSymbol' a p
legendreSymbol a 2 = a `mod` 2

-- strictly speaking, legendreSymbol is only defined for odd p
-- we define it for p == 2 as a convenience
-- !! note that in some applications you should use the Kronecker symbol instead, which gives a *different* answer for p == 2

legendreSymbol' a p = if a' == 0 then 0 else twoSymbol * oddSymbol
	where
		a' = a `mod` p                                                          -- hence a' >= 0
		(s,q) = a' `splitWith` 2                                                -- a' == 2^s * q, hence (a'/p) == (2/p)^s * (q/p)
		twoSymbol = if (p `mod` 8) `elem` [1,7] then 1 else minus1to s          -- (2/p) == (-1)^((p^2-1)/8), p odd
		oddSymbol = if q == 1 then 1 else qrMultiplier * legendreSymbol' p q
		qrMultiplier = if p `mod` 4 == 3 && q `mod` 4 == 3 then -1 else 1       -- == (-1)^((p-1)*(q-1)/4), p, q odd
-- a slight optimisation would be to use .&. 7 an .&. 3 instead of `mod` 8 and `mod` 4

legendreSymbolTest a p =
	if p `divides` a
	then 0
	else if or [(x*x-a) `mod` p == 0 | x <- [1..p `div` 2]] then 1 else -1
-- !! brute force method - for testing purposes only


-- KRONECKER SYMBOL

-- Cohen p28

kroneckerSymbol a 0 = if a `elem` [-1,1] then 1 else 0
kroneckerSymbol a b = kroneckerSign * kroneckerTwo * kroneckerOdd
	where
		(b2s,bOdd) = (abs b) `splitWith` 2
		kroneckerSign = if b < 0 && a < 0 then -1 else 1
		kroneckerTwo
			| even a  = if b2s == 0 then 1 else 0                             -- (a/2) == 0, a even
			|  odd a  = if (a `mod` 8) `elem` [1,7] then 1 else minus1to b2s  -- (a/2) == (-1)^((a^2-1)/8), a odd
		kroneckerOdd
			| bOdd == 1  = 1
			| otherwise  = legendreSymbol' a bOdd

-- Cohen p29 has an algorithm which may be faster


-- SQRTS MOD P

-- based on Cohen p32-3
-- note that this implementation is poor when p-1 is divisible by a large power of 2
-- Koblitz, A Course in Number Theory and Cryptography, 48-9, explains how to fix this (so does Cohen, but rather incomprehensibly)

sqrtsmodp a p = sqrtsmodp' (a `mod` p) p

sqrtsmodp' 0 _ = [0]
sqrtsmodp' 1 2 = [1]
sqrtsmodp' a p =
	let
		(e,q) = (p-1) `splitWith` 2
		z = findSylow2Generator q
		zpowers = take (2^(e-1)) (iterate (\(z_k,z_2k)->(z * z_k `mod` p ,z * z * z_2k `mod` p)) (1,1))
		a_q = power (1, \x y -> x*y `mod` p) a q -- powerMod p a q
	in case (filter (\(_,z_2k) -> a_q * z_2k `mod` p == 1) zpowers) of
		[] -> []
		(z_k,_):_ -> let x = power (1, \x y -> x*y `mod` p) a ((q+1) `div` 2) * z_k `mod` p in ascendingPair [x,p-x]
		-- (z_k,_):_ -> let x = powerMod p a ((q+1) `div` 2) * z_k `mod` p in ascendingPair [x,p-x]
	where
		findSylow2Generator q =
			let nonresidue = head [n | (n,_) <- iterate (\(_,seed) -> randomInteger p seed) (0,342349871), legendreSymbol n p == -1]
			in power (1, \x y -> x*y `mod` p) nonresidue q -- powerMod p nonresidue q

ascendingPair [x,y] = if x < y then [x,y] else [y,x]

sqrtsmodptest a p = [x | x <- [0..p-1], (x*x-a) `mod` p == 0]
