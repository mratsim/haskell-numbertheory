-- factoringecm.hs

module FactoringECM where

import NumberTheoryFundamentals (divides, extendedEuclid)
import MathsPrimitives (partialProducts)
import Primes


-- ELLIPTIC CURVE CODE
-- This code is based on module EllipticCurves
-- but with slight modifications to bail out when we find a factor

data EllipticCurve' = EC' Integer Integer Integer deriving (Eq, Show)
-- EC p a b represents the curve y^2 == x^3+ax+b over Fp

data EllipticCurvePt' = Inf' | P' Integer Integer deriving (Eq, Show)
-- P x y

-- Optim voir Mongoméry optim http://programmingpraxis.com/2010/04/23/modern-elliptic-curve-factorization-part-1/
isEltEC _ Inf' = True
isEltEC (EC' n a b) (P' x y) = (y*y - x*x*x - a*x - b) `mod` n == 0


-- Koblitz p34

-- assumes Fractional a
ecAdd' _ Inf' pt = Left pt
ecAdd' _ pt Inf' = Left pt
ecAdd' (EC' n a b) (P' x1 y1) (P' x2 y2)
	| x1 /= x2 =
		let
			(u,v,d) = extendedEuclid n ((x1-x2) `mod` n)  -- we're expecting d == 1, v == 1/(x1-x2) (mod n)
			m = (y1-y2) * v `mod` n
			x3 = (m*m - x1 - x2) `mod` n
			y3 = (- y1 + m * (x1 - x3)) `mod` n
		in if d == 1 then Left (P' x3 y3) else Right d
	| x1 == x2 =
		if (y1 + y2) `mod` n == 0  -- includes the case y1 == y2 == 0
		then Left Inf'
		else
			let
				(u,v,d) = extendedEuclid n ((2*y1) `mod` n)  -- we're expecting d == 1, v == 1/(2*y1) (mod n)
				m = (3 * x1 * x1 + a) * v `mod` n
				x3 = (m*m - 2*x1) `mod` n
				y3 = (- y1 + m * (x1 - x3)) `mod` n
			in if d == 1 then Left (P' x3 y3) else Right d

-- Note, it would be nice to avoid taking (x1-x2) `mod` n and (2*y1) `mod` n
-- but unfortunately extendedEuclid can return d -ve if either input is -ve
-- This could easily be fixed - if d -ve, change (u,v,d) -> (-u,-v,-d)

ecMult' _ 0 _ = Left Inf'
ecMult' ec k pt | k > 0 = doECMult Inf' pt k
	where
		-- doECMult p q n = p + n * q
		doECMult p _ 0 = Left p
		doECMult p q n =
			let p' = if odd n then ecAdd' ec p q else Left p
			    q' = ecAdd' ec q q
			in case (p',q') of
			(Left p'', Left q'') -> doECMult p'' q'' (n `div` 2)
			(Right _, _) -> p'
			(_, Right _) -> q'

discriminantEC' a b = 4 * a * a * a + 27 * b * b


-- FACTORIZATION CODE

-- We choose an elliptic curve E over Zn, and a point P on the curve
-- We then try to calculate kP, for suitably chosen k
-- What we are hoping is that at some stage we will fail because we can't invert an element in Zn
-- This will lead to finding a non-trivial factor of n


l n = exp (sqrt (log n * log (log n)))
-- L(n) is some sort of measure of the average smoothness of numbers up to n
-- # [x <= n | x is L(n)^a-smooth] = n L(n)^(-1/2a+o(1))  -- Cohen p 482

ecTrial ec@(EC' n a b) ms pt =
	let d = gcd n (discriminantEC' a b)
	in if d == 1 then doECTrial ec ms pt else Right d
	where
		doECTrial ec [] pt = Left pt
		doECTrial ec (m:ms) pt = case ecMult' ec m pt of
			Left pt' -> doECTrial ec ms pt'
			Right d -> Right d

-- q is the largest prime we're looking for - normally sqrt n
-- the b figure here is from Cohen p488
multipliers q = [largestPowerLE p b | p <- (2:[3,5..b]), isPrime p]
	where b = round ((l q) ** (1/sqrt 2))
	      largestPowerLE p m = last (takeWhile (<= m) (iterate (*p) p))

findFactorECM n | gcd n 6 == 1 =
	let ms = multipliers (sqrt n')
	in doFindFactor [ecTrial (EC' n a 1) ms (P' 0 1) | a <- [1..] ]
	where
		n' = fromInteger n
		doFindFactor ((Left _) : ts) = doFindFactor ts
		doFindFactor ((Right m) : ts) =
			if n `divides` m   -- this can happen, for example if the problem was that the discriminant was divisible by n
			then doFindFactor ts
			else m


-- OPTIMISED VERSION USING PARALLEL GCD
-- from Cohen
-- (Not fully implemented)

-- There is a possible optimisation where we can do several curves in parallel
-- (The point being that extendedEuclid is slow compared to multiplication mod n, and we can bundle up several extendedEuclids together at the expense of a few multiplications)
-- Not clear this would be a huge gain for smallish numbers
-- For n having 10 - 15 digits, we tend to need only 1 or a few curves
-- However, for n having 25 - 30 digits, we tend to need about a hundred curves
-- Even if we need only 1 curve, it might be that working in parallel will find our divisor sooner on one of the curves

recipMod a n = let (u,v,d) = extendedEuclid a n in if d == 1 then Left u else Right d


recipModParallel as n =
	let
		cs = map (`mod` n) (partialProducts as) -- can be optimised
		(u,v,d) = extendedEuclid (last cs) n
	in
		if d == 1
		then Left (reverse (computeInverses u (tail (reverse (1:cs))) (reverse as)))
		else Right (head [d | a <- as, d <- [gcd n a], d /= 1])
	where
		computeInverses u (c:cs) (a:as) = (u * c `mod` n) : computeInverses (u * a `mod` n) cs as
		computeInverses _ [] [] = []

-- recipMod sometimes returns -ve numbers, indicating that a (`mod` n) might be in order
-- alternatively we could just remove some of the (`mod` n)s from recipModParallel, and handle them later on in the code.
-- (but might not be a good idea as means our integer arithmetic is correspondingly slower)



-- ALTERNATIVE CODE

-- version which tells us how many curves it had to check
findFactorECMCount n | gcd n 6 == 1 =
	let ms = multipliers (sqrt n')
	in doFindFactor 1 [ecTrial (EC' n a 1) ms (P' 0 1) | a <- [1..] ]
	where
		n' = fromInteger n
		doFindFactor i ((Left _) : ts) = doFindFactor (i+1) ts
		doFindFactor i ((Right m) : ts) =
			if n `divides` m   -- this can happen, for example if the problem was that the discriminant was divisible by n
			then doFindFactor (i+1) ts
			else (m,i)

-- The c figure here is from Koblitz. However, he doesn't specify how to choose the b figure
multipliers2 q = [largestPowerLE p c | p <- (2:[3,5..b]), isPrime p]
	where b = round ((l q) ** (1/sqrt 2))
	      c = round (q + 1 + 2 * sqrt q)
	      largestPowerLE p m = last (takeWhile (<= m) (iterate (*p) p))

findFactorECM2 n | gcd n 6 == 1 =
	let ms = multipliers2 (sqrt n')
	in doFindFactor [ecTrial (EC' n a 1) ms (P' 0 1) | a <- [1..] ]
	where
		n' = fromInteger n
		doFindFactor ((Left _) : ts) = doFindFactor ts
		doFindFactor ((Right m) : ts) =
			if n `divides` m   -- this can happen, for example if the problem was that the discriminant was divisible by n
			then doFindFactor ts
			else m

