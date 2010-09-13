-- factoringCFRAC.hs

module FactoringCFRAC where

import RedBlackTree
import MergeSort
import Primes
import NumberTheoryFundamentals (splitWith, extendedEuclid, intSqrt, legendreSymbol)
import ContinuedFraction (convergentsForSqrt)


-- FACTORING (LARGE PRIMES)

-- The algorithm is as follows:

-- 1. Find some numbers b s.t b*b mod n is small (or sometimes small and negative)
-- (Fermat's method would be to try numbers around intSqrt (k*n) for small k)
-- The continued fraction method uses the fact that continued fractions can provide good rational approximations to sqrt (k*n)

-- 2. For each b we have found, reduce b*b mod n to a factor base of small primes
-- (eg b1 -> p1^2*p2, b2 -> p3*p4^2, b3 -> p2*p3 etc)

-- (now we regard these reductions as vectors over F2, eg p2^2*p3 = (0,2,1,0,0,...) )
-- 3. Perform gaussian elimination on the vectors over the factor base, until we have a product of even powers of small primes
-- (eg b1*b2*b3 -> p1^2 * p2^2 * p3^2 * p4^2)

-- 4. Then we have x, a product of bis, and y, a product of pis, s.t x^2 = y^2 (mod n) - we are hoping, x /= +/- y (mod n)
-- (for example, we have (b1*b2*b3)^2 = (p1*p2*p3*p4)^2 )
-- Then we have x^2 - y^2 = 0 (mod n), so (x+y)(x-y) = kn.
-- What we're hoping is that either gcd (x+y,n) or gcd (x-y,n) will give us a factor
-- (It's not guaranteed, as we may find x+y = n, x-y = k, for example)



-- CONTINUED FRACTION CANDIDATES
-- We use the continued fraction expansion of sqrt kn to find numbers x s.t. x^2 `mod` n is small ( o(sqrt n) )

-- returns the least absolute residue, ie using a small negative number rather than a large positive number where possible
lmod x n = let x' = x `mod` n in if x' + x' < n then x' else x' - n

cfracCandidates k n = [(b `mod` n, (b*b) `lmod` n) | (_,(b,c)) <- convergentsForSqrt (k*n)]
-- b/c is an approximation to sqrt kn
-- hence we expect b*b - k*n*c*c == b*b `lmod` n to be small


-- REDUCTION TO FACTOR BASE


reduceToFactorBase :: [Integer] -> Integer -> (Integer, [(Integer,Int)])
-- the factor base should be a list of small primes
reduceToFactorBase fbase m
	| m > 0  = doReduceToFactorBase fbase m []
	| m < 0  = doReduceToFactorBase fbase (-m) [(-1,1)]
	where
		doReduceToFactorBase ps 1 factors = (1, factors)
		doReduceToFactorBase [] m factors = (m, factors)
		doReduceToFactorBase (p:ps) m factors =
			let (s, m') = m `splitWith` p
			in
				if (s>0)
				then doReduceToFactorBase ps m' ((p,s):factors)
				else doReduceToFactorBase ps m factors



-- This value is actually for MPQS (from Landquis)
-- However, brief experimentation shows it is good for CFRAC too
optimalFactorBase n = fromInteger (round (exp (sqrt (log n' * log (log n'))) ** (sqrt 2 / 4))) :: Int
	where n' = fromInteger n

factorBase k n = take b [p | p <- primesTo10000 ++ filter isPrime [10001,10003..], legendreSymbol (k*n) p `elem` [0,1]]
	where b = optimalFactorBase n


-- CONSTRUCTING OUR TABLE OF FACTORS

-- What we need to do now is take the cfracCandidates, reduce them to a factor base
-- until the number of candidates we have exceeds the number of factors we're using from the factor base
-- at that point we can expect to have linear dependencies between entries in the table


constructFactorTable k n =
	let
		fbase = factorBase k n
		candidates = [(b,factors) | (b,b2) <- cfracCandidates k n, (unfactored,factors) <- [reduceToFactorBase fbase b2], unfactored == 1]
	in doConstructFactorTable [] rbempty candidates
	where
		doConstructFactorTable table factors (entry@(b,ps):entries) =
			if length table > rbcount factors
			then reverse table -- !! reverse is temporary to help debugging
			else
				let factors' = foldl rbinsert factors (map fst ps)
				in doConstructFactorTable (entry:table) factors' entries
-- rather than counting the factors, we could just use the size of the factor base as our stopping point
-- test to see if it makes a significant difference



-- Cohen p479 says that k must be square-free, and kn = 0 or 1 (mod 4)
-- reverse order of last two clauses for slight speed improvement
allFactorTables n = [constructFactorTable k n | k <- [1..], isSquareFree k, k * n `mod` 4 `elem` [0,1] ]


-- GAUSSIAN ELIMINATION

-- We now regard [(b,[(p,a)])] as a sparse matrix over F2 (where p is the column index, and a `mod` 2 is the value)

-- For Gaussian elimination, we should choose the column having the least non-zero entries, and pivot on one of the rows having a non-zero entry in that column
-- We approximate this by pivoting on rows having the largest prime factor first
-- !! We could almost certainly do better by implementing this in a more sophisticated way

-- Calculate the weights of the columns (ie the number of non-zero entries)
columnWeights t = doAnalyzeFactorTable rbempty t
	where
		doAnalyzeFactorTable factors [] = rbtolist factors
		doAnalyzeFactorTable factors ((b,ps):rows) =
			let factors' = foldl (\rb (p,a) -> if even a then rb else rb `rbaddupdate` (p,1)) factors ps
			in doAnalyzeFactorTable factors' rows


-- Remove columns of weight 1, and the rows in which those columns have non-zero coefficients
-- (In other words, if there is a p, such that only one row contains (p,a) with a odd, then remove that row)
removeWeight1s ftable = doRemoveSingletons1 rbempty ftable
	where
		doRemoveSingletons1 factors ((b,ps):rows) =
			let factors' = foldl (doupdate b) factors ps
			in doRemoveSingletons1 factors' rows
		doRemoveSingletons1 factors [] = doRemoveSingletons2 factors
		doupdate b rb (p,a)
			| even a = rb
			| otherwise = case rb `rblookup` p of
				Nothing -> rb `rbupdate` (p,Just b)           -- this prime hasn't been seen before
				Just (Just b') -> rb `rbupdate` (p,Nothing)   -- this prime has been seen only once before
				Just Nothing -> rb                            -- this prime has been seen more than once already
		doRemoveSingletons2 factors =
			let singletonrows = filter (/= Nothing) (rbvalues factors)
			in filter (\(b,ps) -> Just b `notElem` singletonrows) ftable


addRow n (b,ps) (b',ps') = (b*b' `mod` n, multps ps ps')
	where
		multps as [] = as
		multps [] bs = bs
		multps ((p1,a1):ps) ((p2,a2):qs)
			| p1 > p2 = (p1,a1) : multps ps ((p2,a2):qs)
			| p1 == p2 = (p1,a1+a2) : multps ps qs
			| p1 < p2 = (p2,a2) : multps ((p1,a1):ps) qs


orderRows rows = map snd (mergeSort' (>) (map (\(b,ps) -> (largestPrime ps ,(b,ps)) ) rows))
	where
		largestPrime [] = 100000000 -- ie we want this row ordered before any other, as it is already reduced
		largestPrime ((p,a):pas) = if odd a then p else largestPrime pas


gaussElim _ [] = []
gaussElim n ((b,ps):rows) =
	if all (\(p,a) -> even a) ps
	then (b,ps) : gaussElim n rows
	else
		let (p,_) = head (filter (\(p,a) -> odd a) ps)  -- ie the last prime with odd exponent
		in gaussElim n (map (\row@(_,qs) -> if qs `hasOddPower` p then addRow n row (b,ps) else row) rows)
	where
		hasOddPower ((q,a):qs) p
			| q > p  = hasOddPower qs p
			| q == p = odd a
			| otherwise = False
		hasOddPower [] _ = False


-- FINDING A FACTOR
-- putting it all together, to use the results of Gaussian elimination to find a factor

-- find a factor once we have a (b,ps) with all exponents even
findFactorFromPair n (b,ps) =
	let y = product [p ^ (a `div` 2) | (p,a) <- ps] `mod` n
	in gcd n (b+y)

factorSplits n = map (findFactorFromPair n) (concat (map ( (gaussElim n) . orderRows . removeWeight1s) (allFactorTables n)))


findFactorCFRAC n = let n' = intSqrt n in if n == n' * n' then n' else findFactorCFRAC' n
-- we perform this test because the continued fraction code doesn't work if n is a perfect square

findFactorCFRAC' n = head [d | d <- factorSplits n, d /= 1, d /= n]




-- LARGE PRIME VARIATION

-- At the moment, if our b^2 `mod` n doesn't factor completely over our factor base, we throw it away
-- In the large prime variation, then if the unfactored part is a prime, we don't throw it away
-- What we do instead is store it in a separate large prime table
-- (Suppose b^2 `mod` n = ps * q, where q is the large prime - then we store (q, (b,ps)) in our rbtree)
-- If we ever get a collision, ie (q,(b,ps)) is already in the table, and we try to add (q,(b',ps'))
-- then we take both out of the table, and combine them to get an entry for the main table
-- b^2 = ps * q, and b'^2 = ps' * q,
-- so (b*b'/q)^2 = ps * ps' (mod n)
-- (we can calculate 1/q using extended Euclid)

constructFactorTableLP k n =
	let
		fbase = factorBase k n
		candidates = [(unfactored,(b,factors)) | (b,b2) <- cfracCandidates k n, (unfactored,factors) <- [reduceToFactorBase fbase b2] ]
	in doConstructFactorTable [] rbempty rbempty candidates
	where
		doConstructFactorTable table factors lptable (entry@(unfactored,(b,ps)):entries)
			| unfactored == 1 =
				if length table > rbcount factors
				then reverse table -- !! reverse is temporary to help debugging
				else
					let factors' = foldl rbinsert factors (map fst ps)
					in doConstructFactorTable ((b,ps):table) factors' lptable entries
			| isPrime unfactored = case lptable `rblookup` unfactored of
				Nothing ->
					let lptable' = lptable `rbupdate` (unfactored,[(b,ps)])
					in doConstructFactorTable table factors lptable' entries
				Just bps ->
					let
						lptable' = lptable `rbupdate` (unfactored,(b,ps):bps)
						tableadditions = map (lpentryfrom n unfactored (b,ps)) bps
						table' = tableadditions ++ table
					in doConstructFactorTable table' factors lptable' entries
			| otherwise = doConstructFactorTable table factors lptable entries
-- !! we don't check for duplicates when adding tableadditions - perhaps we should

lpentryfrom n q (b1,ps1) (b2,ps2) =
	let (u,v,d) = extendedEuclid n q  -- so un + vq = 1, so v is inverse of q mod n
	in
		if d == 1
		then
			let (b',ps') = addRow n (b1,ps1) (b2,ps2)
			in (b' * v `mod` n, ps')
		else error ("lpentryfrom: found factorization early: " ++ show d)

allFactorTablesLP n = [constructFactorTableLP k n | k <- [1..], isSquareFree k, k * n `mod` 4 `elem` [0,1] ]

factorSplitsLP n = map (findFactorFromPair n) (concat (map ( (gaussElim n) . orderRows . removeWeight1s) (allFactorTablesLP n)))

findFactorLP n = head [d | d <- factorSplitsLP n, d /= 1, d /= n]

