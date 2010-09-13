-- mathsprimitives.hs

module MathsPrimitives where

-- primitive operations on sequences (lists) of numbers
-- used in implementation of vectors, matrices, polynomials, cyclotomic fields, etc

import List (transpose)

infixr 8  */, *//
infixl 7  $*, $., $$*
infixl 6  $+, $-, $$+, $$-


-- addition of sequences
(a:as) $+ (b:bs) = (a+b) : (as $+ bs)
as     $+ []     = as
[]     $+ bs     = bs

as $- bs = as $+ (map negate bs)

-- scalar multiplication
a */ bs = map (a*) bs


-- polynomial multiplication
[] $* _ = []
_ $* [] = []
(a:as) $* (b:bs) = [a*b] $+ shift (map (a*) bs $+ map (*b) as) $+ shift (shift (as $* bs))

shift [] = []
shift as = 0 : as


-- dot product of vectors (also called inner or scalar product)
u $. v = sum (zipWith (*) u v)


-- tensor product of vectors (also called outer or matrix product)
(a:as) $** v = map (a*) v : (as $** v)
[] $** _ = []



-- matrix operations

a $$+ b = zipWith (zipWith (+)) a b

a $$- b = zipWith (zipWith (-)) a b

a $$* b = doMultMx a (transpose b)
	where
		doMultMx [] _ = []
		-- doMultMx (u:us) bT = map (u $.) bT : doMultMx us bT
		doMultMx (u:us) bT = ((:) $! (map (u $.) bT)) (doMultMx us bT)

-- scalar multiplication
k *// m = map (map (k*)) m


fMatrix f n = [[f i j | j <- [1..n]] | i <- [1..n]] 


partialSums xs = scanl1 (+) xs

partialProducts xs = scanl1 (*) xs

factorials :: [Integer]
factorials = scanl (*) 1 [1..]


-- A class for types which represent mathematical functions
class FunctionRep f where
	compose :: f -> f -> f
	deriv :: f -> f
	integ :: f -> f
	nthderiv :: Int -> f -> f
	nthderiv n f = iterate deriv f !! n






{-
-- action on the left
[] <. _ = []
(row:rows) <. xs =
	sum (zipWith (*) row xs) : (rows <. xs)

-- action on the right
v .> m = doApplyRightMx [] v m
	where
		doApplyRightMx ys [] [] = foldl1 (zipWith (+)) ys
		doApplyRightMx ys (x:xs) (row:rows) = doApplyRightMx (map (x *) row : ys) xs rows

-}