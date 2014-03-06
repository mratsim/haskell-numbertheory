import Data.Bits
import Data.List
import System.Environment

-- Modular Exponentiation by squaring http://eli.thegreenplace.net/2009/03/28/efficient-modular-exponentiation-algorithms/

expm :: Integer -> Integer -> Integer -> Integer
expm b e m = foldl' (\r (b', _) -> mod (r * b') m) 1 .
             filter (flip testBit 0 . snd) .
             zip (iterate (flip mod m . (^ 2)) b) .
             takeWhile (> 0) $ iterate (`shiftR` 1) e

rInteger :: String -> Integer
rInteger = read


main = do
  args <- getArgs
  let [a,b,c] = map rInteger args
  print $ expm a b c


-- Explanation :
-- takeWhile (> 0) $ iterate (`shiftR` 1) e
-- -> Store the quotient of successive division by 2 of the exponent
-- (flip mod m . (^ 2)) b
-- -> Calculate mod (b^2) m equivalent to (b^2 `mod` m)
-- ---> (flip mod m . (^ 2)) b = f(b)
-- zip (iterate (flip mod m . (^ 2)) b) . takeWhile (> 0) $ iterate (`shiftR` 1) e
-- ---> iterate [b,f b, f.f b, f.f.f b, ...]
-- ---> successive squaring of b
-- ---> zip [(b,e),(fb,e/2),(ffb,e/4),(fffb,e/8),...]
-- ---> successive squaring of b until exponent is null
-- filter (flip testBit 0 . snd)
-- -> remove even number from list
-- scanl g 1 [(a,b),(c,d),(e,f)]
--   -> [g 1 (a,b), g (g 1 (a,b)) (c,d), g(g(g 1 (a,b)) (c,d)) (e,f))]
-- foldl g 1 [(a,b),(c,d),(e,f)]
--   -> g(g(g 1 (a,b)) (c,d)) (e,f))
--   ---> (\r (b', _) -> mod (r * b') m) z [(x,y)]
--   -------> mod (z*x) m ; y is ignored

{-
Remco Niemeijer Says:
May 2, 2009 at 11:13 pm | Reply

The expm in my code is more complicated because I based it on the pseudocode in the wikipedia article on modular exponentiation, which is probably more appropriate for C than for Haskell. The expmod version in Codec.Encryption.RSA.NumberTheory uses the same algorithm you do, which is cleaner. It’s possible the one with bitshifting is slightly faster, but I can’t be sure without benchmarking.

-}

{-
The following is an example in pseudocode based on Applied Cryptography by Bruce Schneier.[1] The inputs base, exponent, and modulus correspond to b, e, and m in the equations given above.

function modular_pow(base, exponent, modulus)
    result := 1
    while exponent > 0
        if (exponent mod 2 == 1):
           result := (result * base) mod modulus
        exponent := exponent >> 1
        base = (base * base) mod modulus
    return result
    -}

