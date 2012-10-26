import Data.Bits
import Data.List
import System.Environment


-- From Codec.Encryption.RSA.NumberTheory
expmod :: Integer -> Integer -> Integer -> Integer
expmod a x m |  x == 0    = 1
             |  x == 1    = a `mod` m
             |  even x    = let p = (expmod a (x `div` 2) m) `mod` m
                            in  (p^2) `mod` m
             |  otherwise = (a * expmod a (x-1) m) `mod` m


rInteger :: String -> Integer
rInteger = read


main = do
  args <- getArgs
  let [a,b,c] = map rInteger args
  print $ expmod a b c

-- Stack overflow if b is negative