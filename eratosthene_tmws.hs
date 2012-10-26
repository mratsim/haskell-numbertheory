import Data.List
import System.Environment

{-# OPTIONS_GHC -O2 -fno-cse #-}
primesTMWE = 2:3:5:7: gapsW 11 wheel (joinT3 $ rollW 11 wheel primes')
  where
    primes' = 11: gapsW 13 (tail wheel) (joinT3 $ rollW 11 wheel primes')

pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t

gapsW k ws@(w:t) cs@(c:u) | k==c  = gapsW (k+w) t u
                          | True  = k : gapsW (k+w) t cs
rollW k ws@(w:t) ps@(p:u) | k==p  = scanl (\c d->c+p*d) (p*p) ws
                                      : rollW (k+w) t u
                          | True  = rollW (k+w) t ps
joinT3 ((x:xs): ~(ys:zs:t)) = x : union xs (union ys zs)
                                   `union` joinT3 (pairs t)
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

main = do
  args <- getArgs
  let lim = read $ head args
  print $ takeWhile (<lim) $ primesTMWE