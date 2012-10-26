import System.Environment
import Data.List
import Control.Monad.State (forM_,when)
import Control.Monad.Primitive (PrimState)
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU

main = do
  args <- getArgs
  let lim = read $ head args
--  v <- (VUM.replicate lim False) >>= VU.unsafeFreeze >>= (return . VU.toList)
--  print $ primes_dualfeed lim
--  print $ (2:).(3:) $ VU.toList $ VU.elemIndices True $ candidates lim
  print $(2:).(3:). VU.toList $ VU.elemIndices True $ sieve $ candidates lim

vecInv :: Int -> VU.Vector Bool -> VU.Vector Bool
vecInv i v  = runST $ do
  u <- VU.unsafeThaw v
  VUM.unsafeRead u i >>= (return.not) >>=(VUM.unsafeWrite u i)
  u <- VU.unsafeFreeze u
  return u


isqrt = floor . sqrt . fromIntegral


candidates limit = candidates' limit 1 [(x,y) | x <- [1..isqrt limit], y <- [1..isqrt limit]]
 			      $ (VU.replicate limit False)
   where candidates' _ _ [] bvec 			      = bvec
	 candidates' lim step ((x,y):xys) bvec
	   | step ==1 && s1 <= lim && (m1 ==  1 || m1 ==5)    = candidates' lim 2 ((x,y):xys) (vecInv s1 bvec)
	   | step ==1 				      	      = candidates' lim 2 ((x,y):xys) bvec
	   | step ==2 && s2 <= lim && m2 == 7	              = candidates' lim 3 ((x,y):xys) (vecInv s2 bvec)
	   | step ==2					      = candidates' lim 3 ((x,y):xys) bvec
	   | step ==3 && s3 <= lim && x>y && m3 == 11	      = candidates' lim 1 xys (vecInv s3 bvec)
	   | otherwise					      = candidates' lim 1 xys bvec
		  where
		    s1 = 4*x*x + y*y
		    s2 = 3*x*x + y*y
		    s3 = 3*x*x - y*y
		    m1 = s1 `rem` 12
		    m2 = s2 `rem` 12
		    m3 = s3 `rem` 12

------v3 ultrafast, low memory footprint
removeSquares :: VU.MVector (PrimState (ST s)) Bool -> Int -> ST s ()
removeSquares v i = do
  forM_ [i*i,2*i*i..VUM.length v] $ \j -> do
    VUM.unsafeWrite v j False

sieve vcand = runST $ do
  u <- VU.unsafeThaw vcand
  limn <- return (VUM.length u) >>= (return.isqrt)
  forM_ [0..limn] $ \i -> do
    VUM.unsafeRead u i >>= (flip when (removeSquares u i))
  u <- VU.unsafeFreeze u
  return u


{-
------v1 faster less memory
primes_dualfeed lim = 2 : 3:((VU.toList $ VU.elemIndices True $ candidates lim) `minus` joinL [[p*p, 2*p*p..] | p <- primes'])
  where
    primes' = 3 : 5 : ((tail $ VU.toList $ VU.elemIndices True $ candidates lim) `minus` joinL [[p*p, 2*p*p..] | p <- primes'])

joinL ((x:xs):t) = x : fuse xs (joinL t)

minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

fuse (x:xs) (y:ys) = case (compare x y) of
           LT -> x : fuse  xs  (y:ys)
           EQ -> x : fuse  xs     ys
           GT -> y : fuse (x:xs)  ys
fuse  xs     []    = xs
fuse  []     ys    = ys


-----v2 a bit slower, 3x more memory usage
squarefree _ [] = []
squarefree lim xs'@(x:xs)
    | x <= lim        = (x:) . squarefree lim . mergeRemove xs
			. takeWhile (<=lim) $ [ k*x*x | k <- [1..] ]
    | otherwise	      = xs'

mergeRemove [] ys = []
mergeRemove xs [] = xs
mergeRemove xs'@(x:xs) ys'@(y:ys)
    = case compare x y of
        EQ -> mergeRemove xs ys
        LT -> x : mergeRemove xs ys'
        GT -> mergeRemove xs' ys-}
