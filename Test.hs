import Numeric.Optimization.Algorithms.CMAES

-- a function with minimum at x[i]=sqrt(i)
f xs = sum $ zipWith g xs [1..]
  where
    g x i = (x * abs x - i)**2

-- a function with scale diversity
f2 xs = sum $ zipWith g xs [1..]
  where
    g x' i = let x = x'/1e10**i in
      (x * abs x - i)**2

main = do
  -- this works fine
  xs <- run $ minimizer [1..10] f
  print xs
  print $ f xs

  -- if the scale for parameters differ too much
  -- it doesn't work very well
  xs <-run $ minimizer [1..10] f2
  print xs
  print $ f2 xs

  -- you need to hint the optimizer with the
  -- appropriate scales
  xs <- run $
         (minimizer [1..10] f2)
           {scaling = Just [1e10**i | i<-[1..10] ] }
  print xs
  print $ f2 xs
