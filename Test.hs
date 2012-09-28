import Numeric.Optimization.Algorithms.CMAES

-- a function with minimum at x[i]=sqrt(i)
f xs = sum $ zipWith (\x i -> abs(x**2-i)) xs [1..]

-- a function with scale diversity
f2 xs = sum $ zipWith (\x i -> abs(x/1e4**i)**2-i)) xs [1..]

main = do
  -- this works fine
  xs <- run $ minimizer [1..10] f
  print xs

  -- if the scale for parameters differ too much
  -- it doesn't work very well
  xs <-run $ minimizer [1..10] f2
  print xs

  -- you need to hint the mimimizer with the
  -- appropriate scales
  xs <- run $
         (minimizer [1..10] f2)
           {scaling = Just [1e4**i | i<-[1..10] ] }
  print xs
