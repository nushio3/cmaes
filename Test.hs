import Numeric.Optimization.Algorithms.CMAES

main = do
  minimize [1..10] (const 3)
  return ()
