module Numeric.Optimization.Algorithms.CMAES ()where

optimize :: (Show a, Show b) => [(a,a)] -> ([a]-> b) -> IO [a]
optimize init f = optimizeIO init (return . f)

optimizeIO :: (Show a, Show b) => [(a,a)] -> ([a]-> IO b) -> IO [a]
optimizeIO = undefined
