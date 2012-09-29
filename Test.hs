module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-idist/build/autogen/", "./Numeric/Optimization/Algorithms/CMAES.hs"] 
