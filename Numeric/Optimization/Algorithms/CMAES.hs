{-# LANGUAGE RankNTypes, RecordWildCards,ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}


{-|

Usage:

(1) create an optimization problem of type @Config@ by one of
    @minimize@, @minimizeIO@ etc.

(2) @run@ it.




Let's optimize the following function /f(xs)/. @xs@ is a vector and
@f@ has its minimum at @xs !! i = sqrt(i)@.

>>> let f = sum . zipWith (\i x -> (x*abs x - i)**2) [0..] :: [Double] -> Double
>>> let initXs = replicate 10 0                            :: [Double]
>>> bestXs <- run $ minimize f initXs
>>> f bestXs < 1e-10
True

If your optimization is not working well, try:

* Set @scaling@ in the @Config@ to the appropriate search
  range of each parameter.

* Set @tolFun@ in the @Config@ to the appropriate scale of
  the function values.

An example for scaling the function value:

>>> let f2 xs = (/1e100) $ sum $ zipWith (\i x -> (x*abs x - i)**2) [0..] xs
>>> bestXs <- run $ (minimize f2 $ replicate 10 0) {tolFun = Just 1e-111}
>>> f2 bestXs < 1e-110
True

An example for scaling the input values:

>>> let f3 xs = sum $ zipWith (\i x -> (x*abs x - i)**2) [0,1e100..] xs
>>> let xs30 = replicate 10 0 :: [Double]
>>> let m3 = (minimize f3 xs30) {scaling = Just (replicate 10 1e50)}
>>> xs31 <- run $ m3
>>> f3 xs31 / f3 xs30 < 1e-10
True

Use @minimizeT@ to optimize functions on traversable structures.

>>> import qualified Data.Vector as V
>>> let f4 = V.sum . V.imap (\i x -> (x*abs x - fromIntegral i)**2) :: V.Vector Double -> Double
>>> bestVx <- run $ minimizeT f4 $ V.replicate 10 0              
>>> f4 bestVx < 1e-10
True


Or use @minimizeG@ to optimize functions of almost any type. Let's create a triangle ABC
so that AB = 3, AC = 4, BC = 5.

>>> let dist (ax,ay) (bx,by) = ((ax-bx)**2 + (ay-by)**2)**0.5
>>> let f5 [a,b,c] = (dist a b - 3.0)**2 + (dist a c - 4.0)**2 + (dist b c - 5.0)**2 
>>> bestTriangle <- run $ (minimizeG f5 [(0,0),(0,0),(0,0)]){tolFun = Just 1e-20}
>>> f5 bestTriangle < 1e-10
True

Then the angle BAC should be orthogonal.

>>> let [(ax,ay),(bx,by),(cx,cy)] = bestTriangle
>>> abs ((bx-ax)*(cx-ax) + (by-ay)*(cy-ay)) < 1e-10
True


-}


module Numeric.Optimization.Algorithms.CMAES (
       run, Config(..), defaultConfig,
       minimize, minimizeIO,
       minimizeT, minimizeTIO
)where


import           Control.Monad hiding (forM_, mapM)
import qualified Control.Monad.State as State
import           Data.Data
import           Data.Generics 
import           Data.List (isPrefixOf)
import           Data.Maybe
import           Data.Foldable
import           Data.Traversable
import           System.IO
import           System.Process
import           Prelude hiding (concat, mapM, sum)

import Paths_cmaes

import Test.QuickCheck

prop :: Testable p => p -> IO ()
prop p = do
  r <- quickCheckWithResult (stdArgs{chatty=False}) p
  case r of
    Success _ _ _ -> putStrLn "Success"
    _ -> print r


-- | Optimizer configuration. @tgt@ is the type of the value to be
-- optimized.

data Config tgt = Config
  { funcIO        :: tgt -> IO Double
    -- ^ The Function to be optimized.
  , projection    :: tgt -> [Double]
    -- ^ Extract the parameters to be tuned from @tgt@.
  , embedding     :: [Double] -> tgt
    -- ^ Create a value of type @tgt@ from the parameters.
  , initXs        :: [Double]
    -- ^ An initial guess of the parameters.
  , sigma0        :: Double
    -- ^ The global scaling factor.
  , scaling       :: Maybe [Double]
    -- ^ Typical deviation of each input parameters.
  , typicalXs     :: Maybe [Double]
    -- ^ Typical mean of each input parameters.
  , tolFacUpX     :: Maybe Double
    -- ^ Terminate when one of the scaling grew too big
    -- (initial scaling was too small.)
  , tolUpSigma    :: Maybe Double
    -- ^ Terminate when the global scaling grew too big.
  , tolFun        :: Maybe Double
    -- ^ Terminate when the function value diversity in the current
    -- and last few generations is smaller than this value
  , tolStagnation :: Maybe Int
    -- ^ Terminate when the improvement is not seen for this number
    -- of iterations.
  , tolX          :: Maybe Double
    -- ^ Terminate when the deviations in the solutions are smaller
    -- than this value.
  , verbose       :: Bool
    -- ^ Repeat the CMA-ES output into stderr.
  }


-- | The default @Config@ values.
defaultConfig :: Config a
defaultConfig = Config
  { funcIO        = error "funcIO undefined"
  , projection    = error "projection undefined"
  , embedding     = error "embedding undefined"
  , initXs        = error "initXs undefined"
  , sigma0        = 0.25
  , scaling       = Nothing
  , typicalXs     = Nothing
  , tolFacUpX     = Just 1e10
  , tolUpSigma    = Just 1e20
  , tolFun        = Just 1e-11
  , tolStagnation = Nothing
  , tolX          = Just 1e-11
  , verbose       = False
  }


-- | Create a minimizing problem, given a pure function and an initial guess.
minimize :: ([Double]-> Double) -> [Double] -> Config [Double]
minimize f xs = minimizeIO (return . f) xs

-- | Create a minimizing problem, given an @IO@ function and an initial guess.
minimizeIO :: ([Double]-> IO Double) -> [Double] -> Config [Double]
minimizeIO fIO xs = 
  defaultConfig
  { funcIO     = fIO
  , initXs     = xs
  , projection = id
  , embedding  = id
  }

-- | Create a minimizing problem for a function on traversable structure @t@.
minimizeT :: (Traversable t) => (t Double-> Double) -> t Double -> Config (t Double)
minimizeT f tx = minimizeTIO (return . f) tx

-- | Create a minimizing problem for an effectful function on a traversable structure @t@.
minimizeTIO :: (Traversable t) => (t Double-> IO Double) -> t Double -> Config (t Double)
minimizeTIO fIO tx =
  defaultConfig
  { funcIO     = fIO
  , initXs     = proj tx
  , projection = proj
  , embedding  = embd
  }
  where
    proj = toList
    embd = zipTWith (\_ y -> y) tx

-- | Create a minimizing problem for a function on almost any type @a@ which contain Doubles.
minimizeG :: (Data a) => (a -> Double) -> a -> Config a
minimizeG f tx = minimizeGIO (return . f) tx

-- | Create a minimizing problem for an effectful function of almost any type.
minimizeGIO :: (Data a) => (a -> IO Double) -> a -> Config a
minimizeGIO fIO initA =
  defaultConfig
  { funcIO     = fIO
  , initXs     = getDoubles initA
  , projection = getDoubles
  , embedding  = flip putDoubles initA
  }

-- | Execute the optimizer and get the solution.
run :: forall tgt. Config tgt -> IO tgt
run Config{..} = do
  fn <- getDataFileName wrapperFn
  (Just hin, Just hout, _, _) <- createProcess (proc "python2" [fn])
    { std_in = CreatePipe, std_out = CreatePipe }
  sendLine hin $ unwords (map show initXs)
  sendLine hin $ show sigma0
  sendLine hin $ show $ length options
  forM_ options $ \(key, val) -> do
    sendLine hin key
    sendLine hin val
  let loop = do
      str <- recvLine hout
      let ws = words str
      case ws!!0 of
        "a" -> do
          return $ embedding $ map read $ drop 1 ws
        "q" -> do
          ans <- funcIO . embedding $ map read $ drop 1 ws
          sendLine hin $ show ans
          loop
        _ -> do
          fail "ohmy god"
  loop
    where
      options :: [(String, String)]
      options = concat $ map maybeToList
        [ "scaling_of_variables" `is` scaling
        , "typical_x"            `is` typicalXs
        , "tolfacupx"            `is` tolFacUpX
        , "tolupsigma"           `is` tolUpSigma
        , "tolfunhist"           `is` tolFun
        , "tolstagnation"        `is` tolStagnation
        , "tolx"                 `is` tolX
        ]

      is :: Show a => String -> Maybe a -> Maybe (String,String)
      is key = fmap (\val -> (key, show val))

      wrapperFn, commHeader :: String
      wrapperFn = "cmaes_wrapper.py"
      commHeader = "<CMAES_WRAPPER_PY2HS>"
      
      recvLine :: Handle -> IO String
      recvLine h = do
        str <- hGetLine h
        when (verbose) $ hPutStrLn stderr str
        if commHeader `isPrefixOf` str
          then return $ unwords $ drop 1 $ words str
          else do
            recvLine h
      
      sendLine :: Handle -> String -> IO ()
      sendLine h str = do
        hPutStrLn h str
        hFlush h

zipTWith :: (Traversable t1, Traversable t2) => (a->b->c) -> (t1 a) -> (t2 b) -> (t1 c)
zipTWith op xs0 ys0 = State.evalState (mapM zipper xs0) (toList ys0)
  where
    zipper x = do
      (y:ys) <- State.get
      State.put ys
      return (op x y)

{-|

getDoubles and putDoubles are generic functions used to put [Double] in and out
of generic data types. Let's test them.

>>> let d3 = (1,2,3) :: (Double,Int,Double)
>>> getDoubles d3
[1.0,3.0]
>>> putDoubles [4,5] d3
(4.0,2,5.0)

>>> let complicated = ([0,1],(2,[3,4])) :: ([Double],(Double,[Double]))
>>> getDoubles complicated
[0.0,1.0,2.0,3.0,4.0]
>>> putDoubles [5,6,7,8,9] complicated
([5.0,6.0],(7.0,[8.0,9.0]))

Putting back the obtained values should not change the data.

>>> type Complicated = ([[Double]],(),(([(Double,String)]),[Double]))
>>> prop ((\x -> putDoubles (getDoubles x) x == x) :: Complicated -> Bool)
Success

You can get the original list back after putting it.

>>> let make3 xs = take 3 $ xs ++ [0..]
>>> prop ((\xs' y -> let xs = make3 xs' in getDoubles (putDoubles xs y)==xs) :: [Double] -> (Double,Double,Double) -> Bool)
Success



-}

getDoubles :: Data d => d -> [Double]
getDoubles d = reverse $ State.execState (everywhereM getter d) []
  where
    getter :: GenericM (State.State [Double])
    getter a = do
      ys <- State.get
      let da = fmap (flip asTypeOf (head ys)) $ cast a
      case da of
        Nothing -> return a
        Just d -> do
          State.put $ d:ys
          return a

putDoubles :: Data d => [Double] -> d -> d
putDoubles ys0 d = State.evalState (everywhereM putter d) ys0
  where
    putter :: GenericM (State.State [Double])
    putter a = do
      ys <- State.get
      let ma' = (cast =<<) $ fmap (asTypeOf (head ys)) $ cast a
      case ma' of
        Nothing -> return a
        Just a' -> do
          State.put $ tail ys
          return a'