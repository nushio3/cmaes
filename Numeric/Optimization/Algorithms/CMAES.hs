{-# LANGUAGE RankNTypes, RecordWildCards,ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}


{-|

Usage:

(1) create an optimization problem of type `Config` by one of
    `minimize`, `minimizeIO` etc.

(2) `run` it.

Let's optimize the following function /f(xs)/. @xs@ is a list of
Double and @f@ has its minimum at @xs !! i = sqrt(i)@.

>>> import Test.DocTest.Prop
>>> let f = sum . zipWith (\i x -> (x*abs x - i)**2) [0..] :: [Double] -> Double
>>> let initXs = replicate 10 0                            :: [Double]
>>> bestXs <- run $ minimize f initXs
>>> assert $ f bestXs < 1e-10

If your optimization is not working well, try:

* Set `scaling` in the `Config` to the appropriate search
  range of each parameter.

* Set `tolFun` in the `Config` to the appropriate scale of
  the function values.

An example for scaling the function value:

>>> let f2 xs = (/1e100) $ sum $ zipWith (\i x -> (x*abs x - i)**2) [0..] xs
>>> bestXs <- run $ (minimize f2 $ replicate 10 0) {tolFun = Just 1e-111}
>>> assert $ f2 bestXs < 1e-110

An example for scaling the input values:

>>> let f3 xs = sum $ zipWith (\i x -> (x*abs x - i)**2) [0,1e100..] xs
>>> let xs30 = replicate 10 0 :: [Double]
>>> let m3 = (minimize f3 xs30) {scaling = Just (repeat 1e50)}
>>> xs31 <- run $ m3
>>> assert $ f3 xs31 / f3 xs30 < 1e-10

Use `minimizeT` to optimize functions on traversable structures.

>>> import qualified Data.Vector as V
>>> let f4 = V.sum . V.imap (\i x -> (x*abs x - fromIntegral i)**2)

>>> :t f4
f4 :: V.Vector Double -> Double
>>> bestVx <- run $ minimizeT f4 $ V.replicate 10 0
>>> assert $ f4 bestVx < 1e-10

Or use `minimizeG` to optimize functions of any type that is Data
and that contains `Double`s. Here is an example that deal with
Triangles.

>>> :set -XDeriveDataTypeable
>>> import Data.Data
>>> data Pt = Pt Double Double deriving (Typeable,Data)
>>> let dist (Pt ax ay) (Pt bx by) = ((ax-bx)**2 + (ay-by)**2)**0.5
>>> data Triangle = Triangle Pt Pt Pt deriving (Typeable,Data)

Let us create a triangle ABC so that AB = 3, AC = 4, BC = 5.

>>> let f5 (Triangle a b c) = (dist a b - 3.0)**2 + (dist a c - 4.0)**2 + (dist b c - 5.0)**2
>>> let triangle0 = Triangle o o o where o = Pt 0 0
>>> :t f5
f5 :: Triangle -> Double
>>> bestTriangle <- run $ (minimizeG f5 triangle0){tolFun = Just 1e-20}
>>> assert $ f5 bestTriangle < 1e-10

Then the angle BAC should be orthogonal.

>>> let (Triangle (Pt ax ay) (Pt bx by) (Pt cx cy)) = bestTriangle
>>> assert $ abs ((bx-ax)*(cx-ax) + (by-ay)*(cy-ay)) < 1e-10


When optimizing noisy functions, set `noiseHandling` = @True@ (and
increase `noiseReEvals`) for better results.

>>> import System.Random
>>> let noise = randomRIO (0,1e-2)
>>> let f6Pure = sum . zipWith (\i x -> (x*abs x - i)**2) [0..]
>>> let f6 xs = fmap (f6Pure xs +) noise
>>> :t f6
f6 :: [Double] -> IO Double
>>> xs60 <- run $ (minimizeIO f6 $ replicate 10 0) {noiseHandling = False}
>>> xs61 <- run $ (minimizeIO f6 $ replicate 10 0) {noiseHandling = True,noiseReEvals=Just 10}
>>> -- assert $ f6Pure xs61 < f6Pure xs60

(note : the above assertion holds with probability of about 70%.)


-}


module Numeric.Optimization.Algorithms.CMAES (
       run, Config(..), defaultConfig,
       minimize, minimizeIO,
       minimizeT, minimizeTIO,
       minimizeG, minimizeGIO,
       getDoubles, putDoubles
)where


import           Control.Applicative ((<|>), (<$>))
import           Control.Monad hiding (forM_, mapM)
import qualified Control.Monad.State as State
import           Data.Data
import           Data.Generics
import           Data.List (isPrefixOf)
import           Data.Maybe
import           Data.Foldable
import           Data.Traversable
import           Safe (atDef, headDef)
import           System.IO
import qualified System.IO.Strict as Strict
import           System.IO.Unsafe(unsafePerformIO)
import           System.Process
import           Prelude hiding (concat, mapM, sum)

import Paths_cmaes


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
    -- The length of the list is adjusted to be the same as
    -- initXs, e.g. you can lazily use an infinite list here.
  , typicalXs     :: Maybe [Double]
    -- ^ Typical mean of each input parameters.
    -- The length of this list too, is adjusted to be the same as
    -- initXs.
  , noiseHandling :: Bool
    -- ^ Assume the function to be rugged and/or noisy
  , noiseReEvals  :: Maybe Int
    -- ^ How many re-evaluation to make to estimate the noise.
  , noiseEps      :: Maybe Double
    -- ^ Perturb the parameters by this amount (relative to sigma)
    -- to estimate the noise
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
  , otherArgs     :: [(String, String)]
    -- ^ Interfaces for passing other configuration arguments directly to
    -- @cma.py@
  }


-- | The default @Config@ values. Also consult the original document
-- <http://www.lri.fr/~hansen/pythoncma.html#-fmin> for default values
-- of the parameters not listed here.
defaultConfig :: Config a
defaultConfig = Config
  { funcIO        = error "funcIO undefined"
  , projection    = error "projection undefined"
  , embedding     = error "embedding undefined"
  , initXs        = error "initXs undefined"
  , sigma0        = 0.25
  , scaling       = Nothing
  , typicalXs     = Nothing
  , noiseHandling = False
  , noiseReEvals  = Nothing
  , noiseEps      = Just 1e-7
  , tolFacUpX     = Just 1e10
  , tolUpSigma    = Just 1e20
  , tolFun        = Just 1e-11
  , tolStagnation = Nothing
  , tolX          = Just 1e-11
  , verbose       = False
  , otherArgs     = []
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



-- | Silently check for python version and place the correct shebang 
wrapperFnFullPath :: FilePath
wrapperFnFullPath = unsafePerformIO $ do
  fullFn <- getDataFileName wrapperFn
  (_,hin,_,hproc) <- runInteractiveCommand "python --version"
  str <- hGetContents hin
  _ <- waitForProcess hproc
  let pythonVersion :: Int
      pythonVersion = read $ take 1 $ atDef "2" (words str) 1 
  
      correctShebang
        | pythonVersion == 2 = "#!/usr/bin/env python"
        | otherwise          = "#!/usr/bin/env python2"

  wrapperLines <- lines <$> Strict.readFile fullFn
    
  when (headDef "" wrapperLines /= correctShebang) $ do
    writeFile fullFn $ unlines $ correctShebang : drop 1 wrapperLines

  return fullFn

  where
    wrapperFn = "cmaes_wrapper.py"

{-# NOINLINE wrapperFnFullPath #-}



-- | Execute the optimizer and get the solution.
run :: forall tgt. Config tgt -> IO tgt
run Config{..} = do
  (Just hin, Just hout, _, hproc) <- createProcess (proc "python2" [wrapperFnFullPath])
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
  r <- loop
  _ <- waitForProcess hproc
  return r
    where
      probDim :: Int
      probDim = length initXs

      adjustDim :: [a] -> [a] -> [a]
      adjustDim supply orig =
        take probDim $
        catMaybes $
        zipWith (<|>)
          (map Just orig ++ repeat Nothing)
          (map Just supply)

      options :: [(String, String)]
      options = concat $ map maybeToList
        [ "scaling_of_variables" `is` (fmap$adjustDim [1..] ) scaling
        , "typical_x"            `is` (fmap$adjustDim initXs) typicalXs
        , "noise_handling"       `is` Just noiseHandling
        , "noise_reevals"        `is` noiseReEvals
        , "noise_eps"            `is` noiseEps
        , "tolfacupx"            `is` tolFacUpX
        , "tolupsigma"           `is` tolUpSigma
        , "tolfunhist"           `is` tolFun
        , "tolstagnation"        `is` tolStagnation
        , "tolx"                 `is` tolX
        ] ++ [otherArgs]

      is :: Show a => String -> Maybe a -> Maybe (String,String)
      is key = fmap (\val -> (key, show val))

      commHeader :: String
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

>>> import Test.DocTest.Prop
>>> type Complicated = ([[Double]],(),(([(Double,String)]),[Double]))
>>> prop ((\x -> putDoubles (getDoubles x) x == x) :: Complicated -> Bool)

You can get the original list back after putting it.

>>> let make3 xs = take 3 $ xs ++ [0..]
>>> prop ((\xs' y -> let xs = make3 xs' in getDoubles (putDoubles xs y)==xs) :: [Double] -> (Double,Double,Double) -> Bool)



-}

getDoubles :: Data a => a -> [Double]
getDoubles d = reverse $ State.execState (everywhereM getter d) []
  where
    getter :: GenericM (State.State [Double])
    getter a = do
      ys <- State.get
      let da = fmap (flip asTypeOf (head ys)) $ cast a
      case da of
        Nothing -> return a
        Just dd -> do
          State.put $ dd:ys
          return a

putDoubles :: Data a => [Double] -> a -> a
putDoubles ys0 d = State.evalState (everywhereM putter d) ys0
  where
    putter :: GenericM (State.State [Double])
    putter a0 = do
      ys <- State.get
      let ma1 = (cast =<<) $ fmap (asTypeOf (head ys)) $ cast a0
      case ma1 of
        Nothing -> return a0
        Just a1 -> do
          State.put $ tail ys
          return a1