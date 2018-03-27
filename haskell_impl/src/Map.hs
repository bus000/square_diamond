{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns     #-}
module Map
    ( createMap
    , saveMap
    , HeightMap(..) -- TODO: Remove constructor.
    ) where

import ClassyPrelude
import qualified Control.Monad as C
import qualified Control.Monad.Random as C
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa.Algorithms.Pixel as R
import qualified Data.Array.Repa.Shape as R
import qualified Data.Array.Repa.IO.DevIL as R
import qualified Prelude
import qualified Data.Vector.Unboxed as V

data HeightMap = HeightMap (R.Array R.U R.DIM2 Double)

createMap :: C.MonadRandom m => Int -> m HeightMap
createMap !n = do
    randoms <- cornerPositive <$> randomArray (R.ix2 sideLen sideLen) (-1.0) 1.0
    randoms <- R.computeP randoms

    HeightMap <$> C.foldM step randoms stepSizes
  where
    sideLen = (2^n) + 1
    stepSizes = takeWhile (> 1) . Prelude.iterate (`div` 2) $ sideLen - 1

    step heightMap size
        = R.computeP
        . diamondStep size
        . squareStep size
        $ heightMap

saveMap :: HeightMap -> FilePath -> IO ()
saveMap !(HeightMap arr) !path = R.runIL $ do
    image <- R.RGB <$> R.copyP rgb
    R.writeImage path image
  where
    rgbTuple = R.map R.rgb8OfGreyDouble arr
    rgb = R.traverse rgbTuple (\(R.Z :. x :. y) -> R.ix3 x y 3) toThreeDim

    toThreeDim current (R.Z :. x :. y :. 0) = let (v, _, _) = current (R.ix2 x y) in v
    toThreeDim current (R.Z :. x :. y :. 1) = let (_, v, _) = current (R.ix2 x y) in v
    toThreeDim current (R.Z :. x :. y :. 2) = let (_, _, v) = current (R.ix2 x y) in v
    toThreeDim _ _ = error "Third dimension should be either 0, 1 or 2"

diamondStep :: (R.Source s a, Num a, Fractional a, Ord a)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 a
    -- ^ Array to perform diamondStep on.
    -> R.Array R.D R.DIM2 a
diamondStep !size !arr = R.traverse arr id diamond
  where
    diamond current pos@(R.Z :. x :. y)
        | diamondPoint x y = trace ("diamond\t\t\t" ++ show x ++ "\t" ++ show y) $ diamondSquareValue
            (safeGet current (x + halfSize) y)
            (safeGet current (x - halfSize) y)
            (safeGet current x (y + halfSize))
            (safeGet current x (y - halfSize))
            (current $ R.ix2 x y)
        | otherwise = trace ("otherwise diamond\t" ++ show x ++ " " ++ show y) $ current pos

    safeGet current x y
        | R.inShape (R.extent arr) (R.ix2 x y) = current $ R.ix2 x y
        | otherwise = 0

    diamondPoint x y =
        (x `mod` halfSize == 0 && x `mod` size /= 0 && y `mod` size == 0) ||
        (y `mod` halfSize == 0 && y `mod` size /= 0 && x `mod` size == 0)

    halfSize = size `div` 2

squareStep :: (R.Source s a, Num a, Fractional a, Ord a)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 a
    -- ^ Array to perform squareStep on.
    -> R.Array R.D R.DIM2 a
squareStep !size !arr = R.traverse arr id square
  where
    square current pos@(R.Z :. x :. y)
        | squarePoint x y = trace ("square\t\t\t" ++ show x ++ "\t" ++ show y) $ diamondSquareValue
            (safeGet current (x + halfSize) (y + halfSize))
            (safeGet current (x - halfSize) (y + halfSize))
            (safeGet current (x + halfSize) (y - halfSize))
            (safeGet current (x - halfSize) (y - halfSize))
            (current $ R.ix2 x y)
        | otherwise = trace ("otherwise square\t" ++ show x ++ "\t" ++ show y) $ current pos

    safeGet current x y
        | R.inShape (R.extent arr) (R.ix2 x y) = current $ R.ix2 x y
        | otherwise = 0

    squarePoint x y =
        x `mod` halfSize == 0 &&
        x `mod` size /= 0 &&
        y `mod` halfSize == 0 &&
        y `mod` size /= 0

    halfSize = size `div` 2

{- Compute the new value of a field in the diamond square algorithm by taking
 - the 4 corners around eiher the center of a square or a diamond and a random
 - number between -1 and 1. The result is the average of the 4 corners with the
 - random number scaled and added. -}
{-# INLINE diamondSquareValue #-}
diamondSquareValue :: (Num a, Fractional a, Ord a) => a
    -- ^ First corner value.
    -> a
    -- ^ Second corner value.
    -> a
    -- ^ Third corner value.
    -> a
    -- ^ Fourth corner value.
    -> a
    -- ^ Random number between -1.0 and 1.0.
    -> a
diamondSquareValue !a !b !c !d !random
    | newValue > 1.0 = 1.0
    | newValue < 0.0 = 0.0
    | otherwise = newValue
  where
    newValue = ave + 0.1*random
    ave = (a + b + c + d) / 4

{- | Create an array of the type and shape given with random values between
 - below and above. If called as randomArray shape lower upper an array of shape
 - shape with values between lower and upper are returned. -}
randomArray :: (C.MonadRandom m, R.Shape sh, C.Random a, V.Unbox a) => sh
    -- ^ The shape of the array to return.
    -> a
    -- ^ Generate no values lower than this.
    -> a
    -- ^ Generate no values greater than this.
    -> m (R.Array R.U sh a)
randomArray shape lower upper = do
    randoms <- V.replicateM (R.size shape) (C.getRandomR (lower, upper))
    return $ R.fromUnboxed shape randoms

{- | Make sure the corner values in the array are positive. -}
cornerPositive :: (R.Source r a, R.Shape sh, Num a) => R.Array r sh a
    -- ^ Array to change corners to be positive.
    -> R.Array R.D sh a
cornerPositive !arr = R.traverse arr id cornerPositive'
  where
    cornerPositive' current pos
        | isCorner pos = abs . current $ pos
        | otherwise = current pos

    isCorner = all isSide . zip extent . R.listOfShape

    isSide (dimSize, dimIndex)
        | dimIndex == 0 || dimIndex == dimSize - 1 = True
        | otherwise = False

    extent = R.listOfShape $ R.extent arr
