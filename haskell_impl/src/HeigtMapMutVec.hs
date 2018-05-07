{-# LANGUAGE FlexibleContexts #-}
module HeigtMapMutVec where -- TODO: Fix name.

import ClassyPrelude
import qualified Control.Monad as C
import qualified Control.Monad.Primitive as C
import qualified Control.Monad.Random as C
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa.Algorithms.Pixel as R
import qualified Data.Array.Repa.IO.DevIL as R
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Prelude

-- TODO: Add internal type containing a mutable vector.
data HeightMap = HeightMap !Int !(V.Vector Double)

createMap :: (Integral a, C.MonadRandom m, C.PrimMonad m) => a -> m HeightMap
createMap n = do
    heights <- randomArray (sideLen * sideLen) (-1.0) 1.0

    applyCorners sideLen abs heights
    C.mapM_ (step sideLen heights) stepSizes

    HeightMap sideLen <$> V.freeze heights
  where
    sideLen = (2^n) + 1
    stepSizes = takeWhile (> 1) . Prelude.iterate (`div` 2) $ sideLen - 1

step :: (Fractional a, V.Unbox a, C.PrimMonad m, Ord a) => Int
    -- ^ The side length of the map.
    -> V.MVector (C.PrimState m) a
    -- ^ The map.
    -> Int
    -- ^ The step size.
    -> m ()
step sideLen heightMap size = do
    squareStep sideLen heightMap size
    diamondStep sideLen heightMap size

squareStep :: (Fractional a, V.Unbox a, C.PrimMonad m, Ord a) => Int
    -- ^ The side length of the map.
    -> V.MVector (C.PrimState m) a
    -- ^ The map.
    -> Int
    -- ^ The step size.
    -> m ()
squareStep sideLen heightMap size = mapM_ square tupleIndices
  where
    xIndices = [halfSize, size + halfSize..sideLen - 1]
    yIndices = [halfSize, size + halfSize..sideLen - 1]
    tupleIndices = (,) <$> xIndices <*> yIndices
    halfSize = size `div` 2
    square (x, y) = do
        a <- MV.read heightMap $ unsafeVectorIndex (x, y) sideLen
        b <- readDefault sideLen heightMap (x - halfSize, y - halfSize)
        c <- readDefault sideLen heightMap (x - halfSize, y + halfSize)
        d <- readDefault sideLen heightMap (x + halfSize, y - halfSize)
        e <- readDefault sideLen heightMap (x + halfSize, y + halfSize)

        writeBounded sideLen heightMap b c d e a (x, y)

diamondStep :: (Fractional a, V.Unbox a, C.PrimMonad m, Ord a) => Int
    -- ^ The side length of the map.
    -> V.MVector (C.PrimState m) a
    -- ^ The map.
    -> Int
    -- ^ The step size.
    -> m ()
diamondStep sideLen heightMap size = mapM_ diamond tupleIndices
  where
    yIndices = [0, halfSize..sideLen-1]
    tupleIndices = concatMap (\y -> map (\x -> (x, y)) [(y + halfSize) `mod` size,((y + halfSize) `mod` size) + size..sideLen - 1]) yIndices
    halfSize = size `div` 2
    diamond (x, y) = do
        a <- MV.read heightMap $ unsafeVectorIndex (x, y) sideLen
        b <- readDefault sideLen heightMap (x, y - halfSize)
        c <- readDefault sideLen heightMap (x - halfSize, y)
        d <- readDefault sideLen heightMap (x, y + halfSize)
        e <- readDefault sideLen heightMap (x + halfSize, y)

        writeBounded sideLen heightMap b c d e a (x, y)

readDefault :: (Fractional a, V.Unbox a, C.PrimMonad m) => Int
    -- ^ Side length of the map.
    -> V.MVector (C.PrimState m) a
    -- ^ The map.
    -> (Int, Int)
    -- ^ Index to get.
    -> m a
readDefault sideLen heightMap (x, y) =
    maybe (pure 0.0) (MV.read heightMap) $ vectorIndex (x, y) sideLen

writeBounded :: (Fractional a, Ord a, V.Unbox a, C.PrimMonad m) => Int
    -- ^ Sidelength of map.
    -> V.MVector (C.PrimState m) a
    -- ^ The map.
    -> a
    -- ^ Value 1.
    -> a
    -- ^ Value 2.
    -> a
    -- ^ Value 3.
    -> a
    -- ^ Value 4.
    -> a
    -- ^ Random value between -1 and 1.
    -> (Int, Int)
    -- ^ Where to write the new value.
    -> m ()
writeBounded sideLen heightMap val1 val2 val3 val4 random (x, y) =
    MV.write heightMap (unsafeVectorIndex (x, y) sideLen) (bound newVal)
  where
    -- TODO: Scale random from sideLen.
    newVal = ((val1 + val2 + val3 + val4) / 4) + 0.1 * random
    bound = max 0 . min 1

vectorIndex :: (Int, Int) -> Int -> Maybe Int
vectorIndex (x, y) sideLen
    | x < sideLen && y < sideLen && x >= 0 && y >= 0 =
        Just $ unsafeVectorIndex (x, y) sideLen
    | otherwise = Nothing

unsafeVectorIndex :: (Int, Int) -> Int -> Int
unsafeVectorIndex (x, y) sideLen = x * sideLen + y

numElemns :: HeightMap -> Int
numElemns (HeightMap _ heightMap) = V.length heightMap

{- | Create an array of the type and shape given with random values between
 - below and above. If called as randomArray shape lower upper an array of shape
 - shape with values between lower and upper are returned. -}
randomArray :: (C.PrimMonad m, C.MonadRandom m, C.Random a, V.Unbox a) => Int
    -- ^ The number of elements to return.
    -> a
    -- ^ Generate no values lower than this.
    -> a
    -- ^ Generate no values greater than this.
    -> m (MV.MVector (C.PrimState m) a)
randomArray n lower upper = MV.replicateM n $ C.getRandomR (lower, upper)

applyCorners :: (V.Unbox a, C.PrimMonad m) => Int
    -- ^ The side length of the map.
    -> (a -> a)
    -- ^ Function to apply to corners of the map.
    -> V.MVector (C.PrimState m) a
    -- ^ The map.
    -> m ()
applyCorners sideLen f v = mapM_ (MV.modify v f) cornerIndices
  where
    cornerIndices = map (Maybe.fromJust . (flip vectorIndex) sideLen) -- TODO: Use unsafe vectorIndex.
        [ (0, 0)
        , (sideLen - 1, 0)
        , (0, sideLen - 1)
        , (sideLen - 1, sideLen - 1)
        ]

showMap :: HeightMap -> String
showMap (HeightMap sideLen dat) = concatMap showLine lines ++ "\n"
  where
    lines = map (\i -> V.slice i sideLen dat) indices
    indices = [0, sideLen .. sideLen * sideLen - 1]
    showLine = (++) "\n" . show . V.toList

saveMap :: HeightMap -> FilePath -> IO ()
saveMap (HeightMap sideLen heightMap) path = R.runIL $ do
    image <- R.RGB <$> R.copyP rgb
    R.writeImage path image
  where
    array = R.fromUnboxed (R.Z :. sideLen :. sideLen) heightMap
    rgbTuple = R.map R.rgb8OfGreyDouble array
    rgb = R.traverse rgbTuple (\(R.Z :. x :. y) -> R.ix3 x y 3) toThreeDim

    toThreeDim current (R.Z :. x :. y :. 0) = let (v, _, _) = current (R.ix2 x y) in v
    toThreeDim current (R.Z :. x :. y :. 1) = let (_, v, _) = current (R.ix2 x y) in v
    toThreeDim current (R.Z :. x :. y :. 2) = let (_, _, v) = current (R.ix2 x y) in v
    toThreeDim _ _ = error "Third dimension should be either 0, 1 or 2"
