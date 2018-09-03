{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module HeightMap
    ( HeightMap
    , createMap
    , createMapWithConfiguration
    , saveMap
    , showMap

    , Configuration
    , createConfiguration
    ) where

import ClassyPrelude
import qualified Control.Monad as C
import qualified Control.Monad.Primitive as C
import qualified Control.Monad.Random as C
import qualified Control.Monad.Reader as C
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa.Algorithms.Pixel as R
import qualified Data.Array.Repa.IO.DevIL as R
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Prelude

{- | Represents a heightmap. The heightmap is a vector of doubles between 0 and
 - 1. 0 is the lowest height and 1 is the highest. The heightmap is two
 - dimensional, indices in the two dimensional space (x, y) can be translated
 - into vector indices via vectorIndex and unsafeVectorIndex. -}
data HeightMap = HeightMap !Int !(V.Vector Double)

{- | Create a heightmap using the default configuration. -}
createMap
    :: (Integral a, C.MonadRandom m, C.PrimMonad m)
    => a
    -- ^ The side length of the map generated is 2^a+1.
    -> m HeightMap
createMap n = createMapWithConfiguration n defaultConfiguration

{- | Create a heightmap using the configuration given. -}
createMapWithConfiguration
    :: (Integral a, C.MonadRandom m, C.PrimMonad m)
    => a
    -- ^ The side length of the map generated is 2^a+1.
    -> Configuration Double
    -- ^ The configuration to use for the generation.
    -> m HeightMap
createMapWithConfiguration n = C.runReaderT (createMapInternal n)

{- | Configuration used to generate height map. Currently only consist of the
 - roughness of the map. If roughness is high then changes in height occur
 - rapidly and if roughness is low changes in height is slower. -}
data Configuration a = Configuration
    { _roughness :: a -- ^ How rough the map should be.
    }

{- | A default configuration that can be used to generate heightmaps. -}
defaultConfiguration :: Fractional a => Configuration a
defaultConfiguration = Configuration 1.0

{- | Create a new configuration or return an error if arguments is not valid. -}
createConfiguration
    :: (Fractional a, Ord a)
    => a
    -- ^ The roughness of the map between 0 and 1 (inclusive).
    -> Either T.Text (Configuration a)
createConfiguration roughness
    | roughness >= 0.0 && roughness <= 1.0 = Right $ Configuration roughness
    | otherwise = Left "Roughness should be between 0 and 1."

{- | Internal datatype representing a heightmap. Only difference between the
 - internal and external maps is that the internal map is a mutable vector. -}
data IntHeightMap a m = IntHeightMap !Int !(MV.MVector (C.PrimState m) a)

{- | Create a heightmap. -}
createMapInternal
    :: (Integral a, C.MonadRandom m, C.PrimMonad m
       , C.MonadReader (Configuration Double) m)
    => a
    -- ^ The side length of the map generated is 2^a+1.
    -> m HeightMap
createMapInternal n = do
    heights <- randomArray (sideLen * sideLen) (-1.0) 1.0

    let heightMap = IntHeightMap sideLen heights

    applyCorners heightMap abs
    C.mapM_ (step heightMap) stepSizes

    HeightMap sideLen <$> V.unsafeFreeze heights
  where
    sideLen = (2^n) + 1
    stepSizes = takeWhile (> 1) . Prelude.iterate (`div` 2) $ sideLen - 1

{- | Run both a square and a diamondstep for the current step size. -}
step
    :: (Fractional a, V.Unbox a, Ord a, C.PrimMonad m
       , C.MonadReader (Configuration a) m)
    => IntHeightMap a m
    -- ^ The map.
    -> Int
    -- ^ The step size.
    -> m ()
step heightMap size = squareStep heightMap size >> diamondStep heightMap size

{- | Run a square step on a map. -}
squareStep
    :: (Fractional a, V.Unbox a, C.PrimMonad m, Ord a
       , C.MonadReader (Configuration a) m)
    => IntHeightMap a m
    -- ^ The map.
    -> Int
    -- ^ The step size.
    -> m ()
squareStep hmap size = mapM_ square tupleIndices
  where
    sideLen = getSideLen hmap
    xIndices = [halfSize, size + halfSize..sideLen - 1]
    yIndices = [halfSize, size + halfSize..sideLen - 1]
    tupleIndices = (,) <$> xIndices <*> yIndices
    halfSize = size `div` 2
    square (x, y) = do
        a <- unsafeRead hmap (x, y)
        b <- readDefault hmap (x - halfSize, y - halfSize) 0.5
        c <- readDefault hmap (x - halfSize, y + halfSize) 0.5
        d <- readDefault hmap (x + halfSize, y - halfSize) 0.5
        e <- readDefault hmap (x + halfSize, y + halfSize) 0.5

        writeBounded hmap b c d e a (x, y) size

{- | Run a diamond step on the map. -}
diamondStep
    :: (Fractional a, V.Unbox a, C.PrimMonad m, Ord a
       , C.MonadReader (Configuration a) m)
    => IntHeightMap a m
    -- ^ The map.
    -> Int
    -- ^ The step size.
    -> m ()
diamondStep hmap size = mapM_ diamond tupleIndices
  where
    sideLen = getSideLen hmap
    yIndices = [0, halfSize..sideLen-1]
    tupleIndices = concatMap (\y -> zip (xIndices y) (repeat y)) yIndices
    xIndices = takeWhile (<= sideLen - 1) . Prelude.iterate (+ size) .
        (`mod` size) . (+) halfSize
    halfSize = size `div` 2
    diamond (x, y) = do
        a <- unsafeRead hmap (x, y)
        b <- readDefault hmap (x, y - halfSize) 0.5
        c <- readDefault hmap (x - halfSize, y) 0.5
        d <- readDefault hmap (x, y + halfSize) 0.5
        e <- readDefault hmap (x + halfSize, y) 0.5

        writeBounded hmap b c d e a (x, y) size

{- | Read a value from a heightmap located at a particular index. If the index
 - is outside the map then the default value given in returned. -}
readDefault
    :: (V.Unbox a, C.PrimMonad m)
    => IntHeightMap a m
    -- ^ The map.
    -> (Int, Int)
    -- ^ Index to get.
    -> a
    -- ^ Default value to return if index is outside the map.
    -> m a
readDefault (IntHeightMap sideLen heightMap) (x, y) d =
    maybe (pure d) (MV.read heightMap) $ vectorIndex (x, y) sideLen

{- | Read a value from a heightmap located at a particular index. If the index
 - is outside the map an exception is thrown as there is no bounds checking. -}
unsafeRead
    :: (V.Unbox a, C.PrimMonad m)
    => IntHeightMap a m
    -- ^ The map.
    -> (Int, Int)
    -- ^ Index to get.
    -> m a
unsafeRead (IntHeightMap sideLen heightMap) (x, y) =
    MV.read heightMap $ unsafeVectorIndex (x, y) sideLen

{- | Undate the value of a particular index. The new value will be the average
 - of the 4 surrounding values plus a random value between 0 and 1 that is
 - scaled by the current stepsize and roughness. -}
writeBounded
    :: (Fractional a, Ord a, V.Unbox a, C.PrimMonad m
       , C.MonadReader (Configuration a) m)
    => IntHeightMap a m
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
    -> Int
    -- ^ Current step size.
    -> m ()
writeBounded (IntHeightMap sideLen heightMap) v1 v2 v3 v4 rand pos stepSize = do
    roughness <- C.asks _roughness
    let newVal = ((v1 + v2 + v3 + v4) / 4) + scale * rand * roughness
    MV.write heightMap (unsafeVectorIndex pos sideLen) (bound newVal)
  where
    bound = max 0 . min 1
    scale = fromIntegral stepSize / fromIntegral (sideLen - 1)

{- | Get the index into a one dimensional vector that corresponds to an index in
 - a two dimensional heightmap. If the index is outside the map then Nothing is
 - returned. -}
vectorIndex
    :: (Int, Int)
    -- ^ Position in the heightmap.
    -> Int
    -- ^ Side length of the heightmap.
    -> Maybe Int
vectorIndex (x, y) sideLen
    | x < sideLen && y < sideLen && x >= 0 && y >= 0 =
        Just $ unsafeVectorIndex (x, y) sideLen
    | otherwise = Nothing

{- | Get the index into a one dimensional vector that corresponds to an index in
 - a two dimensional heightmap. Bounds checking is not performed so indices
 - outside the map is still translated silently. -}
unsafeVectorIndex
    :: (Int, Int)
    -- ^ Position in the heightmap.
    -> Int
    -- ^ Side length of the heightmap.
    -> Int
unsafeVectorIndex (x, y) sideLen = x * sideLen + y

{- | Create an array of the type and shape given with random values between
 - below and above. If called as randomArray shape lower upper an array of shape
 - shape with values between lower and upper are returned. -}
randomArray
    :: (C.PrimMonad m, C.MonadRandom m, C.Random a, V.Unbox a)
    => Int
    -- ^ The number of elements to return.
    -> a
    -- ^ Generate no values lower than this.
    -> a
    -- ^ Generate no values greater than this.
    -> m (MV.MVector (C.PrimState m) a)
randomArray n lower upper = MV.replicateM n $ C.getRandomR (lower, upper)

{- | Apply a function to the 4 corners of a heightmap. -}
applyCorners
    :: (V.Unbox a, C.PrimMonad m) => IntHeightMap a m
    -- ^ The map.
    -> (a -> a)
    -- ^ Function to apply to corners of the map.
    -> m ()
applyCorners (IntHeightMap sideLen v) f = mapM_ (MV.modify v f) cornerIndices
  where
    cornerIndices = map (`unsafeVectorIndex` sideLen)
        [ (0, 0)
        , (sideLen - 1, 0)
        , (0, sideLen - 1)
        , (sideLen - 1, sideLen - 1)
        ]

{- | Show a heightmap as text. -}
showMap
    :: HeightMap
    -- ^ The heightmap to show.
    -> T.Text
showMap (HeightMap sideLen dat) = concatMap showLine mylines ++ "\n"
  where
    mylines = map (\i -> V.slice i sideLen dat) indices
    indices = [0, sideLen .. sideLen * sideLen - 1]
    showLine = (++) "\n" . T.pack . show . V.toList

-- TODO: Stop using repa and use friday instead.
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

{- | Get the side length of a heightmap. -}
getSideLen
    :: IntHeightMap a m
    -- ^ The heightmap to get side length of.
    -> Int
getSideLen (IntHeightMap sideLen _) = sideLen
