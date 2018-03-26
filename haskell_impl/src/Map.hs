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
import qualified Data.Array.Repa.IO.DevIL as R
import qualified Prelude
import qualified Data.Vector.Unboxed as V

data HeightMap = HeightMap (R.Array R.U R.DIM2 Double)

createMap :: C.MonadRandom m => Int -> m HeightMap
createMap !n = do
    [nw, ne, sw, se] <- C.replicateM 4 (C.getRandomR (0.0, 1.0))

    let heightMap = R.fromFunction (R.ix2 sideLen sideLen) $ \case
            (R.Z :. x :. y) | x == 0 && y == 0 -> nw
            (R.Z :. x :. y) | x == 0 && y == (sideLen - 1) -> sw
            (R.Z :. x :. y) | x == (sideLen - 1) && y == 0 -> ne
            (R.Z :. x :. y) | x == (sideLen - 1) && y == (sideLen - 1) -> se
            _ | otherwise -> 0
        stepSizes = takeWhile (> 1) . Prelude.iterate (`div` 2) $ sideLen - 1

    finalMap <- C.foldM (flip step) heightMap stepSizes

    HeightMap <$> R.computeP finalMap
  where
    sideLen = (2^n) + 1
    step size heightMap = squareStep size heightMap >>= diamondStep size

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

diamondStep :: (R.Source s Double, C.MonadRandom m)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 Double
    -- ^ Array to perform diamondStep on.
    -> m (R.Array R.D R.DIM2 Double)
diamondStep !size !arr = do
    randoms <- randomArray (R.extent arr) 0.0 0.1 -- TODO: Don't hardcode.
    return $ R.traverse2 arr randoms const diamond
  where
    diamond current random pos@(R.Z :. x :. y)
        | (x `mod` halfSize == 0 && x `mod` size /= 0 && y `mod` size == 0) ||
            (y `mod` halfSize == 0 && y `mod` size /= 0 && x `mod` size == 0) =
                let v1 = safeGet current (x + halfSize) y
                    v2 = safeGet current (x - halfSize) y
                    v3 = safeGet current x (y + halfSize)
                    v4 = safeGet current x (y - halfSize)
                in probability $ ((v1 + v2 + v3 + v4) / 4) + random pos
        | otherwise = current pos

    safeGet current x y
        | x < 0 || y < 0 || x >= xMax || y >= yMax = 0
        | otherwise = current $ R.ix2 x y

    (R.Z :. xMax :. yMax) = R.extent arr

    halfSize = size `div` 2

squareStep :: (R.Source s Double, C.MonadRandom m)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 Double
    -- ^ Array to perform squareStep on.
    -> m (R.Array R.D R.DIM2 Double)
squareStep !size !arr = do
    randoms <- randomArray (R.extent arr) (-0.1) 0.1 -- TODO: Don't hardcode.
    return $ R.traverse2 arr randoms const square
  where
    square current random pos@(R.Z :. x :. y)
        | x `mod` halfSize == 0 && x `mod` size /= 0 && y `mod` halfSize == 0 && y `mod` size /= 0 =
            let v1 = safeGet current (x + halfSize) (y + halfSize)
                v2 = safeGet current (x - halfSize) (y + halfSize)
                v3 = safeGet current (x + halfSize) (y - halfSize)
                v4 = safeGet current (x - halfSize) (y - halfSize)
            in probability $ ((v1 + v2 + v3 + v4) / 4) + random pos
        | otherwise = current pos

    safeGet current x y
        | x < 0 || y < 0 || x >= xMax || y >= yMax = 0
        | otherwise = current $ R.ix2 x y

    (R.Z :. xMax :. yMax) = R.extent arr

    halfSize = size `div` 2

randomArray :: (C.MonadRandom m, R.Shape sh, C.Random a, V.Unbox a) => sh -> a -> a -> m (R.Array R.U sh a)
randomArray shape lower upper = do
    randoms <- V.replicateM (R.size shape) (C.getRandomR (lower, upper))
    return $ R.fromUnboxed shape randoms

probability :: Double -> Double
probability x
    | x > 1.0 = 1.0
    | x < 0.0 = 0.0
    | otherwise = x
