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
import qualified Control.Monad.State.Strict as C
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa.Index as R
import qualified Data.Array.Repa.IO.DevIL as R
import qualified Data.Map.Strict as Map
import qualified Data.Word as Word
import qualified Prelude

-- TODO: Change to doubles using
-- https://hackage.haskell.org/package/repa-algorithms-3.4.1.2/docs/Data-Array-Repa-Algorithms-Pixel.html.

data HeightMap = HeightMap (R.Array R.U R.DIM2 Word.Word8)

createMap :: C.MonadRandom m => Word.Word32 -> m HeightMap
createMap !n = do
    [nw, ne, sw, se] <- C.replicateM 4 (C.getRandomR (0, 255))

    let heightMap = R.fromFunction (R.ix2 sideLen sideLen) $ \case
            (R.Z :. x :. y) | x == 0 && y == 0 -> nw
            (R.Z :. x :. y) | x == 0 && y == (sideLen - 1) -> sw
            (R.Z :. x :. y) | x == (sideLen - 1) && y == 0 -> ne
            (R.Z :. x :. y) | x == (sideLen - 1) && y == (sideLen - 1) -> se
            otherwise -> 0

        stepSizes = takeWhile (> 1) . Prelude.iterate (`div` 2) $ sideLen - 1

        finalMap = foldl' (flip step) heightMap $ stepSizes

    HeightMap <$> R.computeP finalMap
  where
    sideLen = (2^(fromIntegral n)) + 1

    step size = diamondStep size . squareStep size

saveMap :: HeightMap -> FilePath -> IO ()
saveMap !(HeightMap arr) !path = R.runIL $ do
    image <- R.Grey <$> R.copyP arr
    R.writeImage path image

diamondStep :: (R.Source s e, Integral e, Integral a)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 e
    {-- ^ Array to perform diamondStep on.-}
    -> R.Array R.D R.DIM2 e
diamondStep !size !arr = arr'
  where
    arr' = R.traverse arr id $ \current pos@(R.Z :. x :. y) ->
        if (x `mod` halfSize == 0 && x `mod` size /= 0 && y `mod` size == 0) ||
                (y `mod` halfSize == 0 && y `mod` size /= 0 && x `mod` size == 0)
            then
                let v1 = fromIntegral $ current $ R.ix2 (x + halfSize) y :: Word.Word32
                    v2 = fromIntegral $ current $ R.ix2 (x - halfSize) y :: Word.Word32
                    v3 = fromIntegral $ current $ R.ix2 x (y + halfSize) :: Word.Word32
                    v4 = fromIntegral $ current $ R.ix2 x (y - halfSize) :: Word.Word32
                in fromIntegral $ (v1 + v2 + v3 + v4) `div` 4
            else current pos

    (R.Z :. xMax :. yMax) = R.extent arr

    halfSize = size `div` 2

squareStep :: (R.Source s e, Integral e, Integral a)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 e
    {-- ^ Array to perform squareStep on.-}
    -> R.Array R.D R.DIM2 e
squareStep !size !arr = arr'
  where
    arr' = R.traverse arr id $ \current pos@(R.Z :. x :. y) ->
        if x `mod` halfSize == 0 && x `mod` size /= 0 && y `mod` halfSize == 0 && y `mod` size /= 0
            then
                let v1 = fromIntegral $ current $ R.ix2 (x + halfSize) (y + halfSize) :: Word.Word32
                    v2 = fromIntegral $ current $ R.ix2 (x - halfSize) (y + halfSize) :: Word.Word32
                    v3 = fromIntegral $ current $ R.ix2 (x + halfSize) (y - halfSize) :: Word.Word32
                    v4 = fromIntegral $ current $ R.ix2 (x - halfSize) (y - halfSize) :: Word.Word32
                in fromIntegral $ (v1 + v2 + v3 + v4) `div` 4
            else current pos

    (R.Z :. xMax :. yMax) = R.extent arr

    halfSize = size `div` 2
