{-# LANGUAGE LambdaCase #-}
module Map where

import ClassyPrelude
import qualified Prelude
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Index as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Word as Word
import qualified Control.Monad.Random as C
import qualified Data.Map.Strict as Map
import qualified Control.Monad as C

data Test s = Test (R.Array s R.DIM2 Word.Word8)

createMap :: C.MonadRandom m => Word.Word32 -> m (Test R.D)
createMap n = do
    nw <- C.getRandomR (0, 255)
    ne <- C.getRandomR (0, 255)
    sw <- C.getRandomR (0, 255)
    se <- C.getRandomR (0, 255)

    let heightMap = R.fromFunction (R.ix2 sideLen sideLen) $ \case
            (R.Z :. x :. y) | x == 0 && y == 0 -> nw
            (R.Z :. x :. y) | x == 0 && y == (sideLen - 1) -> sw
            (R.Z :. x :. y) | x == (sideLen - 1) && y == 0 -> ne
            (R.Z :. x :. y) | x == (sideLen - 1) && y == (sideLen - 1) -> se
            otherwise -> 0

    (arr1, figures1) <- step heightMap [Square (Point halfLen halfLen) halfLen]
    (arr2, figures2) <- step arr1 figures1
    (arr3, figures3) <- step arr2 figures2
    (arr4, figures4) <- step arr3 figures3
    (arr5, figures5) <- step arr4 figures4
    (arr6, figures6) <- step arr5 figures5

    traceShowM figures6

    return . Test $ arr6
  where
    sideLen = (2^(fromIntegral n)) + 1
    halfLen = sideLen `div` 2

-- TODO: Remove show.
step :: (R.Source s e, Integral e, Integral a, C.MonadRandom m, C.Random e, Show a)
    => R.Array s R.DIM2 e
    -- ^ Array to perform step on.
    -> [Figure a]
    -- ^ The figures to set centers of.
    -> m (R.Array R.D R.DIM2 e, [Figure a])
step arr figures = do
    newValues <- Map.fromList <$> C.mapM f figures

    let arr' = R.traverse arr id $ \lookup pos ->
            maybe (lookup pos) id $ Map.lookup pos newValues

    return (arr', filter (centerInside (fromIntegral x) (fromIntegral y)) subfigures)
  where
    subfigures = concatMap getSubFigures figures

    (R.Z :. x :. y) = R.extent arr

    f figure = do
        -- TODO: Should not be hardcoded and should also be negative.
        randomChange <- C.getRandomR (0, 10)

        let corners = map toIndex . figureCorners $ figure
            cornerValues = map fromIntegral . map (arr R.!) $ corners :: [Word.Word32]
            average = fromIntegral . (`div` 4) . sum $ cornerValues

        return (toIndex . _center $ figure, average + randomChange)

data Point a = Point { _x :: !a, _y :: !a }
  deriving (Show, Eq, Ord)

toIndex :: Integral a => Point a -> R.DIM2
toIndex (Point x y) = R.ix2 (fromIntegral x) (fromIntegral y)

fromIndex :: Integral a => R.DIM2 -> Point a
fromIndex (R.Z :. x :. y) = Point (fromIntegral x) (fromIntegral y)

data Figure a
    = Square { _center :: !(Point a), _sideLen :: !a }
    | Diamond { _center :: !(Point a), _sideLen :: !a }
  deriving (Show, Eq, Ord)

figureCorners :: (Num a, Integral a) => Figure a -> [Point a]
figureCorners (Square (Point x y) sideLen) =
    [ Point (x - sideLen) (y - sideLen)
    , Point (x - sideLen) (y + sideLen)
    , Point (x + sideLen) (y - sideLen)
    , Point (x + sideLen) (y + sideLen)
    ]
figureCorners (Diamond (Point x y) sideLen) =
    [ Point (x + sideLen) y
    , Point (x - sideLen) y
    , Point x (y + sideLen)
    , Point x (y - sideLen)
    ]

getSubFigures :: Integral a => Figure a -> [Figure a]
getSubFigures (Square (Point x y) sideLen) =
    [ Diamond (Point (x + sideLen) y) sideLen
    , Diamond (Point (x - sideLen) y) sideLen
    , Diamond (Point x (y + sideLen)) sideLen
    , Diamond (Point x (y - sideLen)) sideLen
    ]
getSubFigures (Diamond (Point x y) sideLen)
    | halfLen == 0 = []
    | otherwise =
        [ Square (Point (x - halfLen) (y - halfLen)) halfLen
        , Square (Point (x - halfLen) (y + halfLen)) halfLen
        , Square (Point (x + halfLen) (y - halfLen)) halfLen
        , Square (Point (x + halfLen) (y + halfLen)) halfLen
        ]
  where
    halfLen = sideLen `div` 2

centerInside :: (Ord a, Num a) => a -> a -> Figure a -> Bool
centerInside x y figure = px >= 0 && py >= 0 && px < x && py < y
  where
    Point px py = _center figure
