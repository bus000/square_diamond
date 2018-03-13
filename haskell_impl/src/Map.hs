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

data HeightMap s = HeightMap (R.Array s R.DIM2 Word.Word8)

createMap :: C.MonadRandom m => Word.Word32 -> m (HeightMap R.D)
createMap !n = do
    [nw, ne, sw, se] <- C.replicateM 4 (C.getRandomR (0, 255))

    let heightMap = R.fromFunction (R.ix2 sideLen sideLen) $ \case
            (R.Z :. x :. y) | x == 0 && y == 0 -> nw
            (R.Z :. x :. y) | x == 0 && y == (sideLen - 1) -> sw
            (R.Z :. x :. y) | x == (sideLen - 1) && y == 0 -> ne
            (R.Z :. x :. y) | x == (sideLen - 1) && y == (sideLen - 1) -> se
            otherwise -> 0

    {-HeightMap <$> C.evalStateT (iterateWhile (not . null <$> C.get) step heightMap) figures-}
    heightMap' <- squareStep (sideLen - 1) heightMap
    heightMap'' <- diamondStep (sideLen - 1) heightMap'

    return . HeightMap $ heightMap''
  where
    sideLen = (2^(fromIntegral n)) + 1
    halfLen = sideLen `div` 2

saveMap :: R.Source s Word.Word8 => HeightMap s -> FilePath -> IO ()
saveMap !(HeightMap arr) !path = R.runIL $ do
    image <- R.Grey <$> R.copyP arr
    R.writeImage path image

diamondStep :: (R.Source s e, Integral e, Integral a, C.MonadRandom m)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 e
    {-- ^ Array to perform diamondStep on.-}
    -> m (R.Array R.D R.DIM2 e)
diamondStep !size !arr = do
    return arr'
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

squareStep :: (R.Source s e, Integral e, Integral a, C.MonadRandom m)
    => Int
    -- ^ Size.
    -> R.Array s R.DIM2 e
    {-- ^ Array to perform squareStep on.-}
    -> m (R.Array R.D R.DIM2 e)
squareStep !size !arr = do
    traceShowM $ "size " ++ show size
    return arr'
  where
    arr' = R.traverse arr id $ \current pos@(R.Z :. x :. y) ->
        if x `mod` halfSize == 0 && x `mod` size /= 0 && y `mod` halfSize == 0 && y `mod` size /= 0
            then traceShow ("In then " ++ show x ++ ", " ++ show y) (
                let v1 = fromIntegral $ current $ R.ix2 (x + halfSize) (y + halfSize) :: Word.Word32
                    v2 = fromIntegral $ current $ R.ix2 (x - halfSize) (y + halfSize) :: Word.Word32
                    v3 = fromIntegral $ current $ R.ix2 (x + halfSize) (y - halfSize) :: Word.Word32
                    v4 = fromIntegral $ current $ R.ix2 (x - halfSize) (y - halfSize) :: Word.Word32
                in fromIntegral $ (v1 + v2 + v3 + v4) `div` 4)
            else traceShow "In else" (current pos)

    (R.Z :. xMax :. yMax) = R.extent arr

    halfSize = size `div` 2









{-step :: (R.Source s e, Integral e, Integral a, C.MonadRandom m,-}
    {-C.MonadState [Figure a] m, C.Random e)-}
    {-=> R.Array s R.DIM2 e-}
    {-{-- ^ Array to perform step on.-}-}
    {--> m (R.Array R.D R.DIM2 e)-}
{-step !arr = do-}
    {-figures <- C.get-}
    {-C.put $ filter (centerInside (fromIntegral x) (fromIntegral y)) (getAllSubfigures figures)-}

    {-traceShowM $ "running 1 " ++ (show . length . getAllSubfigures $ figures)-}

    {-newValues <- Map.fromList <$> C.mapM f figures-}

    {-let arr' = R.traverse arr id $ \lookup pos ->-}
            {-maybe (lookup pos) id $ Map.lookup pos newValues-}

    {-traceShowM "running 2"-}

    {-return arr'-}
  {-where-}
    {-(R.Z :. x :. y) = R.extent arr-}

    {-f figure = do-}
        {--- TODO: Should not be hardcoded and should also be negative.-}
        {-randomChange <- C.getRandomR (0, 10)-}

        {-let corners = map toIndex . figureCorners $ figure-}
            {-cornerValues = map fromIntegral . map (arr R.!) $ corners :: [Word.Word32]-}
            {-average = fromIntegral . (`div` 4) . sum $ cornerValues-}

        {-return (toIndex . _center $ figure, average + randomChange)-}

{-data Point a = Point { _x :: !a, _y :: !a }-}
  {-deriving (Show, Eq, Ord)-}

{-toIndex :: Integral a => Point a -> R.DIM2-}
{-toIndex !(Point x y) = R.ix2 (fromIntegral x) (fromIntegral y)-}

{-fromIndex :: Integral a => R.DIM2 -> Point a-}
{-fromIndex !(R.Z :. x :. y) = Point (fromIntegral x) (fromIntegral y)-}

{-data Figure a-}
    {-= Square { _center :: !(Point a), _sideLen :: !a }-}
    {-| Diamond { _center :: !(Point a), _sideLen :: !a }-}
  {-deriving (Show, Eq, Ord)-}

{-figureCorners :: (Num a, Integral a) => Figure a -> [Point a]-}
{-figureCorners !(Square (Point x y) sideLen) =-}
    {-[ Point (x - sideLen) (y - sideLen)-}
    {-, Point (x - sideLen) (y + sideLen)-}
    {-, Point (x + sideLen) (y - sideLen)-}
    {-, Point (x + sideLen) (y + sideLen)-}
    {-]-}
{-figureCorners !(Diamond (Point x y) sideLen) =-}
    {-[ Point (x + sideLen) y-}
    {-, Point (x - sideLen) y-}
    {-, Point x (y + sideLen)-}
    {-, Point x (y - sideLen)-}
    {-]-}

{-getAllSubfigures :: Integral a => [Figure a] -> [Figure a]-}
{-getAllSubfigures = foldr getSubFigures []-}
  {-where-}
    {-getSubFigures !(Square (Point x y) sideLen) xs =-}
        {-Diamond (Point (x + sideLen) y) sideLen:-}
        {-Diamond (Point (x - sideLen) y) sideLen:-}
        {-Diamond (Point x (y + sideLen)) sideLen:-}
        {-Diamond (Point x (y - sideLen)) sideLen:-}
        {-xs-}
    {-getSubFigures !(Diamond (Point x y) sideLen) xs-}
        {-| halfLen == 0 = xs-}
        {-| otherwise =-}
            {-Square (Point (x - halfLen) (y - halfLen)) halfLen:-}
            {-Square (Point (x - halfLen) (y + halfLen)) halfLen:-}
            {-Square (Point (x + halfLen) (y - halfLen)) halfLen:-}
            {-Square (Point (x + halfLen) (y + halfLen)) halfLen:-}
            {-xs-}
      {-where-}
        {-halfLen = sideLen `div` 2-}

{-getSubFigures :: Integral a => Figure a -> [Figure a]-}
{-getSubFigures !(Square (Point x y) sideLen) =-}
    {-[ Diamond (Point (x + sideLen) y) sideLen-}
    {-, Diamond (Point (x - sideLen) y) sideLen-}
    {-, Diamond (Point x (y + sideLen)) sideLen-}
    {-, Diamond (Point x (y - sideLen)) sideLen-}
    {-]-}
{-getSubFigures !(Diamond (Point x y) sideLen)-}
    {-| halfLen == 0 = []-}
    {-| otherwise =-}
        {-[ Square (Point (x - halfLen) (y - halfLen)) halfLen-}
        {-, Square (Point (x - halfLen) (y + halfLen)) halfLen-}
        {-, Square (Point (x + halfLen) (y - halfLen)) halfLen-}
        {-, Square (Point (x + halfLen) (y + halfLen)) halfLen-}
        {-]-}
  {-where-}
    {-halfLen = sideLen `div` 2-}

{-centerInside :: (Ord a, Num a) => a -> a -> Figure a -> Bool-}
{-centerInside x y figure = px >= 0 && py >= 0 && px < x && py < y-}
  {-where-}
    {-Point px py = _center figure-}

iterateWhile :: Monad m => (m Bool) -> (a -> m a) -> a -> m a
iterateWhile !p !f !base = do
    x <- p
    if x
        then f base >>= iterateWhile p f
        else return base
