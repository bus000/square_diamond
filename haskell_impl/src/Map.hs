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

    asdf <- diamondStep heightMap [Square (Point 0 0) (sideLen - 1)]
    asdf2 <- squareStep (fst asdf) (snd asdf)

    return . Test . fst $ asdf2
  where
    sideLen = (2^(fromIntegral n)) + 1

-- TODO: Remove show
diamondStep :: (R.Source s e, Integral e, Integral a, C.MonadRandom m, C.Random e)
    => R.Array s R.DIM2 e
    -- ^ Array to perform diamond step on.
    -> [Square a]
    -- ^ The squares to perform the diamond step on.
    -> m (R.Array R.D R.DIM2 e, [Diamond a])
diamondStep arr squares = do
    squareLookup <- Map.fromList <$> C.mapM f squares

    let arr' = R.traverse arr id $ \lookup pos ->
            case Map.lookup pos squareLookup of
                Just height -> height
                Nothing -> lookup pos

    return (arr', diamonds)
  where
    {-diamonds = filter insideExtend . concatMap diamondsInSquare squares-}
    diamonds = concatMap diamondsInSquare squares

    f square = do
        randomChange <- C.getRandomR (0, 10) -- TODO: Should not be hardcoded.

        let cornerValues = map fromIntegral . map (arr R.!) . map toIndex
                . squareCorners $ square :: [Word.Word32]
            average = fromIntegral . (`div` 4) . sum $ cornerValues

        return (toIndex . squareMiddle $ square, average + randomChange)


    {-f square = (,) <$> pure (toIndex . squareMiddle $ square)-}
        {-<*> C.getRandomR (0, 10)-}

diamondsInSquare :: (Num a, Integral a) => Square a -> [Diamond a]
diamondsInSquare (Square (Point x y) sideLen) = [d1, d2, d3, d4]
  where
    halfLen = sideLen `div` 2
    d1 = Diamond (Point (x + halfLen) y) halfLen
    d2 = Diamond (Point x (y + halfLen)) halfLen
    d3 = Diamond (Point (x + halfLen) (y + sideLen)) halfLen
    d4 = Diamond (Point (x + sideLen) (y + halfLen)) halfLen

squareStep :: (R.Source s e, Integral e, Integral a, C.MonadRandom m, C.Random e)
    => R.Array s R.DIM2 e
    -> [Diamond a]
    -> m (R.Array R.D R.DIM2 e, [Square a])
squareStep arr diamonds = do
    diamondLookup <- Map.fromList <$> C.mapM f diamonds

    let arr' = R.traverse arr id $ \lookup pos ->
            case Map.lookup pos diamondLookup of
                Just height -> height
                Nothing -> lookup pos

    return (arr', squares)
  where
    squares = []

    f diamond = do
        randomChange <- C.getRandomR (0, 10) -- TODO: Should not be hardcoded and should also be negative.

        let cornerValues = map fromIntegral . map (arr R.!) . map toIndex
                . diamondSides $ diamond :: [Word.Word32]
            average = fromIntegral . (`div` 4) . sum $ cornerValues

        return (toIndex . _center $ diamond, average + randomChange)

data Point a = Point { _x :: !a, _y :: !a }
  deriving (Show, Eq, Ord)

toIndex :: Integral a => Point a -> R.DIM2
toIndex (Point x y) = R.ix2 (fromIntegral x) (fromIntegral y)

fromIndex :: Integral a => R.DIM2 -> Point a
fromIndex (R.Z :. x :. y) = Point (fromIntegral x) (fromIntegral y)

data Square a = Square { _upperLeft :: !(Point a), _squareSideLen :: !a }

squareMiddle :: (Num a, Integral a) => Square a -> Point a
squareMiddle (Square (Point x y) len) = Point (x + halfLen) (y + halfLen)
  where
    halfLen = ceiling $ fromIntegral len / 2

squareCorners :: Num a => Square a -> [Point a]
squareCorners (Square (Point x y) sideLen) =
    [ Point x y
    , Point (x + sideLen) y
    , Point x (y + sideLen)
    , Point (x + sideLen) (y + sideLen)
    ]

-- TODO: Maybe create special type cornerdiamond that only contains three points.
data Diamond a = Diamond { _center :: !(Point a), _diamondSideLen :: !a }
  deriving (Show, Eq)

diamondSides :: Num a => Diamond a -> [Point a]
diamondSides (Diamond (Point x y) len) = [s1, s2, s3, s4]
  where
    s1 = Point (x + len) y
    s2 = Point x (y + len)
    s3 = Point (x - len) y
    s4 = Point x (y - len)
