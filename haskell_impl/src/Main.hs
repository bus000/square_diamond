{-# LANGUAGE FlexibleInstances #-}
module Main where

import ClassyPrelude
import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa.Algorithms.Matrix as R
import qualified Data.Word as Word
import qualified Map as Map
import qualified Prelude as Prelude
import qualified System.Environment as Sys
import qualified Text.PrettyPrint as PP
import qualified Text.PrettyPrint.HughesPJClass as PP

instance PP.Pretty Word.Word8 where
    pPrint = PP.int . fromIntegral

instance (R.Source t a, PP.Pretty a) => PP.Pretty (R.Array t R.DIM1 a) where
    pPrint a = PP.brackets $ PP.hcat $ PP.punctuate (PP.comma PP.<> PP.space) elems
      where
        elems = [ PP.pPrint (a R.! j) | i <- [0..n-1], let j = R.Z :. i ]
        R.Z :. n = R.extent a

instance (R.Source t a, PP.Pretty a) => PP.Pretty (R.Array t R.DIM2 a) where
    pPrint a = PP.vcat elems
      where
        elems = [ PP.pPrint (R.slice a j) | i <- [0..n-1], let j = R.Any :. i :. R.All]
        R.Z :. n :. _m = R.extent a

main :: IO ()
main = do
    {-Map.HeightMap heightmap <- Map.createMap 3-}
    {-putStr . pack . PP.render . PP.pPrint $ heightmap-}

    [size] <- Sys.getArgs

    heightmap <- Map.createMap (Prelude.read size)
    Map.saveMap heightmap "output.png"
