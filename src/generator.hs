module Main where
import Cell
import Problem

import           Control.Monad
import           Control.Monad.Loops
import           Data.Maybe
import           Data.Random
import qualified Data.Vector         as V
import           Ersatz

main :: IO ()
main = putStrLn . prettyGrid =<< generateGrid 0.5 10 10

generateGrid :: Rational -> Height -> Width -> IO Grid
generateGrid rat h w =
  fromJust . snd <$> iterateUntil ((== Satisfied) . fst) (solve =<< sample (randomGrid rat h w))

randomGrid :: Rational -> Height -> Width -> RVar Grid
randomGrid r h w = do
  let count = floor $ fromIntegral (h * w) * r
  V.replicateM h $ V.replicateM w $
    join $ randomElement $ replicate count (return Empty)
                        ++ replicate (h*w - count) wall

wall :: RVar Cell
wall = Wall <$> randomElement [Nothing, Just 0, Just 1, Just 2, Just 3]

cell :: RVar Cell
cell = join $ randomElement [wall, return Empty, return Empty]
