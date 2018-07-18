{-# LANGUAGE LambdaCase, PatternSynonyms #-}
module Main where
import           Cell
import qualified Data.Vector as V
import           Ersatz
import           Problem

pattern W :: Cell
pattern W = Wall Nothing

pattern E :: Cell
pattern E = Empty

input :: Grid
input = V.fromList $ map V.fromList
  [[E, E, W, E, E, E, W]
  ,[E, 4, E, E, 1, E, W]
  ,[E, E, E, 2, E, E, E]
  ,[E, W, E, E, E, W, E]
  ,[E, E, E, W, E, E, E]
  ,[W, E, W, E, E, 1, E]
  ,[1, E, E, E, 1, E, E]
  ]
-- Problem taken from  the NIKOLI web site:
-- https://www.nikoli.co.jp/ja/puzzles/akari/


main :: IO ()
main = solve input >>= \case
  (Satisfied, Just grid) -> putStrLn $ prettyGrid grid
  ans -> print ans
{-
ghci> :ma
＋○■＋＋○■
○４○＋１＋■
＋○＋２○＋＋
＋■＋○＋■＋
＋＋＋■＋＋○
■＋■○＋１＋
１○＋＋１○＋
-}