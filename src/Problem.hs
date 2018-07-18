{-# LANGUAGE LambdaCase, PartialTypeSignatures #-}
module Problem where
import Cell

import           Control.Arrow
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Semigroup
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           Data.Word
import           Ersatz
import           Prelude              hiding (all, any, not, (&&), (||))

type Grid = Vector (Vector Cell)
type MGrid = Vector (Vector MCell)

solve :: MonadIO f => Grid -> f (Result, Maybe Grid)
solve grid =
  second (fmap $ V.fromList . map V.fromList) <$> solveWith minisat (problem grid)


prettyGrid :: Grid -> String
prettyGrid = unlines . V.toList . V.map (foldMap pret)
  where
    pret Empty            = "　"
    pret Lit              = "＋"
    pret (Wall Nothing)   = "■"
    pret (Wall (Just 0))  = "０"
    pret (Wall (Just 1))  = "１"
    pret (Wall (Just 2))  = "２"
    pret (Wall (Just 3))  = "３"
    pret (Wall (Just ~4)) = "４"
    pret Light            = "○"


problem :: (HasSAT s, MonadState s m, MonadIO m) => Grid -> m [[MCell]]
problem gri = do
  let height = V.length gri
      Max width  = foldMap (Max . V.length) gri
  matrix <- V.mapM (V.mapM $ const exists) gri
  let pMat = zipMatrix gri matrix
      toIni i j (Empty, v) =
        let mcs = accessible width height pMat (i, j)
        in assert $
           v === encode Lit && any (=== encode Light) mcs ||
           v === encode Light && all (=== encode Lit) mcs
      toIni _ _ (b, v)     = assert $ v === encode b
  -- Preservation of initial placemants
  V.imapM_ (V.imapM_ . toIni) pMat

  -- Processing cells adjacent to numbered wall
  V.imapM_ (V.imapM_ . procAdj matrix height width) pMat
  return $ map V.toList $ V.toList matrix

accessible :: Width -> Height
           -> Vector (Vector (Cell, MCell)) -> (YCoord, XCoord) -> [MCell]
accessible w h mat (i, j) =
  let getIt k = foldMap (map snd . filter ((/= k) . fst) . V.toList) .
                filter (any $ (== k) . fst) . emptySegments

      r = getIt j $
          V.imap (\k (a, b) -> (a, (k, b))) $ mat V.! i
      c = getIt i $
          V.imap (\k (a, b) -> (a, (k, b))) $ transpose w h mat V.! j
  in r ++ c


type Height = Int
type Width = Int
type XCoord = Int
type YCoord = Int

procAdj :: (HasSAT s, MonadState s m)
        => MGrid -> Height -> Width -> YCoord -> XCoord -> (Cell, MCell) -> m ()
procAdj mc h w i j (Wall (Just n), _) = do
  let as = [ (i', j')
           | (dy, dx) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
           , let i' = i + dy; j' = j + dx
           , 0 <= i' && i' < h
           , 0 <= j' && j' < w
           ]
      pos = comb n as
  assert $ any (\(ps, ns) -> all ((=== encode Light) . (mc !@)) ps && all ((/== encode Light) . (mc !@)) ns) pos
procAdj _ _ _ _ _ _                  = return ()

(!@) :: Vector (Vector a) -> (Int, Int) -> a
m !@ (i, j) = m V.! i V.! j

zipMatrix :: Vector (Vector a) -> Vector (Vector b) -> Vector (Vector (a, b))
zipMatrix = V.zipWith V.zip

transpose :: Width -> Height -> Vector (Vector a) -> Vector (Vector a)
transpose w h m =
  V.generate w $ \j -> V.generate h $ \ i ->
  m V.! i V.! j

emptySegments :: Vector (Cell, a) -> [Vector a]
emptySegments vc =
  map (V.map snd) $
  filter (not . V.null) $
  splitOn ((`notElem` [Empty, Light]) . fst) vc

splitOn :: (a -> Bool) -> Vector a -> [Vector a]
splitOn p vs =
  let (ls, rs) = V.break p vs
  in if V.null rs
  then if V.null ls then [] else [ls]
  else ls : splitOn p (V.tail rs)

comb :: Word8 -> [a] -> [([a], [a])]
comb 0 [] = [([], [])]
comb _ [] = []
comb n (x : xs) =
  [(x : ys, zs) | (ys, zs) <- comb (n - 1) xs ]
  ++ [ (ys, x:zs) | (ys, zs) <- comb n xs ]

uniqueLight :: [MCell] -> Bit
uniqueLight vs =
  any (\(~[a], bs) -> a === encode Light && all (/== encode Light) bs) $ comb 1 vs
