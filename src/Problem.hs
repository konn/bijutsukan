{-# LANGUAGE FlexibleContexts, LambdaCase, PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards, TupleSections                      #-}
module Problem where
import Cell

import           Control.Arrow
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List            (inits)
import           Data.Maybe
import           Data.Semigroup
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           Ersatz
import           Ersatz.Counting
import           Prelude              hiding (all, and, any, not, or, (&&),
                                       (||))

type Matrix a = Vector (Vector a)
type Grid     = Matrix Cell
type MGrid    = Matrix MCell

solve :: (MonadPlus f, MonadIO m) => Grid -> m (Result, f Grid)
solve grid =
  second (fmap $ V.fromList . map V.fromList) <$> solveWith minisat (problem grid)

solve' :: (MonadPlus f, MonadIO m) => Bool -> Grid -> m (Result, f Grid)
solve' b grid =
  second (fmap $ V.fromList . map V.fromList) <$> solveWith minisat (problem' b grid)

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


data Env = Env { matrix :: Matrix (Cell, MCell)
               , width  :: Width
               , height :: Height
               }
         deriving (Show)

problem :: (HasSAT s, MonadState s m, MonadIO m)
        => Grid -> m [[MCell]]
problem = problem' False

problem' :: (HasSAT s, MonadState s m, MonadIO m)
         => Bool                -- ^ Allow adding new walls?
         -> Grid -> m [[MCell]]
problem' newWalls gri = do
  let height = V.length gri
      Max width  = foldMap (Max . V.length) gri
      poses = [(y, x) | y <- [0..height-1], x <- [0..width-1]]
  vMat <- V.mapM (V.mapM $ const exists) gri
  let matrix = zipMatrix gri vMat
  invs <- runReaderT (mapM (invariantAt newWalls) poses) Env{..}
  assert $ and invs
  return $ map V.toList $ V.toList vMat

isNumWall :: MCell -> Bit
isNumWall c = any ((c===) . encode . Wall) [Just n | n <- [0..4]]

isWall :: MCell -> Bit
isWall c = any ((c===) . encode . Wall) $ Nothing : [Just n | n <- [0..4]]

invariantAt :: MonadReader Env m => Bool -> (Row, Col) -> m Bit
invariantAt generationMode pos = do
  (cell, var) <- at pos
  adjCond <- procAdj pos
  brightness <-
    if generationMode
    then (not (isWall var) ==>) <$> shouldBeBright pos var
    else case cell of
      Empty -> do
        mcs <- accessible pos
        return $
           var === encode Lit   && any (=== encode Light) mcs ||
           var === encode Light && all (=== encode Lit) mcs
      _     -> return true
  return $ and [ isNumWall var ==> adjCond
               , initialPlacement generationMode cell var
               , brightness
               ]

accessible :: MonadReader Env m => (Row, Col) -> m [MCell]
accessible (i, j) = do
  Env mat w h <- ask
  let getIt k = foldMap (map snd . filter ((/= k) . fst) . V.toList) .
                filter (any $ (== k) . fst) . emptySegments

      r = getIt j $
          V.imap (\k (a, b) -> (a, (k, b))) $ mat V.! i
      c = getIt i $
          V.imap (\k (a, b) -> (a, (k, b))) $ transpose w h mat V.! j
  return $ r ++ c

initialPlacement :: Bool        -- Allow new walls?
                 -> Cell
                 -> MCell -> Bit
initialPlacement False Empty v = v === encode Lit || v === encode Light
initialPlacement True  Empty _ = true
initialPlacement _     b     v = v === encode b

shouldBeBright :: MonadReader Env m => (Row, Col) -> MCell -> m Bit
shouldBeBright (r, c) v = do
  Env{..} <- ask
  bss <- mapM (cellsRelativeTo (r, c)) [U,D,L,R]
  let anyWall = foldl (\a u -> isWall u || a) false
      anyAccLight cs = or $ zipWith (\t wall -> not (anyWall wall) && t === encode Light) cs (inits cs)
      allAccLit   cs = and $ zipWith (\t wall -> anyWall wall || t === encode Lit) cs (tail $ inits cs)
  return $ v === encode Light && all allAccLit bss
        || v === encode Lit   && any anyAccLight bss

cellsRelativeTo :: MonadReader Env m => (Int, Int) -> Direction -> m [MCell]
cellsRelativeTo (row, col) dir = do
  Env{..} <- ask
  case dir of
    U -> mapM (fmap snd . at . (,col)) [row-1, row-2 .. 0]
    D -> mapM (fmap snd . at . (,col)) [row+1..height-1]
    L -> mapM (fmap snd . at . (row,)) [col-1,col-2 .. 0]
    R -> mapM (fmap snd . at . (row,)) [col+1..width-1]

data Direction = U | D | L | R
               deriving (Read, Show, Eq, Ord)

type Height = Int
type Width = Int
type Col = Int
type Row = Int

procAdj :: MonadReader Env m => (Int, Int) -> m Bit
procAdj (i, j) = do
  (_, var) <- at (i, j)
  as <- catMaybes <$> mapM atMaybe
           [ (i', j')
           | (dy, dx) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
           , let i' = i + dy; j' = j + dx
           ]
  let choice n = exactly n $ map ((=== encode Light) . snd) as
  return $ or [ var === encode (fromIntegral n :: Cell) && choice n
              | n <- [0..4]
              ]

(!@) :: Matrix a -> (Int, Int) -> a
m !@ (i, j) = m V.! i V.! j

zipMatrix :: Matrix a -> Matrix b -> Matrix (a, b)
zipMatrix = V.zipWith V.zip

at :: MonadReader Env m => (Int, Int) -> m (Cell, MCell)
at pos = asks ((!@ pos) . matrix)

atMaybe :: MonadReader Env m => (Height, Width) -> m (Maybe (Cell, MCell))
atMaybe (y, x) = asks $ \Env{..} ->
  if 0 <= y && y < height && 0 <= x && x < width
  then Just (matrix !@ (y, x))
  else Nothing

clearEmpty :: Matrix Cell -> Matrix Cell
clearEmpty = V.map (V.map clear)
  where
    clear b@Wall{} = b
    clear _        = Empty

findAnotherSolution :: (HasSAT s, MonadState s m, MonadIO m)
                    => Grid -> m [[MCell]]
findAnotherSolution gr = do
  vs <- problem $ clearEmpty gr
  let tab = V.fromList $ map V.fromList vs
      step i j Light = [tab !@ (i, j) /== encode Light]
      step _ _ _     = []
  let noLights = foldMap (uncurry $ \i a -> foldMap (uncurry $ step i) $ V.indexed a) $ V.indexed gr
  assert $ or noLights
  return vs

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

uniqueLight :: [MCell] -> Bit
uniqueLight = exactly 1 . map (=== encode Light)

{-# ANN module "HLint: ignore Use ||" #-}
{-# ANN module "HLint: ignore Use &&" #-}
