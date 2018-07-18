{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies, FlexibleInstances, MultiWayIf #-}
{-# LANGUAGE TypeFamilies                                      #-}
module Cell where
import Control.Applicative (empty)
import Data.Typeable       (Typeable)
import Data.Word
import Ersatz
import GHC.Generics        (Generic)

newtype MCell = MCell Bit4
          deriving (Show, Typeable, Generic,
                    Equatable, Variable, Boolean)

data Cell = Empty | Light | Lit | Wall (Maybe Word8)
  deriving (Read, Show, Eq, Ord, Generic)

instance Codec MCell where
  type Decoded MCell = Cell
  decode s (MCell n) = do
    i <- decode s n
    if | i <= 4    -> pure $ Wall $ Just i
       | i == 5    -> pure Empty
       | i == 6    -> pure Light
       | i == 7    -> pure $ Wall Nothing
       | i == 8    -> pure Lit
       | otherwise -> empty
  encode Empty = MCell $ encode 5
  encode Light = MCell $ encode 6
  encode (Wall Nothing) = MCell $ encode 7
  encode Lit = MCell $ encode 8
  encode (Wall (Just n))
    | n <= 4 = MCell $ encode n
    | otherwise = error $ "Block number out of bound: " ++ show n

instance Num Cell where
  fromInteger = Wall . Just . fromInteger
  (+) = error "not implemented"
  (-) = error "not implemented"
  (*) = error "not implemented"
  abs = error "not implemented"
  signum = error "not implemented"

