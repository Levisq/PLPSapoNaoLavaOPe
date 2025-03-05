module Objetos (Tronco(..), Regia(..), Sapo(..)) where

-- Tipo para representar uma posição no grid
type Position = (Int, Int)

-- Tipo para representar um Tronco
data Tronco = Tronco
  { troncoPosition :: Position
  , troncoSpeed :: Int  -- Velocidade do tronco
  } deriving (Eq, Show)

-- Tipo para representar uma Regia (vitória-régia)
data Regia = Regia
  { regiaPosition :: Position
  , regiaSpeed :: Int  -- Velocidade da vitória-régia
  } deriving (Eq, Show)

-- Tipo para representar o Sapo
data Sapo = Sapo
  { sapoPosition :: Position
  , sapoLives :: Int  -- Número de vidas do sapo
  } deriving (Eq, Show)