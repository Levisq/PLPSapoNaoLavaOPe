module Grid (Cell(..), Grid, GameState(..), getCell, updateCell) where

import Objetos (Tronco, Regia, Sapo)

-- Tipos de celula e deriving EQ serve para comparar tipos de Cell usando == /=
-- Show Permite converter valores do tipo Cell em strings (Acho que vai ser útil para imprimir no terminal)
data Cell = Empty | Water | TroncoCell Tronco | RegiaCell Regia | SapoCell Sapo
  deriving (Eq, Show)

-- O grid é uma lista de listas de células
type Grid = [[Cell]]

data GameState = GameState
  { grid :: Grid
    ,timeSinceLastMove :: Float
  } deriving (Eq, Show)

-- Acessa uma célula de forma segura
getCell :: Grid -> (Int, Int) -> Maybe Cell
getCell g (x, y) =
  if y >= 0 && y < length g && x >= 0 && x < length (g !! y)
    then Just (g !! y !! x)
    else Nothing

-- att uma celula
updateCell :: Grid -> (Int, Int) -> Cell -> Maybe Grid
updateCell g (x, y) newCell =
  if y >= 0 && y < length g && x >= 0 && x < length (g !! y)
    then Just (take y g ++
               [take x (g !! y) ++ [newCell] ++ drop (x + 1) (g !! y)] ++
               drop (y + 1) g)
    else Nothing