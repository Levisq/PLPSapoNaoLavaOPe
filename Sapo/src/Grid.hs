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
  } deriving (Eq, Show)

-- Acessa uma célula de forma segura
getCell :: Grid -> (Int, Int) -> Maybe Cell
getCell grid (x, y) =
  if y >= 0 && y < length grid && x >= 0 && x < length (grid !! y)
    then Just (grid !! y !! x)
    else Nothing

-- Atualiza uma célula de forma segura
updateCell :: Grid -> (Int, Int) -> Cell -> Maybe Grid
updateCell grid (x, y) newCell =
  if y >= 0 && y < length grid && x >= 0 && x < length (grid !! y)
    then Just (take y grid ++
               [take x (grid !! y) ++ [newCell] ++ drop (x + 1) (grid !! y)] ++
               drop (y + 1) grid)
    else Nothing