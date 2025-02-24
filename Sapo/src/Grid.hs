module Grid (Cell(..), getCell, updateCell) where

-- Tipos de celula e deriving EQ serve para comparar tipos de Cell usando == /=
-- Show Permite converter valores do tipo Cell em strings (útil para imprimir no terminal)
data Cell = Empty | Water | Log | LilyPad | Player deriving (Eq, Show)
-- Meu gride vai ser uma lista com várias Cells
type Grid = [[Cell]]

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