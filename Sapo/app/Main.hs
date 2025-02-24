module Main (main) where

import Grid (Cell(..), getCell, updateCell)

main :: IO ()
main = do 
    let grid = [[Empty, Water], [Log, Player]]
  
    -- Acessa uma célula
    print (getCell grid (1, 1))  -- Just Player
  
    -- Atualiza uma célula
    let updatedGrid = updateCell grid (1, 1) LilyPad
    print updatedGrid  -- Just [[Empty, Water], [Log, LilyPad]]
  
    -- Tenta atualizar uma célula fora dos limites
    let invalidUpdate = updateCell grid (2, 2) Empty
    print invalidUpdate  -- Nothing
