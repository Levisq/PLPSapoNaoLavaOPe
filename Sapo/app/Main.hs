module Main where

import Graphics.Gloss (Color, makeColor)
import Graphics.Gloss.Interface.Pure.Game
import Data.List (findIndex, partition)
import Grid (Cell(..), Grid, GameState(..), getCell, updateCell)
import Objetos (Tronco(..), Regia(..), Sapo(..))

--Cores especificas da biblioteca Gloss
brown :: Color
brown = makeColor 0.65 0.16 0.16 1.0  -- Marrom para Terra

lightBlue :: Color
lightBlue = makeColor 0.53 0.81 0.98 1.0  -- Azul claro para Água

darkBrown :: Color
darkBrown = makeColor 0.36 0.25 0.20 1.0  -- Marrom escuro para Troncos

lightGreen :: Color
lightGreen = makeColor 0.56 0.93 0.56 1.0  -- Verde claro para Vitórias-régias

darkGreen :: Color
darkGreen = makeColor 0.0 0.5 0.0 1.0  -- Verde escuro para o Sapo

-- Estado inicial do jogo
initialState :: GameState
initialState = GameState
  { grid = 
      [ [Terra, Terra, Terra, Terra, Terra, Terra, Terra],  -- Linha 1
        [Water, Water, RegiaCell (Regia (6,3) 2), Water, Water, Water, Water],  -- Linha 2
        [Water, Water, Water, TroncoCell (Tronco (0,5) 1), TroncoCell (Tronco (0,5) 1), Water, Water],  -- Linha 3
        [Water, Water, Water, RegiaCell (Regia (6,3) 2), Water, Water, Water],  -- Linha 4
        [Water, Water, Water, TroncoCell (Tronco (0,5) 1), TroncoCell (Tronco (0,5) 1), Water, Water],  -- Linha 5
        [RegiaCell (Regia (6,3) 2), Water, Water, Water, Water, Water, Water],  -- Linha 6
        [Terra, Terra, Terra, SapoCell (Sapo (3,6) 3), Terra, Terra, Terra]   -- Linha 7
      ],
    timeSinceLastMove = 0.1
  }

-- Função de renderização
render :: GameState -> Picture
render state =
  pictures [translate (fromIntegral x * 50 - 100) (fromIntegral y * 50 - 100) (cellToPicture cell)
           | (y, row) <- zip [0..] (grid state),
             (x, cell) <- zip [0..] row]
  where
    cellToPicture :: Cell -> Picture
    cellToPicture Empty = color white (rectangleWire 50 50)
    cellToPicture Terra = color brown (rectangleSolid 50 50) 
    cellToPicture Water = color lightBlue (rectangleSolid 50 50)
    cellToPicture (TroncoCell _) = color darkBrown (rectangleSolid 50 50)
    cellToPicture (RegiaCell _) = color lightGreen (rectangleSolid 50 50)
    cellToPicture (SapoCell _) = color darkGreen (circleSolid 20)
-- Função de atualização
update :: Float -> GameState -> GameState
update deltaTime state =
  let newTime = timeSinceLastMove state + deltaTime
      moveInterval = 0.6  -- Tempo entre movimentos (em segundos)
  in if newTime >= moveInterval
     then state { grid = moveObjects (grid state), timeSinceLastMove = 0.0 }
     else state { timeSinceLastMove = newTime }

-- Função para mover troncos e vitórias-régias de forma independente
moveObjects :: Grid -> Grid
moveObjects grid = zipWith moveRow [0..] grid
  where
    initialSapoRowIndex = 6  -- Índice da linha inicial do sapo (linha 6)

    moveRow :: Int -> [Cell] -> [Cell]
    moveRow rowIndex row
      | rowIndex == initialSapoRowIndex = row  -- Mantemos a linha do sapo fixa
      | rowIndex == 3 = moveBy 1 row  -- Troncos na linha 3 se movem 1 posição
      | rowIndex == 4 = moveBy 2 row  -- Vitórias-régias na linha 4 se movem 2 posições
      | rowIndex `elem` [2, 5] = moveBy 1 row  -- Troncos nas linhas 2 e 5 se movem 1 posição
      | rowIndex `elem` [0, 1, 4] = moveBy 2 row  -- Vitórias-régias nas linhas 0, 1 e 4 se movem 2 posições
      | otherwise = row
      where
        moveBy n list = drop n list ++ take n list  -- Função para deslocar elementos ciclicamente

-- Função de entrada (movimento do sapo)
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state =
  state { grid = moveSapo (grid state) (0, 1) }
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =
  state { grid = moveSapo (grid state) (0, -1) }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state { grid = moveSapo (grid state) (-1, 0) }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state =
  state { grid = moveSapo (grid state) (1, 0) }
handleInput _ state = state  -- Ignora outros eventos

-- Função para mover o sapo
moveSapo :: Grid -> (Int, Int) -> Grid
moveSapo grid (dx, dy) =
  case findSapo grid of
    Just (x, y) ->
      let newX = x + dx
          newY = y + dy
          maxX = length (head grid) - 1
          maxY = length grid - 1
      in if newX >= 0 && newX <= maxX && newY >= 0 && newY <= maxY
         then case updateCell grid (x, y) Empty of
                Just gridWithoutSapo ->
                  case updateCell gridWithoutSapo (newX, newY) (SapoCell (Sapo (newX, newY) 3)) of
                    Just newGrid -> newGrid
                    Nothing -> grid  -- Se a nova posição for inválida, não move o sapo
                Nothing -> grid  -- Se a remoção do sapo falhar, retorna o grid original
         else grid  -- Se o sapo tentar sair do grid, não move
    Nothing -> grid  -- Se o sapo não for encontrado, retorna o grid original

-- Função para encontrar a posição do sapo no grid
findSapo :: Grid -> Maybe (Int, Int)
findSapo grid =
  let indexedRows = zip [0..] grid
  in foldl (\acc (y, row) ->
              case findIndex (== SapoCell (Sapo (0, 0) 3)) row of
                Just x -> Just (x, y)
                Nothing -> acc)
          Nothing indexedRows

-- Função principal
main :: IO ()
main = play
    (InWindow "Frogger" (400, 400) (100, 100))  -- Janela do jogo
    white                                       -- Cor de fundo
    30                                          -- Taxa de atualização (FPS)
    initialState                                -- Estado inicial
    render                                      -- Função de renderização
    handleInput                                 -- Função de entrada
    update                                      -- Função de atualização
