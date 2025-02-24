module Main where

import Graphics.Gloss (Color, makeColor)
import Graphics.Gloss.Interface.Pure.Game
import Data.List (findIndex)
import Grid (Cell(..), Grid, GameState(..), getCell, updateCell)
import Objetos (Tronco(..), Regia(..), Sapo(..))

brown :: Color
brown = makeColor 0.65 0.16 0.16 1.0 

-- Estado inicial do jogo
initialState :: GameState
initialState = GameState

  { grid = [[Empty, Water],
            [TroncoCell (Tronco (1, 1) 1), SapoCell (Sapo (0, 0) 3)],
            [Empty, Water]]
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
    cellToPicture Water = color blue (rectangleSolid 50 50)
    cellToPicture (TroncoCell _) = color brown (rectangleSolid 50 50)
    cellToPicture (RegiaCell _) = color green (rectangleSolid 50 50)
    cellToPicture (SapoCell _) = color red (circleSolid 20)

-- Função de atualização
update :: Float -> GameState -> GameState
update _ state = state  -- Por enquanto, o estado não muda

-- Função de entrada
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
      case updateCell grid (x, y) Empty of
        Just gridWithoutSapo ->
          case updateCell gridWithoutSapo (x + dx, y + dy) (SapoCell (Sapo (x + dx, y + dy) 3)) of
            Just newGrid -> newGrid
            Nothing -> grid  -- Se a nova posição for inválida, não move o sapo
        Nothing -> grid  -- Se a remoção do sapo falhar, retorna o grid original
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