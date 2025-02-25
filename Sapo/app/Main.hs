module Main where

import Data.List
import Data.Maybe
import Graphics.Gloss (Color, makeColor)
import Graphics.Gloss.Interface.Pure.Game
import Grid (Cell (..), GameState (..), Grid, getCell, updateCell)
import Objetos (Regia (..), Sapo (..), Tronco (..))

brown :: Color
brown = makeColor 0.65 0.16 0.16 1.0

-- Estado inicial do jogo
initialState :: GameState
initialState =
  GameState
    { grid =
        [ [Water, Water, Water, Water, RegiaCell (Regia (6, 3) 2), Water, Water], -- Linha 0
          [Water, Water, TroncoCell (Tronco (0, 5) 1), TroncoCell (Tronco (0, 5) 1), Water, Water, Water], -- Linha 1
          [Water, Water, Water, RegiaCell (Regia (6, 3) 2), Water, Water, Water], -- Linha 2
          [TroncoCell (Tronco (0, 5) 1), TroncoCell (Tronco (0, 5) 1), Water, Water, Water, Water, Water], -- Linha 3
          [Water, Water, Water, RegiaCell (Regia (6, 3) 2), Water, Water, Water], -- Linha 4
          [TroncoCell (Tronco (0, 5) 1), TroncoCell (Tronco (0, 5) 1), Water, Water, Water, Water, Water], -- Linha 5
          [Water, Water, Water, SapoCell (Sapo (3, 6) 3), Water, Water, Water] -- Linha 6
        ],
      timeSinceLastMove = 0.1
    }

-- Função de renderização
render :: GameState -> Picture
render state =
  pictures
    [ translate (fromIntegral x * 50 - 100) (fromIntegral y * 50 - 100) (cellToPicture cell)
      | (y, row) <- zip [0 ..] (grid state),
        (x, cell) <- zip [0 ..] row
    ]
  where
    cellToPicture :: Cell -> Picture
    cellToPicture Empty = color white (rectangleWire 50 50)
    cellToPicture Water = color blue (rectangleSolid 50 50)
    cellToPicture (TroncoCell _) = color brown (rectangleSolid 50 50)
    cellToPicture (RegiaCell _) = color green (rectangleSolid 50 50)
    cellToPicture (SapoCell _) = color green (circleSolid 20)

-- Função de atualização
update :: Float -> GameState -> GameState
update deltaTime state =
  let newTime = timeSinceLastMove state + deltaTime
      moveInterval = 0.5 -- Tempo entre movimentos (em segundos)
   in if newTime >= moveInterval
        then state {grid = moveObjects (grid state), timeSinceLastMove = 0.0}
        else state {timeSinceLastMove = newTime}

-- Função para mover troncos e vitórias-régias da direita para a esquerda
moveObjects :: Grid -> Grid
moveObjects = zipWith moveRow [0 ..]
  where
    initialSapoRowIndex = 6 -- Índice da linha inicial do sapo (linha 6)
    moveRow :: Int -> [Cell] -> [Cell]
    moveRow rowIndex row
      | rowIndex == initialSapoRowIndex = row -- Mantemos a linha do sapo fixa
      | otherwise =
          let moveBy n list = drop n list ++ take n list -- Função para deslocar elementos ciclicamente
              troncoRow = if rowIndex `elem` [2, 5] then moveBy 1 row else row -- Troncos se movem 1 posição
              regiaRow = if rowIndex `elem` [0, 1, 3, 4] then moveBy 2 troncoRow else troncoRow -- Vitórias-régias se movem 2 posições adicionais
           in regiaRow

-- Função de entrada (movimento do sapo)
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state =
  state {grid = moveSapo (grid state) (0, 1)}
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =
  state {grid = moveSapo (grid state) (0, -1)}
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state {grid = moveSapo (grid state) (-1, 0)}
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state =
  state {grid = moveSapo (grid state) (1, 0)}
handleInput _ state = state -- Ignora outros eventos

-- Posição inicial do sapo
initialSapoPos :: (Int, Int)
initialSapoPos = (3, 6)

-- Função para mover o sapo
moveSapo :: Grid -> (Int, Int) -> Grid
moveSapo grid (dx, dy) =
  case findSapo grid of
    Just (x, y) ->
      let newX = x + dx
          newY = y + dy
          maxX = length (head grid) - 1
          maxY = length grid - 1
          newPos = (newX, newY)
       in if newX >= 0 && newX <= maxX && newY >= 0 && newY <= maxY
            then case getCell grid newPos of
              Just Water ->
                -- Se o novo local tiver água, retorna o sapo para a posição inicial
                case updateCell grid (x, y) Empty of
                  Just gridWithoutSapo ->
                    fromMaybe
                      grid
                      ( updateCell
                          gridWithoutSapo
                          initialSapoPos
                          (SapoCell (Sapo initialSapoPos 3)) -- Correção aqui
                      )
                  Nothing -> grid
              _ ->
                -- Movimentação normal do sapo
                case updateCell grid (x, y) Empty of
                  Just gridWithoutSapo ->
                    fromMaybe
                      grid
                      (updateCell gridWithoutSapo newPos (SapoCell (Sapo (newX, newY) 3))) -- Correção aqui
                  Nothing -> grid -- Se a remoção do sapo falhar, retorna o grid original
            else grid -- Se o sapo tentar sair do grid, não move
    Nothing -> grid

-- Função para encontrar a posição do sapo no grid
findSapo :: Grid -> Maybe (Int, Int)
findSapo grid =
  let indexedRows = zip [0 ..] grid
   in foldl
        ( \acc (y, row) ->
            case findIndex
              ( \cell -> case cell of
                  SapoCell (Sapo (posX, posY) _) -> True -- Verifica se é uma célula com sapo
                  _ -> False
              )
              row of
              Just x -> Just (x, y) -- Retorna a posição (x, y) onde o sapo foi encontrado
              Nothing -> acc -- Continua procurando
        )
        Nothing -- Caso o sapo não seja encontrado
        indexedRows

-- Função principal
main :: IO ()
main =
  play
    (InWindow "Frogger" (400, 400) (100, 100)) -- Janela do jogo
    white -- Cor de fundo
    30 -- Taxa de atualização (FPS)
    initialState -- Estado inicial
    render -- Função de renderização
    handleInput -- Função de entrada
    update -- Função de atualização