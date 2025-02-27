module Main where

import Data.List
import Data.Maybe
import Graphics.Gloss (Color, makeColor, text, scale, translate)
import Graphics.Gloss.Interface.Pure.Game
import Grid (Cell (..), GameState (..), Grid, getCell, updateCell)
import Objetos (Regia (..), Sapo (..), Tronco (..))

--Just faz parte do tipo Maybe em Haskell. O tipo Maybe é usado para representar valores opcionais
--ou seja, algo que pode existir (Just valor) ou não (Nothing).

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
      [ [Terra, Terra, Terra, Terra, Terra, Terra, Terra],  -- Linha 0
        [Water, Water, RegiaCell (Regia (6,3) 2), Water, Water, Water, Water],  -- Linha 1
        [Water, Water, Water, TroncoCell (Tronco (0,5) 1), TroncoCell (Tronco (0,5) 1), Water, Water],  -- Linha 2
        [Water, Water, Water, RegiaCell (Regia (6,3) 2), Water, Water, Water],  -- Linha 3
        [Water, Water, Water, TroncoCell (Tronco (0,5) 1), TroncoCell (Tronco (0,5) 1), Water, Water],  -- Linha 4
        [RegiaCell (Regia (6,3) 2), Water, Water, Water, Water, Water, Water],  -- Linha 5
        [Terra, Terra, Terra, SapoCell (Sapo (3,6) 3), Terra, Terra, Terra]   -- Linha 6
      ],
    timeSinceLastMove = 0.1
  }

-- Função de renderização
render :: GameState -> Picture
render state =
  pictures $
    [ translate (fromIntegral x * 50 - 100) (fromIntegral y * 50 - 100) (cellToPicture cell)
      | (y, row) <- zip [0 ..] (grid state),
        (x, cell) <- zip [0 ..] row
    ]
    ++ victory
  where
    cellToPicture :: Cell -> Picture
    cellToPicture Empty = color white (rectangleWire 50 50)
    cellToPicture Terra = color brown (rectangleSolid 50 50) 
    cellToPicture Water = color lightBlue (rectangleSolid 50 50)
    cellToPicture (TroncoCell _) = color darkBrown (rectangleSolid 50 50)
    cellToPicture (RegiaCell _) = color lightGreen (rectangleSolid 50 50)
    cellToPicture (SapoCell _) = color darkGreen (circleSolid 20)

    -- Se o sapo estiver na linha 0, exibir mensagem de vitória
    victory =
      case findSapo (grid state) of
        Just (_, 0) -> [translate (-80) 0 (scale 0.2 0.2 (text "Parabens! Voce nao lavou o pe!"))]
        _ -> []

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
moveObjects = zipWith moveRow [0 ..]
  where
    initialSapoRowIndex = 6 -- Índice da linha inicial do sapo (linha 6)
    finalSapoRowIndex = 0 -- Índice da linha inicial do sapo (linha 6)
    moveRow :: Int -> [Cell] -> [Cell]
    moveRow rowIndex row
      | rowIndex `elem` [initialSapoRowIndex, finalSapoRowIndex] = row  -- Mantemos a linha do sapo fixa
      | rowIndex `elem` [2, 4] = moveBy 1 row  -- Troncos nas linhas 2 e 5 se movem 1 posição
      | rowIndex `elem` [1, 3, 5] = moveBy 2 row  -- Vitórias-régias nas linhas 2, 4 e 6 se movem 2 posições
      | otherwise = row
      where
        moveBy n list = drop n list ++ take n list  -- Função para deslocar elementos ciclicamente

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
                          (SapoCell (Sapo initialSapoPos 3))
                      )
                  Nothing -> grid
              _ ->
                -- Movimentação normal do sapo
                case updateCell grid (x, y) Empty of
                  Just gridWithoutSapo ->
                    fromMaybe
                      grid
                      (updateCell gridWithoutSapo newPos (SapoCell (Sapo (newX, newY) 3))) 
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
main = play
    (InWindow "OSapoNaoLavaOPe" (400, 400) (100, 100))  -- Janela do jogo
    white-- Cor de fundo
    30-- Taxa de atualização (FPS)
    initialState-- Estado inicial
    render-- Função de renderização
    handleInput-- Função de entrada
    update-- Função de atualização