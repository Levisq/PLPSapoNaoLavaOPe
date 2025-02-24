module Lib
    ( posInicial,
      mostraPos,
      limpaPos,
      movDir
    ) where

posInicial :: String
posInicial =  "|--------------------------S--------------------------|"
              
mostraPos :: String -> IO()
mostraPos pos = putStrLn pos

limpaPos :: String
limpaPos = ""

movDir :: String -> String
movDir posAtual = do 
              let aux = drop 1 posAtual
              let posAtualizada = init(init aux)
              "|-" ++ posAtualizada ++ "|"

-- TO DO
--identificaMov :: IO String -> String
--identificaMov mov = if mov == 'd' then movDir 
--                    else "Comando inválido!"
           
