module Lib
    ( posInicial--,
      --refresh,
      --movDir
    ) where
        
import Text.Printf (printf)

posInicial :: IO ()
posInicial = do 
        let aux =  "                          S                          \n"
        printf "%s" aux

--refresh :: IO()
--refresh = putStrLn ""

--toString :: IO() -> String
--toString input =  show input >>= getLine

--movDir :: IO() -> IO()
--movDir posAtual = do 
--           let input <- posAtual
--           let output = read (init input)
--            putStrLn ("" ++ output)
           
