module Main (main) where

import Lib

main :: IO ()
main = do 
    mostraPos posInicial
    input <- getLine
    mostraPos (movDir posInicial)
