
{-# language TemplateHaskell, QuasiQuotes #-}
module Main where
import System.Console.ANSI

import Data.String.Here
import Serialize (fromString, toString)
import RoseSeq (drawT)
import Data.Foldable (toList)
import Tour (mkEulerTour, fromEulerTour)

-- example from ET-technique.pdf
t1 = [here| 
a
a b
b c
b g
b d
d e
d f
|]

drawTree = drawT (init . tail . show)
readT = fromString head 

line = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "\n============================"
    setSGR [Reset]
parag t v = do
    setSGR [SetConsoleIntensity BoldIntensity, SetUnderlining SingleUnderline]
    putStrLn $ "\n" ++ t ++ "\n"
    setSGR [Reset]
    putStrLn v
    line

-- main = putStrLn $ fromString >>> bstEulerTour  $ t1
main :: IO ()
main = do
    setTitle "Euler Tour Test"
    line
    parag "String" t1
    parag "String-Tree"  $ drawTree $ readT t1
    parag "String-Tree-Tour" $ toList . mkEulerTour $ readT t1
    parag "String-Tree-Tour-Tree" $ drawTree $ fromEulerTour .  mkEulerTour $ readT t1
    parag "String-Tree-Tour-Tree-String" $ toString return $ fromEulerTour .  mkEulerTour $ readT t1
    setTitle ""


