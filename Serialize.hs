
module Serialize (fromString, toString) where

import Control.Arrow
import Data.List
import Data.Ord
import RoseSeq
import Tour

-- char specialized
parseString :: Ord a => (String -> a) -> String -> SerT a
parseString f s = let
    q:es = lines s 
    in SerT (f q) $ map ((\[x,y] -> (f x, f y)) . words) es

fromString :: Ord a => (String -> a) -> String -> T a
fromString f = parseString f >>> rebuildT

toString :: (a -> String) -> T a -> String
toString f t = let 
    SerT x es = serializeT t
    in unlines $ f x : map (\(s,e) -> unwords [f s, f e]) es



