
module Parse where

import Control.Arrow
import Data.List
import Data.Ord
import RoseSeq
import Tour
import Edge

-- char specialized
parseString :: String -> ([E Char],Char)
parseString s = let
    [q]:es = lines s 
    in (map ((\[[x],[y]] -> (x,y)) . words) es,q)

fromString :: String -> T Char
fromString = parseString >>> first mkMap >>> uncurry mkT


