module Edge where

import qualified Data.Map as M
import Control.Arrow
import Data.List
import Data.Ord
import Data.Function

-- edge
type E a = (a,a)

mkMap :: Ord a => [E a] -> M.Map a [a]
mkMap =     sortBy (comparing fst) >>>
            groupBy ((==) `on` fst) >>> 
            map (fst . head &&& map snd) >>> 
            M.fromList

