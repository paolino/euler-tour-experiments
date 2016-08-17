
{-# language ViewPatterns #-}
import qualified Data.Map as M
import qualified Data.Set as S


import Control.Arrow hiding (right)
import Data.List hiding (delete)
import Data.Ord
import Data.Function

-- edge
type E a = (a,a)

type DynT a =  M.Map a (S.Set a)

mkMap :: Ord a => [E a] -> DynT a
mkMap =     (\xs -> xs ++ map swap xs) >>>
            sortBy (comparing fst) >>>
            groupBy ((==) `on` fst) >>> 
            map (fst . head &&& S.fromList . map snd) >>> 
            M.fromList

swap (x,y) = (y,x)

addLink x y = f x y . f y x  where
    f x y = M.insertWith S.union x (S.singleton y)

connected0 m x y = connected' (S.singleton x) where
    connected' s = case  S.minView s of
        Nothing -> False
        Just (k,s') -> let 
            s'' = m M.! k
            in if y `S.member` s'' then True 
                else connected' $ s' `S.union` s''

type DynTs a = [DynT a]

connected :: Ord a => DynTs a -> a -> a -> Bool
connected ms x y = any (\m -> connected0 m x y) ms

link :: Ord a => DynTs a -> a -> a -> DynTs a
link ms x y = let
    ([mx],ms') = partition (x `M.member`) ms
    ([my],ms'') = partition (y `M.member`) ms'
    in addLink x y (M.union mx my) : ms''


delete :: Ord a => DynTs a -> a -> a -> DynTs a
delete ms x y = let 
    ([mxy],ms') = partition (x `M.member`) ms
    sx = S.delete y $ mxy M.! x
    mxy' = M.adjust (S.delete x) y  mxy 
    (mx,my) = M.partitionWithKey (\k _ -> k `S.member` sx) mxy'
    in mx:my:ms'
    
    
    
