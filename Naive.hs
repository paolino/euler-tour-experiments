
{-# language ViewPatterns #-}
import qualified Data.Map as M
import qualified Data.Set as S


import Control.Arrow hiding (right)
import Data.List hiding (delete)
import Data.Ord
import Data.Function
import Control.Monad
import System.IO

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

connected0 m x y = connected' (S.singleton x) (S.singleton x) where
    connected' s st = case  S.minView s of
        Nothing -> False
        Just (k,s') -> case M.lookup k m of
            Just s'' -> let  
                s''' = s'' `S.difference` st
                in if y `S.member` s''' then True 
                    else connected' (s' `S.union` s''') (st `S.union` s''')
            Nothing -> False

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
    

main = do
    n <- readLn
    ds <- fmap (map mkMap) . replicateM n $ do
        m <- readLn
        replicateM m $ (\[x,y] -> (head x, head y)) <$> words <$> getLine
    let f rs ds = do
            t <- isEOF
            if not t then do
                c:[x]:[y]:r <- words <$> getLine
                case c of
                    "connected" -> f ((connected ds x y == read (head r)):rs) ds
                    "link" -> f rs $ link ds x y
                    "delete" -> f rs $ delete ds x y
            else return rs
    f [] ds >>= mapM_ print . reverse

   
