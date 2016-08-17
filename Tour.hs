{-# language ViewPatterns#-}
module Tour where

import Data.List.NonEmpty hiding (toList, (<|))
import Data.Sequence ((|>),(<|),Seq)
import Data.Foldable (toList)

import RoseSeq

mkEulerTour :: T a -> NonEmpty a
mkEulerTour = fromList . toList . mkEulerTour' where
    mkEulerTour' :: T a -> Seq a
    mkEulerTour' (viewT -> (x,ts)) = (ts >>= (x <|) . mkEulerTour') |> x

fromEulerTour :: Eq a => NonEmpty a -> T a
fromEulerTour (x:|xs) = tree . top $ fromEulerTour' (Just $ mkZ x) xs where
    fromEulerTour' (Just z) [] = z
    fromEulerTour' (Just z) (x:xs) = case focus <$> up z of
        Just ((==) x -> True) -> fromEulerTour' (up z) xs
        _ -> fromEulerTour' (insertC x z) xs

