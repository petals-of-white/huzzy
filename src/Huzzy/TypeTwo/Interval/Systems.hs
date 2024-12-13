module Huzzy.TypeTwo.Interval.Systems
( FRule(..)
, Defuzzifier(..)
, km
) where

import Data.List
import Huzzy.Base.Sets
import Huzzy.Base.Systems
import Huzzy.TypeTwo.Interval.Sets

instance FRule (IT2Set a) where
    -- | Firing strength is membership grade of both lmf and umf.
    type Antecedent (IT2Set a) = (Double, Double)
    (=*>) (a,b) it2 = it2 { lmf = a =*> (lmf it2)
                          , umf = b =*> (umf it2)
                          }
    (=|>) (a,b) it2 = it2 { lmf = a =|> (lmf it2)
                          , umf = b =|> (umf it2)
                          }
    weight it2 b    = it2 { lmf = (lmf it2) `weight` b
                          , umf = (umf it2) `weight` b
                          }

instance Defuzzifier (IT2Set Double) where
    type Result (IT2Set Double) = (Double, Double)
    centroid its = (yl, yr)
                    where
                        (yl, yr, _, _) = km its


-- | Karnik-Mendel algorithm.
-- Currently needs a big overhaul.
km :: IT2Set Double -> ( Double -- yl
                  , Double -- yr
                  , Int -- k l
                  , Int -- k r
                  )
km its = case findK 0 yI xs of
            Nothing -> error "No k 1"
            Just k  -> revCompCheck yI k
          where
            lrsup          = unzip $ support its
            xs             = getXS lrsup
            (wsl, wsu)     = getWS its xs
            weightsI       = getWeights (wsl, wsu)
            yI             = weightedSum xs weightsI
            doLeft k' yi'  = case findK k' yi' xs of
                                    Nothing -> error ("No k 2, k:" ++ show k' ++ " yi:" ++ show yi' )
                                    Just k  -> revCompCheck yi' k'
            revCompCheck yi'' k' = case y' == yi'' of
                                    True -> (y', yr, k', kr)
                                            where
                                                (yr, kr) = kmr its
                                    False -> doLeft 0 y'
                                    where
                                        ws = lWeights wsl wsu k'
                                        y' = weightedSum xs ws

kmr :: IT2Set Double -> ( Double -- yr
                        , Int -- k r
                        )
kmr its = case findK 0 yI xs of
            Nothing -> error "no k 3"
            Just k  -> revCompCheck yI k
         where
            lrsup          = unzip $ support its
            xs             = getXS lrsup
            (wsl, wsu)     = getWS its xs
            weightsI       = getWeights (wsl, wsu)
            yI             = weightedSum xs weightsI
            doRight k' yi' = case findK k' yi' xs of
                                    Nothing -> error "No k 4"
                                    Just k  -> revCompCheck yi' k'
            revCompCheck yi'' k' = case y' == yi'' of
                                    True -> (y', k')
                                    False -> doRight 0 y'
                                    where
                                        ws = rWeights wsl wsu k'
                                        y' = weightedSum xs ws

getXS :: Ord a => ([a], [a]) -- Supports
               -> [a] -- xs
getXS (ls, us) = sort $ nub $ ls ++ us

getWS :: IT2Set a -- Input set
      -> [a] -- xs
      -> ([Double], [Double]) --x_ x^-
getWS its xs = unzip $ map (\x -> x `is` its) xs

getWeights :: ([Double], [Double]) -- w_ w^-
          -> [Double] -- w
getWeights (lws, uws) = zipWith (\l u -> (l+u)/2) lws uws

weightedSum :: [Double] -> [Double] -> Double
weightedSum x w = sum (zipWith (*) x w) / sum w

findK :: Int -> Double -> [Double] -> Maybe Int
findK k y xs = if k >= length xs then Nothing else
                    case (xs !! k) <= y && y <= (xs !! k+1) of
                        True -> Just k
                        False -> findK (k+1) y xs

lWeights :: [Double] -> [Double] -> Int -> [Double]
lWeights lws uws k = r' ++ l'
                        where
                            (r',_) = splitAt k uws
                            (_,l') = splitAt k lws


rWeights :: [Double] -> [Double] -> Int -> [Double]
rWeights lws uws k = l' ++ r'
                        where
                            (l',_) = splitAt k lws
                            (_,r') = splitAt k uws

