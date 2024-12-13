module Huzzy.TypeTwo.ZSlices.Sets
( T2ZSet(zLevels, zSlices, zdom)
, Fuzzy(..)
, FSet(..)
, contZT2
, discZT2
, unsafeZT2
, cylExtT2
, mkT2Tri
, zLevelAxis
)where

import Data.Function(on)
import Data.List(sortBy)
import Huzzy.Base.Sets
import Huzzy.TypeOne.Sets
import Huzzy.TypeTwo.Interval.Sets

-- | A zSlices based type-2 set requires the number of z levels, and a list of zslices.
data T2ZSet a = T2ZS { zLevels :: Int
                     , zSlices :: [IT2Set a]
                     , zdom    :: [a]
                     }
-- | Operations on zSlices fuzzy sets are simply defined as higher order funcitons over the list of zSlices.
instance Fuzzy (T2ZSet a) where
    a ?&& b = a { zLevels = zLevels a, zSlices = zipWith (?&&) (zSlices a) (zSlices b) }
    a ?|| b = a { zLevels = zLevels a, zSlices = zipWith (?||) (zSlices a) (zSlices b) }
    fnot a  = a { zLevels = zLevels a, zSlices = map (fnot) (zSlices a) }

-- | Currently the most complex supported fuzzy set.
instance FSet (T2ZSet a) where
    -- | Single value of the domain.
    type Value (T2ZSet a)    = a
    -- | Supprt in zSlices only works on the base interval set.
    type Support (T2ZSet a)  = [(a,a)]
    -- | Type-2 membership functions return a vertical slice, a type-1 membership function.
    type Returned (T2ZSet a) = MF Double
    support s = support (head $ zSlices s)
    hedge d s = s { zSlices = map (hedge d) (zSlices s)}
    x `is` s  = discrete disPairs
                where
                    its      = zSlices s
                    (ls, us) = unzip $ map (x`is`) its
                    zs       = zLevelAxis (length its)
                    -- | Order the list to ensure maximum z value is returned in case of multiple z values existing for a given u.
                    disPairs = sortBy (flip compare `on` snd ) $ zip ls zs ++ zip us zs


zLevelAxis :: Int -> [Double]
zLevelAxis n = 0 : (count step (n'-1))
                where
                    n' = fromIntegral $ n-1
                    step = 1/n'
                    count s 0 = [s*n']
                    count s z = (s*(n'-z)) : count s (z-1)


-- | Smart constructor for continuous type-2 fuzzy membership functions.
--  Works only on the base interval set, make sure you trust your zSlices.
contZT2 :: (Enum a, Num a) => a -> a -> a -> [IT2Set a] -> T2ZSet a
contZT2 minB maxB res its = case check of
                                True -> error "Truth values must be in the range [0..1]"
                                False -> case check' of
                                    True -> error "Truth values must be in the range [0..1]"
                                    False ->  T2ZS { zLevels = length its
                                                   , zSlices = its
                                                   , zdom    = domain
                                                   }
                            where
                                (MF lf, MF uf) = (lmf $ head its, umf $ head its)
                                domain = [minB, minB+res .. maxB]
                                check  = any (\x -> x > 1 || x < 0) (map lf domain)
                                check' = any (\x -> x > 1 || x < 0) (map uf domain)

-- | Smart constructor for discrete type-2 fuzzy membership functions.
--  Works only on the base interval set, make sure you trust your zSlices.
discZT2 :: [a] -> [IT2Set a] -> T2ZSet a
discZT2 dom its = case check of
                    True -> error "Truth values must be in the range [0..1]"
                    False -> case check' of
                            True -> error "Truth values must be in the range [0..1]"
                            False ->  T2ZS { zLevels = length its
                                            , zSlices = its
                                            , zdom    = dom
                                           }
                    where
                        (MF lf, MF uf) = (lmf $ head its, umf $ head its)
                        check  = any (\x -> x > 1 || x < 0) (map lf dom)
                        check' = any (\x -> x > 1 || x < 0) (map uf dom)

-- | Unsafe constructor, only use if you trust your membership functions or domain is very large.
unsafeZT2 :: [a] -> [IT2Set a] -> T2ZSet a
unsafeZT2 dom its = T2ZS { zLevels = length its
                         , zSlices = its
                         , zdom    = dom
                         }

-- | Used in defuzzification.
cylExtT2 :: T1Set Double -> Int -> T2ZSet Double
cylExtT2 s z = T2ZS { zLevels = z
                    , zSlices = map (\(l, r) -> cylExt l r) lsrs
                    , zdom = []
                    }
                where
                    zs = zLevelAxis z
                    lsrs = map (findCuts s) zs

-- | Constructor for triangular type-2 fuzzy set.
-- Arguements are pairs of points for defining a base Interval type-2 fuzzy set.
-- The left element of each pair is for the lower membership function,
-- The right element is for the upper membership function,
-- Order is: left corner, peak, right corner.
-- Int is number of zSlices desired, the level of discretisation.
mkT2Tri :: (Double, Double) ->
           (Double, Double) ->
           (Double, Double) ->
           Int -> T2ZSet Double
mkT2Tri (a,a') (b,b') (c,c') z = T2ZS { zLevels = z
                                      , zSlices = base : rc (z-1) stepA stepC
                                      , zdom = dom }
                                where
                                    dom    = [min a a' .. max c c']
                                    base   = unsafeMkIT2 dom (tri a b c) (tri a' b' c')
                                    stepA  = ((a-a')/fromIntegral (z-1))/2
                                    stepC  = ((c-c')/fromIntegral (z-1))/2
                                    rc 0 _ _   = []
                                    rc z sa sc = (unsafeMkIT2
                                        [min (a-sa) (a'-sa) .. max (c-sc) (c'-sc)]
                                        (tri (a-sa) b (c-sc))
                                        ((tri (a'-sa) b' (c'-sc))))
                                        : (rc (z-1) (sa+stepA) (sc+stepC))
