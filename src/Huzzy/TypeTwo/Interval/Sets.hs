module Huzzy.TypeTwo.Interval.Sets
( IT2Set(lmf, umf, idom)
, Fuzzy(..)
, FSet(..)
, contIT2
, discIT2
, unsafeMkIT2
, cylExt
)where

import Huzzy.Base.Sets
import Huzzy.TypeOne.Sets

-- | Interval Type-2 Fuzzy sets.
-- Defined entirely by the footprint of uncertainty,
-- lmf and umf are the bounds of this area.
data IT2Set a = IT2S { lmf :: MF a
                     , umf :: MF a
                     , idom :: [a]
                     }

-- | Interval Type-2 fuzzy sets allow us to work in type-1 concepts.
-- Operators are defined through application to lower and upper membership functions.
instance Fuzzy (IT2Set a) where
    a ?&& b     = a { lmf = lmf a ?&& lmf b, umf = umf a ?&& umf b}
    a ?|| b     = a { lmf = lmf a ?|| lmf b, umf = umf a ?|| umf b}
    fnot a      = a { lmf = fnot (lmf a), umf = fnot (umf a)}

-- | Enables use of support, hedge and `is` on interval type-2 fuzzy sets.
instance FSet (IT2Set a) where
    -- | Single value from the domain.
    type Value (IT2Set a)        = a
    -- | List of pairs with non-zero membership to lmf and umf.
    type Support (IT2Set a)      = [(a,a)]
    -- | Applying a value to an interval set gives an interval membership value.
    type Returned (IT2Set a)     = (Double, Double)
    support s = filter (\(x,y) -> (fst $ xis x) > 0 || (snd $ xis y) > 0) d
                where
                    xis = \x -> x `is` s
                    d = zip (idom s) (idom s)

    hedge p s = s { lmf = MF (\x -> lmf' x)
                  , umf = MF (\x -> umf' x)
                  }
                where
                    (MF l) = lmf s
                    (MF u) = umf s
                    lmf' x | l x == 0 = 0
                           | otherwise = l x ** p
                    umf' x | u x == 0 = 0
                           | otherwise = u x ** p
    x `is` s  = (l x, u x)
                where
                    (MF l) = lmf s
                    (MF u) = umf s

{-
instance FSet (IT2Set a) a (a,a) (Double, Double) where
    support s = filter (\(x,y) -> (fst $ xis x) > 0 || (snd $ xis y) > 0) d
                where
                    xis = \x -> x `is` s
                    d = zip (idom s) (idom s)

    hedge p s = s { lmf = MF (\x -> lmf' x)
                  , umf = MF (\x -> umf' x)
                  }
                where
                    (MF l) = lmf s
                    (MF u) = umf s
                    lmf' x | l x == 0 = 0
                           | otherwise = l x ** p
                    umf' x | u x == 0 = 0
                           | otherwise = u x ** p
    x `is` s  = (l x, u x)
                where
                    (MF l) = lmf s
                    (MF u) = umf s
-}

-- | Smart constructor for continuos interval type-2 membership functions. Watch that resolution!
contIT2 :: (Num a, Enum a) => a -> a -> a -> MF a -> MF a -> IT2Set a
contIT2 minB maxB res (MF lmf) (MF umf) = case check of
                                            True -> error "Truth values must be in the range [0..1]"
                                            False -> case check' of
                                                True -> error "Truth values must be in the range [0..1]"
                                                False -> IT2S { lmf = MF lmf
                                                               , umf = MF umf
                                                               , idom = domain
                                                               }
                                            where
                                                domain = [minB, minB+res .. maxB]
                                                check  = any (\x -> x > 1 || x < 0) (map lmf domain)
                                                check' = any (\x -> x > 1 || x < 0) (map umf domain)

-- | Smart constructor for discrete interval type-2 membership functions. Be wary of domain size.
discIT2 :: [a] -> MF a -> MF a -> IT2Set a
discIT2 dom (MF lmf) (MF umf) = case check of
                                            True -> error "Truth values must be in the range [0..1]"
                                            False -> case check' of
                                                True -> error "Truth values must be in the range [0..1]"
                                                False -> IT2S { lmf = MF lmf
                                                               , umf = MF umf
                                                               , idom = dom
                                                               }
                                            where
                                                check  = any (\x -> x > 1 || x < 0) (map lmf dom)
                                                check' = any (\x -> x > 1 || x < 0) (map umf dom)

-- | Only use this if you trust your functions or have no other recourse.
unsafeMkIT2 :: [a] -> MF a -> MF a -> IT2Set a
unsafeMkIT2 dom lmf umf = IT2S { lmf = lmf
                               , umf = umf
                               , idom = dom }

-- | Used in zSlices type-2 defuzzification
cylExt :: Double -> Double -> IT2Set a
cylExt l u = unsafeMkIT2 [] (singleton l) (singleton u)