module Huzzy.TypeOne.Sets
( T1Set(mf, dom)
, Fuzzy(..)
, FSet(..)
, contT1
, discT1
, unsafeMkT1
, alpha
, findCuts
)where

import Data.List(sortBy, nub, elemIndex)
import Data.Maybe(fromJust)
import Huzzy.Base.Sets


-- | Type-1 fuzzy sets, with associated membership function and domain. Use smart constructors to create.
data T1Set a = T1S { mf  :: MF a
                   , dom :: [a]
                   }

-- | Fuzzy operators are supported on T1Sets.
-- Applies operator to membership functions inside T1Set type.
instance Fuzzy (T1Set a) where
    a ?&& b = a { mf = (mf a) ?&& (mf b)}
    a ?|| b = a { mf = (mf a) ?|| (mf b)}
    fnot  a  = a { mf = fnot (mf a)}

-- | Type-1 fuzzy sets are the most basic fuzzy set.
instance FSet (T1Set a) where
    -- | Single element of the domain.
    type Value (T1Set a)        = a
    -- | List of elements from the domain with non-zero membership.
    type Support (T1Set a)      = [a]
    -- | Type-1 membership functions return a double, hopefully in the range 0 to 1.
    type Returned (T1Set a)     = Double
    support s = filter (\x -> (x `is` s)  > 0) d
                where
                    d = dom s

    hedge p s = s {mf = MF (\x -> mf' x)}
                where
                    (MF f) = mf s
                    mf' x | f x == 0 = 0
                          | otherwise = f x ** p
    x `is` s  = f x
                where
                    (MF f) = mf s
{-
instance FSet (T1Set a) a a Double where
    support s = filter (\x -> (x `is` s)  > 0) d
                where
                    d = dom s

    hedge p s = s {mf = MF (\x -> mf' x)}
                where
                    (MF f) = mf s
                    mf' x | f x == 0 = 0
                          | otherwise = f x ** p
    x `is` s  = f x
                where
                    (MF f) = mf s
-}

instance Num (T1Set a) where
  a + b = a { mf = (mf a) + (mf b)}
  a * b = a { mf = (mf a) * (mf b)}
  a - b = a { mf = (mf a) * (mf b)}
  abs a = a { mf = abs (mf a)}
  signum  a      = a {mf = signum (mf a)}
  fromInteger n  = T1S { mf  = MF $ \x -> fromInteger n
                       , dom = [] }


-- | Smart constructor for continuous membership functions. Warning, fine resolutions will make this a very slow construction.
contT1 :: (Num a, Enum a) => a -> a -> a -> MF a -> T1Set a
contT1 minB maxB res (MF mf) = case check of
                                True -> error "Truth values must be in the range [0..1]"
                                False -> T1S { mf = MF mf
                                             , dom = domain
                                             }
                                where
                                    domain = [minB, minB+res .. maxB]
                                    check  = any (\x -> x > 1 || x < 0) (map mf domain)

-- | Smart constructor for discrete continuous membership functions. Avoid large domains.
discT1 :: [a] -> MF a -> T1Set a
discT1 dom (MF mf) = case check of
                        True -> error "Truth values must be in the range [0..1]"
                        False -> T1S { mf = MF mf
                                     , dom = dom
                                     }
                        where
                            check = any (\x -> x > 1 || x < 0) (map mf dom)

-- | Only use this if you're sure your membership functions are safe, or your domain is huge.
unsafeMkT1 :: [a] -> MF a -> T1Set a
unsafeMkT1 dom mf = T1S { mf = mf
                        , dom = dom
                        }

-- | Cuts a type-1 fuzzy set at a given degree of membership.
alpha :: Double -> T1Set a -> [a]
alpha d s = filter (\x -> f x >= d) (dom s)
             where
                (MF f) = mf s

-- | Performs a cut and then finds the x values on the curve at point of cut.
findCuts :: Ord a =>  T1Set a -> Double -> (a, a)
findCuts s d = (l, r)
                where
                    as = alpha d s
                    l  = maximum as
                    li = fromJust $ elemIndex l as
                    r  = maximum (snd $ splitAt li as)

