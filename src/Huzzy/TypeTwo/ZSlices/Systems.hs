module Huzzy.TypeTwo.ZSlices.Systems
( FRule(..)
, Defuzzifier(..)
) where

import Data.Function(on)
import Data.List(sortBy, nub)
import Huzzy.Base.Sets
import Huzzy.Base.Systems
import Huzzy.TypeOne.Sets
import Huzzy.TypeOne.Systems
import Huzzy.TypeTwo.Interval.Sets
import Huzzy.TypeTwo.Interval.Systems
import Huzzy.TypeTwo.ZSlices.Sets

-- | In zSlices type-2 fuzzy sets, both implicators are the same.
instance FRule (T2ZSet Double) where
    type Antecedent (T2ZSet Double) = T1Set Double
    (=*>) t1 t2 = (cylExtT2 t1 (zLevels t2)) ?|| t2
    (=|>) t1 t2 = (cylExtT2 t1 (zLevels t2)) ?|| t2
    weight t2 x = t2 {zSlices = map (\it2 -> weight it2 x) (zSlices t2)}

instance Defuzzifier (T2ZSet Double) where
    type Result (T2ZSet Double) = T1Set Double
    centroid t2s = unsafeMkT1 (ldom++rdom) $ discrete disPairs
                    where
                        its      = zSlices t2s
                        (ldom, rdom) = unzip $ support (head its)
                        (ls, us) = unzip $ map centroid its
                        zs       = zLevelAxis (length its)
                        disPairs = sortBy (flip compare `on` snd ) $ zip ls zs ++ zip us zs