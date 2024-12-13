module Huzzy.TypeOne.Systems
( FRule(..)
, Defuzzifier(..)
) where

import Huzzy.Base.Sets
import Huzzy.Base.Systems
import Huzzy.TypeOne.Sets

-- | Simple type-1 fuzzy rule systems.
instance FRule (T1Set a) where
    -- | Firing strength of Type-1 rules is just membership grade.
    type Antecedent (T1Set a) = Double
    (=*>) a t1s  = t1s { mf = a =*> (mf t1s)}
    (=|>) a t1s  = t1s { mf = a =|> (mf t1s)}
    weight t1s b = t1s {mf = (mf t1s) `weight` b}

instance Defuzzifier (T1Set Double) where
    type Result (T1Set Double) = Double
    -- | Centroid can be a computationally costly operation.
    -- Reducing resolution of the domain can reduce costs.
    centroid t1s = sum (zipWith (*) dom' fdom) / sum fdom
                    where
                        dom'     = dom t1s
                        (MF f)  = mf t1s
                        fdom     = map f dom'