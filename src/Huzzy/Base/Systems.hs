module Huzzy.Base.Systems
( FRule(..)
, Defuzzifier(..)
) where

import Huzzy.Base.Sets

infix 0 =*>
infix 0 =|>

-- | Allows overloading of functions used in rule definition.
class Fuzzy a => FRule a where
    -- | Firing strength
    type Antecedent a
    -- | Scaling implication.
    (=*>) :: Antecedent a -> a -> a
    -- | Truncate implication.
    (=|>) :: Antecedent a -> a -> a
    -- | Weight a rule
    weight :: a -> Double -> a

instance FRule Double where
    type Antecedent Double = Double
    (=*>) a b  = a * b
    (=|>) a b  = a `min` b
    weight a b = a * b

instance FRule b => FRule (a -> b) where
    type Antecedent (a -> b) = Antecedent b
    (=*>) a b  = \x -> a =*> b x
    (=|>) a b  = \x -> a =|> b x
    weight a b = \x -> a x `weight` b

instance FRule (MF a) where
    type Antecedent (MF a) = Double
    (=*>) a (MF f)  = MF (\x -> a =*> f x)
    (=|>) a (MF f)  = MF (\x -> a =|> f x)
    weight (MF f) b = MF (\x -> f x `weight` b)

-- | Overloaded defuzzification functions.
class FRule a => Defuzzifier a where
    type Result a
    centroid :: a -> Result a

ruleBase :: Fuzzy a => (a -> a -> a) -> [a] -> a
ruleBase = foldr1