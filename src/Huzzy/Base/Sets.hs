module Huzzy.Base.Sets
( MF(..)
, Fuzzy(..)
, FSet(..)
, tCo
, tGodel
, tProd
, tLuk
, tDras
, tNilMin
, tHam
, discrete
, singleton
, tri
, trap
, bell
, gaus
, up
, down
, sig
) where

-- | Type representing type-1 membership functions.
newtype MF a = MF (a -> Double)
-- | Used internally to represent type-1 membership functions.
-- Not exported for safety reasons.
-- Library users, use the newtype.
type MF' a = a -> Double

-- | FuzOp is used to denote functions expecting operators on fuzzy sets.
type FuzOp a = a -> a -> a

infixr 3 ?&&
infixr 2 ?||
-- | Standard operations on fuzzy sets.
-- Instantiated for each kind of fuzzy set.
-- If you want to overload with a t-norm, instantiate against a newtype or instantiated set.
class Fuzzy a where
    -- | Intersection over fuzzy values.
    (?&&) :: a -> a -> a
    -- | Union over fuzzy values.
    (?||) :: a -> a -> a
    -- | Fuzzy complement.
    fnot  :: a -> a

-- | Standard definitions for operations as defined by Zadeh (1965)
instance Fuzzy Double where
    -- | Equivalent to use of the Godel t-conorm,
    -- > (?&&) = tCo tGodel
    (?&&)  = min
    -- | Equivalent to use of the Godel t-norm,
    -- > (?||) = tGodel
    (?||)  = max
    fnot x = 1 - x

-- | Fuzzy operators for membership functions.
instance (Fuzzy b) => Fuzzy (a -> b) where
    f ?&& g      = \x -> f x ?&& g x
    f ?|| g      = \x -> f x ?|| g x
    fnot f       = fnot (\x -> f x)

instance Fuzzy (MF a) where
    (MF f) ?&& (MF g) = MF (f ?&& g)
    (MF f) ?|| (MF g) = MF (f ?|| g)
    fnot (MF f)       = MF (fnot f)

-- | Instance for tuple needed for interval type-2 fuzzy sets.
instance (Fuzzy a, Fuzzy b) => Fuzzy (a, b) where
  (a, b) ?&& (c, d) = (a ?&& c, b ?&& d)
  (a ,b) ?|| (c, d) = (a ?|| c, b ?|| d)
  fnot (a, b) = (fnot a, fnot b)

instance Num (MF a) where
  (MF f) + (MF g) = MF (\x -> f x + g x)
  (MF f) * (MF g) = MF (\x -> f x * g x)
  (MF f) - (MF g) = MF (\x -> f x - g x)
  abs (MF f)      = MF (\x -> abs (f x))
  signum (MF f)   = MF (\x -> signum (f x))
  fromInteger n   = MF (\x -> fromInteger n)

-- | Specifically for fuzzy sets, as opposed to fuzzy values.
-- Support is all elements of domain for which membership is non-zero.
-- Hedge is a modifier of fuzzy sets.
-- `is` is for application of a value to a fuzzy set.
class FSet a where
  -- | A single value of the domain.
  type Value a
  -- | A list of values from the domain for which membership is non-zero.
  type Support a
  -- | Degree of membership from applying a value to membership function.
  type Returned a
  support :: a -> Support a
  hedge   :: Double -> a -> a
  is      :: Value a -> a -> Returned a

{- Old functional dependency definition, remains for
report purposes.
class FSet a b c d | a -> b, a -> c, a -> d where
  support :: a -> [c]
  hedge   :: Double -> a -> a
  is      :: b -> a -> d
-}

-- | Produces the dual t-conorm from a t-norm
tCo :: (Num a, Fuzzy a) => FuzOp a -> a -> a -> a
tCo tNo a b = (-) 1 $ tNo (1 - a) (1 - b)

-- | Standard t-norm used for intersection.
tGodel :: (Fuzzy a, Ord a) => FuzOp a
tGodel = min

tProd :: (Fuzzy a, Num a) => FuzOp a
tProd = (*)

tLuk :: (Fuzzy a, Num a, Ord a) => FuzOp a
tLuk a b = max 0 (a + b - 1)

tDras :: (Fuzzy a, Eq a, Num a) => FuzOp a
tDras a b | a == 1 = b
          | b == 1 = a
          | otherwise = 0

tNilMin :: (Fuzzy a, Eq a, Num a, Ord a) => FuzOp a
tNilMin a b | a + b > 1 = min a b
            | otherwise = 0

tHam :: (Fuzzy a, Eq a, Num a, Fractional a) => FuzOp a
tHam a b | a == b && b == 0 = 0
         | otherwise        = a*b/a+b-a*b

support' :: [a] -> MF' a -> [a]
support' xs f = filter (\x -> f x > 0) xs

hedge' :: Double -> MF' a -> MF' a
hedge' p f x | f x == 0 = 0
            | otherwise = f x ** p

very', extremely', somewhat', slightly' :: MF' a -> MF' a
very'      = hedge' 2
extremely' = hedge' 3
somewhat'  = hedge' 0.5
slightly'  = hedge' (1/3)

-- | Ensure that input list is correctly ordered for desired performance.
-- I.e. if desired property is that for a u with 2 values of z, max is chosen, order descending on right value of tuple.
discrete :: Eq a => [(a, Double)] -> MF a
discrete vs = MF (\x -> discrete' vs x)

discrete' :: Eq a => [(a, Double)] -> MF' a
discrete' vs x = case lookup x vs of
                  Just t -> t
                  Nothing -> 0

-- | Used for type-2 defuzzification.
singleton :: Double -> MF a
singleton d = MF (\x -> singleton' d x)

singleton' :: Double -> MF' a
singleton' d x = d

up :: Double -> Double -> MF Double
up a b = MF (\x -> up' a b x)

up' :: Double -> Double -> MF' Double
up' a b x
  | x < a = 0
  | x < b = (x - a) / (b - a)
  | otherwise = 1

down :: Double -> Double -> MF Double
down a b = MF (\x -> down' a b x)

down' :: Double -> Double -> MF' Double
down' a b x
    | x < a = 1.0
    | x < b = (x-b)/(a-b)
    | otherwise = 0.0

tri :: Double -> Double -> Double -> MF Double
tri a b c = MF (\x -> tri' a b c x)

tri' :: Double -> Double -> Double -> MF' Double
tri' a b c x | x <= a = 0
             | a <= x && x <= b = (x-a)/(b-a)
             | b <= x && x <= c = (c-x)/(c-b)
             | c <= x = 0

trap :: Double -> Double -> Double -> Double -> MF Double
trap a b c d = MF (\x -> trap' a b c d x)

trap' :: Double -> Double -> Double -> Double -> MF' Double
trap' a b c d x | x <= a || d <= x = 0
                | a <= x && x <= b = (x-a)/(b-a)
                | b <= x && x <= c = 1
                | c <= x && x <= d = (d-x)/(d-c)
                | otherwise = 0

gaus :: Double -> Double -> MF Double
gaus sig c = MF (\x -> gaus' sig c x)

gaus' :: Double -> Double -> MF' Double
gaus' sig c x = let e = exp 1 in e**(top/bottom)
                where
                  top = negate $ (x-c)**2
                  bottom = 2*(sig**2)

bell :: Double -> Double -> Double -> MF Double
bell a b c = MF (\x -> bell' a b c x)

bell' :: Double -> Double -> Double -> MF' Double
bell' a b c x = 1/(1+abs (((x-c)/a)**(2*b)))

sig :: Double -> Double -> MF Double
sig a c = MF (\x -> sig' a c x)

sig' :: Double -> Double -> MF' Double
sig' a c x = 1/(1+exp(-a*(x-c)))
