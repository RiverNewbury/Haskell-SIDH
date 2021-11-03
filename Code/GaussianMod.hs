{-
Module      : gaussianMod
Description : Comtains defintion for the gaussian ℤ mod p (where p∈ℤ)
Maintainer  : River
-}

-------- Langauge Options ------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module GaussianMod
( Modℂ,
  inv,
  conjugate,
  showModless,
  ι
) where 

-------- Imports ---------------------------------------------------------------

import GHC.Natural (Natural(..), naturalToInteger)
import GHC.TypeNats (Nat, KnownNat, natVal)

-------- Datatypes -------------------------------------------------------------

{- The base value of a Mod m is the one where 0 ≤ real, imag < m

INV : All Mod input(s) of f is base values ⇒ all mod output(s) of f is base valued
  - f is any function in this module

-}
data Modℂ (m :: Nat) = Mod {real :: Natural, imag :: Natural}

-------- Instances -------------------------------------------------------------

instance KnownNat m => Show (Modℂ m) where
    show m = showModless m ++ " (mod " ++ show (natVal m) ++ ")"

instance KnownNat m => Eq (Modℂ m) where
    (==) c1 c2 = real c1 `mod` natVal c1 == real c2 `mod` natVal c2 && imag c1 `mod` natVal c1 == imag c2 `mod` natVal c2

instance KnownNat m => Num (Modℂ m) where
    (+) c1 c2 = baseValue (Mod (real c1 + real c2) (imag c1 + imag c2))
    (*) c1 c2 = baseValue (Mod (real c1 * real c2) (real c1 * imag c2 + real c2 * imag c1) - baseValue (Mod (imag c1 * imag c2) 0))
    abs c1 = c1
    signum c1 = (Mod 1 0)
    fromInteger z
        | z < 0 = negate $ fromInteger (-z)
        | otherwise = baseValue (Mod (fromInteger z) 0)
    negate c1 = baseValue (Mod (natVal c1 - real c1) (natVal c1 - imag c1))

-------- Helper Functions ------------------------------------------------------


-- inv a n finds 1/a (mod n)
inv' :: Integer -> Integer -> Integer

-- Extended euclidean algorithm : extGCD a b gives (x,y,z) s.t a*x + b+y = z = gcd(a,b)
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)

-- Given a gaussian Int reduces it to base value
baseValue :: KnownNat m => Modℂ m -> Modℂ m

-- Returns the value if imaginary component is 0 ow errors
backtoInteger :: Modℂ m -> Integer

inv' a n
    | (z /= 1) = error ("Can't find 1/" ++ show a ++" (mod " ++ show n ++ ") as " ++ show a ++ " and " ++ show n ++" aren't coprime")
    | otherwise = mod x n
    where (x,y,z) = extGCD a n


extGCD 0 0 = error "extGCD 0 0 not defined"
extGCD a 0 = (1,0,a)
extGCD 0 b = (0,1,b)
extGCD a b = (x, g-q*x, y)
    where
        (q,r) = divMod a b
        (g,x,y) = extGCD b r


baseValue m = Mod (mod (real m) (natVal m)) (mod (imag m) (natVal m))


backtoInteger (Mod x 0) = naturalToInteger x
backtoInteger _ = error "Needs to have no imaginary component to be converted to an ℤ"

-------- Main Functions --------------------------------------------------------

-- Defines the imaginary constant (only way to create is to say x + y*ι)
ι :: Modℂ m

-- Returns complex conjugate of point (in base form)
conjugate :: KnownNat m => Modℂ m -> Modℂ m

-- Returns the modular inverse of a gaussian Int (if possible)
inv :: KnownNat m => Modℂ m -> Modℂ m

-- Shows a general ℂ number without mod
showModless :: Modℂ m -> String

ι = Mod 0 1

conjugate c1 = (Mod (real c1) (natVal c1 - imag c1))

-- 1/c1 = conj(c1)/(c1*conj(c1)) = conj(c1)* inv (c1*conj(c1))
inv c1 = baseValue $ conj * (fromInteger $ inv' denom (naturalToInteger $ natVal c1))
    where
        conj = conjugate c1
        -- denom ∈ ℝ - so can use inv' on it to get mod inverse
        denom = backtoInteger (conj * c1)

showModless m
    | imag m == 0 = show (real m)
    | real m == 0 = show (imag m) ++  "i"
    | otherwise   = "(" ++ show (real m) ++ " + " ++ show (imag m) ++  "i)"
