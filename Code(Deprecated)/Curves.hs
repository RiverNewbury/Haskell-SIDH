{-
Module      : Curves
Description : Contains information describing the Eliptic curves
Maintainer  : River
-}

-------- Langauge Options ------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}


module Curves
    ( EllipticCurve(..)
    ) where

-------- Imports ---------------------------------------------------------------

import GaussianMod (Modℂ, showModless)
import GHC.TypeNats (Nat, KnownNat, natVal)

-------- Datatypes -------------------------------------------------------------

data EllipticCurve (m::Nat) = ShortWeierstrass {a :: Modℂ m, b :: Modℂ m}
                   | Montgomery       {a :: Modℂ m, b :: Modℂ m}
                   | Edwards          {c :: Modℂ m, d :: Modℂ m} deriving (Eq)

instance KnownNat m => Show (EllipticCurve m) where
    show curve = case curve of {
        (ShortWeierstrass _ _) -> "y^2 = x^3 + " ++ showModless (a curve) ++ "*x + " ++ showModless (b curve) ++ " (mod " ++ show (natVal curve) ++ ")";
        (Montgomery _ _)       -> showModless (b curve) ++ "*y^2 = x^3 + " ++ showModless (a curve) ++ "*x^2 + x (mod " ++ show (natVal curve) ++ ")";
        (Edwards _ _)          -> "x^2 + y^2 = " ++ showModless (c curve * c curve) ++  " * (1 + " ++ showModless (d curve) ++ "*(xy)^2) (mod " ++ show (natVal curve) ++ ")"}

-------- Helper Functions ------------------------------------------------------

-------- Main Functions --------------------------------------------------------
