{-
Module      : Points
Description : Describes the points that lie on the Eliptic Curve
Maintainer  : River
-}

-------- Langauge Options ------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module Points
(   Point(..),
    IVS(..),
    EllipticCurve(..)
) where

-------- Imports ---------------------------------------------------------------

import Curves
import IntVectorSpace
import GaussianMod (Modℂ, inv)
import GHC.TypeNats (Nat, KnownNat)

-------- Datatypes -------------------------------------------------------------

-- Zero is a simple generalisation of the Poif or other 0 if curve is closed
--  Exists almost entirely so that I can make Point at IVS
--  m should be be same as n of EllipticCurve
data Point (m :: Nat) = Point {curve :: (EllipticCurve m), x :: (Modℂ m), y :: (Modℂ m), o :: Integer} | Zero deriving (Eq)

instance KnownNat m => Show (Point m) where
    show (Point c x y o) = "Point{\n\tcurve: " ++ show c ++ "\n\tx: " ++ show x ++"\n\ty: " ++ show y ++ "\n\to: " ++ show o ++ "}"
    show Zero = "Zero"

instance KnownNat m => IVS (Point m) where
    zero = Zero
    neg = neg'
    (|+|) p1 p2
        | p1 == p2 = double p1
        | otherwise = add p1 p2


-------- Destructors -----------------------------------------------------------
-------- Helper Functions ------------------------------------------------------

-------- Main Functions --------------------------------------------------------

-- negate p gives -p i.e. the point which added to p gies 0
neg' :: KnownNat m => Point m -> Point m

-- add a b gives a + b
add :: KnownNat m => Point m -> Point m -> Point m

-- double p gives 2*p
double  :: KnownNat m => Point m -> Point m


neg' Zero = Zero
neg' (Point c x y o) =
    case c of {
        (ShortWeierstrass _ _) -> Point c x (-y) o;
        (Montgomery _ _)       -> Point c x (-y) o;
        (Edwards _ _)          -> Point c (-x) y o}


add Zero p = p
add p Zero = p
-- For Montgomery Curve uses standard formula
add (Point (Montgomery a1 b1) x1 y1 o1) (Point (Montgomery a2 b2) x2 y2 o2)
    -- Ensures that points are on the same curve
    | c1 /= c2 = error "Both points must be on the same curve"
    -- Checks if points are negations of each other to return the 0
    -- Also checks if points are same as then add alg doesn't work and we need double
    | x1 == x2 = if y1 == y2 then Zero else double (Point c1 x1 y1 o1)
    -- Otherwise does normal algorithm
    | otherwise = Point c1 x3 y3 o3
    where
        c1 = (Montgomery a1 b1)
        c2 = (Montgomery a2 b2)

        a = a1
        b = b1

        g = (y2 - y1) * inv (x2 -x1)

        x3 = b*g*g - a - x1 - x2
        y3 = (2*x1 + x2 + a)*g - b*g*g*g - y1
        o3 = o2

-- For Montgomery Curve uses standard formula
double Zero = Zero

double (Point (Montgomery a b) x y o) = Point curve x3 y3 o3
    where
        curve = (Montgomery a b)

        l = (3*x*x + 2*a*x + 1) * inv (2*b*y)

        x3 = b*l*l - a - 2*x
        y3 = (3*x + a)*l - b*l*l*l - y

        o3 = o
