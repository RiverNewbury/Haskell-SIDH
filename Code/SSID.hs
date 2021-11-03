{-
Module      : SSID
Description : Module for computer SSID
Maintainer  : River
-}

-------- Langauge Options ------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module SSID
(

) where

-------- Imports ---------------------------------------------------------------

import Points
import GaussianMod(Modℂ, ι, inv)
--import IntVectorSpace
import GHC.TypeNats (Nat, KnownNat, natVal)

-------- Datatypes -------------------------------------------------------------

-------- Helper Functions ------------------------------------------------------

-- Calculates the J Invariant of a Elliptic Curve
jInvariant :: KnownNat m => EllipticCurve m -> Modℂ m

-- twoIsogenyCurve p : Calculates the output curve of the unique 2 isogeny with p as it's kernal
twoIsogenyCurve :: KnownNat m => Point m -> EllipticCurve m

-- twoIsogney p1 p2 : Caculates the image of p2 under unique 2 isogney with p1 as it's kernal
twoIsogeny :: KnownNat m => Point m -> Point m -> Point m

-- threeIsogenyCurve p : Calculates the output curve of the unique 3 isogeny with p as it's kernal
threeIsogenyCurve :: KnownNat m => Point m -> EllipticCurve m

-- threeIsogeny p1 p2 : Caculates the image of p2 under unique 3 isogney with p1 as it's kernal
threeIsogeny :: KnownNat m => Point m -> Point m -> Point m


jInvariant c = case c of {
    (Montgomery _ _) -> let a2 = (a c)^2 in 256 * (a2 - 3)^3 * inv (a2 - 4);
    _ -> error "not yet defined"}

twoIsogenyCurve p = case (curve p) of {
    (Montgomery _ b1) -> let x1 = x p in Montgomery {a = 2*(1-2*x1^2), b = b1*x1};
    _ -> error "not yet defined"
}

twoIsogeny p1 p2
    | (curve p1) /= (curve p2) = error "not on the same curve"
    | otherwise = case (curve p1) of { (Montgomery _ _) -> if p1 == p2 || p2 == Zero then Zero
                      else let denom = inv (x2 - x1) in
                           let x3 = x2*(x2*x1 - 1)*denom in
                           let y3 = y2*x1*(x2^2 - 2*x1*x2 + 1)*denom^2 in
                           Point (twoIsogenyCurve p1) x3 y3 (o p1);
    _ -> error "not yet defined"}
    where
        x1 = x p1
        x2 = x p2
        y1 = y p1
        y2 = y p2

threeIsogenyCurve p = case (curve p) of {
    (Montgomery a1 b1) -> let x1 = x p in Montgomery {a = (a1*x1 - 6*x1^2 + 6)*x1, b = b1*x1^2};
    _ -> error "not yet defined"
}

threeIsogeny p1 p2
    | (curve p1) /= (curve p2) = error "not on the same curve"
    | otherwise = case (curve p1) of { (Montgomery _ _) -> if p1 == p2 || p2 == Zero then Zero
                      else let denom = inv (x2 - x1) in
                           let x3 = x2*(x2*x1 - 1)^2*denom^2 in
                           let y3 = y2*(x2*x1 - 1)*(x2^2*x1 - 3*x2*x1^2 + x1 + x2)*denom^3 in
                           Point (threeIsogenyCurve p1) x3 y3 (o p1);
    _ -> error "not yet defined"}
    where
        x1 = x p1
        x2 = x p2
        y1 = y p1
        y2 = y p2

-------- Constants -------------------------------------------------------------

basewA = 2
basewB = 3
baseeA = 4
baseeB = 3
basef  = 1

-- in [0, basewA^baseeA)
kA = 11
-- in [0, basewB^baseeB)
kB = 2

basen = ((basewA^baseeA) * (basewB^baseeB) * basef -1)


-- b*y^2 = x^3 + a*x^2 + x
sampleCurve = Montgomery {a = 329*ι + 423, b = 1} :: EllipticCurve 431

basepA = Point sampleCurve (100*ι + 248) (304*ι + 199) (basewA^baseeA)
basepB = Point sampleCurve (358*ι + 275) (410*ι + 104) (basewB^baseeB)
baseqA = Point sampleCurve (426*ι + 394) (51*ι + 79) (basewA^baseeA)
baseqB = Point sampleCurve (20*ι + 185) (281*ι + 239) (basewB^baseeB)


-------- Main Functions --------------------------------------------------------

-- genAPubKey pA qA k : k should be secret and less than 2^eA
genAPubKey  :: KnownNat m => Point m -> Point m -> Point m -> Integer -> (Point m, Point m)

-- genBPubKey pB qB k : k should be secret and less than 2^eB
genBPubKey  :: KnownNat m =>  Point m -> Point m -> Point m -> Integer -> (Point m, Point m)

-- genAPrivKey pA pA k : should be same k as in PubKey
genAPrivKey :: KnownNat m => Point m -> Integer -> Modℂ m

-- genBPrivKey pB pB k : should be same k as in PubKey
genBPrivKey :: KnownNat m => Point m -> Integer -> Modℂ m


-- Input sA qA qB eA - sA is secret point (i.e. pA + k*qA)
genAPubKey sA pB qB eA = genIsogeny sA pB qB (eA - 1)
    where
        -- Generates all necessary Isogenies
        genIsogeny :: KnownNat m => Point m -> Point m -> Point m -> Integer -> (Point m, Point m)
        genIsogeny sA p1 p2 (-1) = (p1, p2)
        genIsogeny sA p1 p2 i = genIsogeny (f sA) (f p1) (f p2) (i-1)
            where
                f = twoIsogeny (2^i |.| sA)

-- Input  sB pA qA eB - sB a secret point (i.e. pB + k*qB)
genBPubKey sB pA qA eB = genIsogeny sB pA qA (eB - 1)
    where
        genIsogeny :: KnownNat m => Point m -> Point m -> Point m -> Integer -> (Point m, Point m)
        genIsogeny sB p1 p2 (-1) = (p1, p2)
        genIsogeny sB p1 p2 i = genIsogeny (f sB) (f p1) (f p2) (i-1)
            where
                f = threeIsogeny (3^i |.| sB)

-- Input phipB phiqB eA and a secret key (same as used in public key)
genAPrivKey sA eA = jInvariant $ genIsogeny sA (eA - 1)
    where
        genIsogeny :: KnownNat m => Point m -> Integer -> EllipticCurve m
        genIsogeny sA 0 = twoIsogenyCurve sA
        genIsogeny sA i = genIsogeny (f sA) (i-1)
            where
                f = twoIsogeny (2^i |.| sA)

-- Input phipA phiqA eB and a secret key (same as used in public key)
genBPrivKey sB eB = jInvariant $ genIsogeny sB (eB - 1)
    where
        genIsogeny :: KnownNat m => Point m -> Integer -> EllipticCurve m
        genIsogeny sB 0 = threeIsogenyCurve sB
        genIsogeny sB i = genIsogeny (f sB) (i-1)
            where
                f = threeIsogeny (3^i |.| sB)

(aPub1, aPub2) = genAPubKey (basepA |+| kA |.| baseqA) basepB baseqB baseeA
(bPub1, bPub2) = genBPubKey (basepB |+| kB |.| baseqB) basepA baseqA baseeB
aSharedSec     = genAPrivKey (bPub1 |+| kA |.| bPub2) baseeA
bSharedSec     = genBPrivKey (aPub1 |+| kB |.| aPub2) baseeB
