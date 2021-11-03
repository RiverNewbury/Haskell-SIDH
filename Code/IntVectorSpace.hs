{-
Module      : IntVectorSpace
Description : Defines Class for Vector Spaces over integers
Maintainer  : River
-}

module IntVectorSpace
( IVS (..),
) where

-------- Imports ---------------------------------------------------------------

-------- Classes ---------------------------------------------------------------

{-
a is a type which is a VS over the Integers
Min Viable definition
  - zero
  - neg
  - |+|
-}
class Eq a => IVS a where
    -- The unit of addition
    zero  :: a
    -- the additive inverse
    neg :: a -> a
    -- Standard addition
    (|+|) :: a -> a -> a
    -- Standard subtraction
    (|-|) :: a -> a -> a
    (|-|) v1 v2 = v1 |+| neg v2
    -- Uses a binary ladder to compute multiplication by a large Integer
    (|.|) :: Integer -> a -> a
    (|.|) 0 v = zero
    (|.|) 1 v = v
    (|.|) x p = if (mod x 2 == 1) then p |+| recAns else recAns
        where
            recAns = (|.|) (div x 2) (p |+| p)

-- |+| binds less tightly than |.| (i.e. p1 |+| k |.| p2 will return p1 |+| (k |.| p2))
infixl 6 |+|
infixl 6 |-|
infixl 7 |.|


-------- Datatypes -------------------------------------------------------------

-------- Helper Functions ------------------------------------------------------

-------- Main Functions --------------------------------------------------------
