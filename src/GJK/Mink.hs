{-| Point
-}

module GJK.Mink
  ( Mink
  , calcMinkSupport
  ) where

import GJK.Point (Pt, sub, neg)

-- | Simple alias for boundary objects bundled with a support function
type Mink n a = (a, a -> Pt n -> Maybe (Pt n))

-- | Calculate the support of the Minkowski difference of two Mink's
calcMinkSupport :: (Ord n, Fractional n) => Mink n a -> Mink n b -> (n, n) -> Maybe (n, n)
calcMinkSupport (objA, suppA) (objB, suppB) d =
  let
    maybep1 = suppA objA (neg d)
    maybep2 = suppB objB d
   in
     case (maybep1, maybep2) of
       (Just p1, Just p2) -> Just (sub p1 p2)
       _                  -> Nothing
