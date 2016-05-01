{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds ,ScopedTypeVariables #-}
module Type.ANat(ANat,anatVal,A0,A1,A2,A3,A4,A5,module GHC.TypeLits) where

import GHC.TypeLits

-- Envelope to get Nats with * kind 
data ANat (n :: Nat)

-- |Convert a Nat to the corresponding Integer
anatVal :: KnownNat n => ANat n -> Integer
anatVal = natVal

-- |Shortcuts
type A5 = ANat 5
type A4 = ANat 4
type A3 = ANat 3
type A2 = ANat 2
type A1 = ANat 1
type A0 = ANat 0

{-
t = nat (undefined :: (S (S (S Z))))
-- t = natVal (undefined :: N (S (S (S Z))))

class AsNat a where nat :: a -> Int
instance AsNat Z where nat _ = 0
instance AsNat t => AsNat (S t) where nat _ = 1 + nat (undefined::t)

data Z

data S n
{-
type family N a where
  N Z = 0
  N (S t) = 1 + N t
-}
-}


