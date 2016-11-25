{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds ,ScopedTypeVariables #-}
-- |Nats with * kind
module Type.ANat(ANat,anatVal,A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,module GHC.TypeLits) where

import GHC.TypeLits

-- |Envelope to get Nats with * kind
data ANat (n :: Nat)

-- |Convert a Nat to the corresponding Integer
--
-- >>> anatVal (undefined::A5)
-- 5
anatVal :: KnownNat n => ANat n -> Integer
anatVal = natVal

type A0 = ANat 0
type A1 = ANat 1
type A2 = ANat 2
type A3 = ANat 3
type A4 = ANat 4
type A5 = ANat 5
type A6 = ANat 6
type A7 = ANat 7
type A8 = ANat 8
type A9 = ANat 9





