{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Type.Analyse(Ana,App,Typ,module Type.ANat) where

import Type.ANat

test :: App (App (Typ (Either A0 A1)) (App (Typ (Maybe A0)) (Typ Bool))) (App (App (App (App (Typ (A0, A1, A2, A3)) (Typ Int)) (App (Typ (Maybe A0)) (Typ Integer))) (Typ Char)) (Typ Integer))
test = undefined :: Ana (Either (Maybe Bool) (Int,Maybe Integer,Char,Integer))

-- | Convert a type with specific parameters to a type applied to variables (or more precisely types standing for variables)
-- |For example convert 'Either Word (Maybe Char)' to something equivalent to '(Either a b) Word ((Maybe a) Char)' or '(\a b -> Either a b) Word ((\a -> Maybe a) Char)'
-- TODO: Define recursively to fix:
-- BUG: silently fails for unsupported arities
type family Ana t where
   -- Ana (f a b) = App (Ana (f a)) (Ana b)
   Ana (f a b c d) = App (App (App (App (Typ (f A0 A1 A2 A3)) (Ana a)) (Ana b)) (Ana c)) (Ana d)
   Ana (f a b c)   = App (App (App (Typ (f A0 A1 A2)) (Ana a)) (Ana b)) (Ana c)
   Ana (f a b)     = App (App (Typ (f A0 A1)) (Ana a)) (Ana b)
   Ana (f a)       = App (Typ (f A0)) (Ana a)
   Ana a           = Typ a

data App f a
data Typ a

-- type family Analyse n t where
--    Analyse n (f a)       = App (Analyse (S n) f) a
--    Analyse n a           = Typ a
-- data S a
-- data Z
