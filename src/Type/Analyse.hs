{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Utility to abstract parametric types
module Type.Analyse(Ana,App,Typ,module Type.ANat) where

import Type.ANat

{- |
Abstract a concrete type to a type applied to variables.

More precisely: to a meta-representation where type application is represented by `App`, data types are marked by `Typ` and variables are represented by `ANat` types.

BUG: Silently fails for types with more than 9 parameters (should be defined recursively, if you know how let me know)

Examples:

>> undefined :: Ana (Maybe Char)
undefined :: Ana (Maybe Char) :: App (Typ (Maybe A0)) (Typ Char)

>> undefined :: Ana (Either Int Char)
undefined :: Ana (Either Int Char)
  :: App (App (Typ (Either A0 A1)) (Typ Int)) (Typ Char)

>> undefined :: Ana ([(Bool,())])
undefined :: Ana ([(Bool,())])
  :: App (Typ [A0]) (App (App (Typ (A0, A1)) (Typ Bool)) (Typ ()))
-}
type family Ana t where
    Ana (f a0 a1 a2 a3 a4 a5 a6 a7 a8) = App (App (App (App (App (App (App (App (App (Typ (f A0 A1 A2 A3 A4 A5 A6 A7 A8 )) (Ana a0)) (Ana a1)) (Ana a2)) (Ana a3)) (Ana a4)) (Ana a5)) (Ana a6)) (Ana a7)) (Ana a8)
    Ana (f a0 a1 a2 a3 a4 a5 a6 a7) = App (App (App (App (App (App (App (App (Typ (f A0 A1 A2 A3 A4 A5 A6 A7 )) (Ana a0)) (Ana a1)) (Ana a2)) (Ana a3)) (Ana a4)) (Ana a5)) (Ana a6)) (Ana a7)
    Ana (f a0 a1 a2 a3 a4 a5 a6) = App (App (App (App (App (App (App (Typ (f A0 A1 A2 A3 A4 A5 A6 )) (Ana a0)) (Ana a1)) (Ana a2)) (Ana a3)) (Ana a4)) (Ana a5)) (Ana a6)
    Ana (f a0 a1 a2 a3 a4 a5) = App (App (App (App (App (App (Typ (f A0 A1 A2 A3 A4 A5 )) (Ana a0)) (Ana a1)) (Ana a2)) (Ana a3)) (Ana a4)) (Ana a5)
    Ana (f a0 a1 a2 a3 a4) = App (App (App (App (App (Typ (f A0 A1 A2 A3 A4 )) (Ana a0)) (Ana a1)) (Ana a2)) (Ana a3)) (Ana a4)
    Ana (f a0 a1 a2 a3) = App (App (App (App (Typ (f A0 A1 A2 A3 )) (Ana a0)) (Ana a1)) (Ana a2)) (Ana a3)
    Ana (f a0 a1 a2) = App (App (App (Typ (f A0 A1 A2 )) (Ana a0)) (Ana a1)) (Ana a2)
    Ana (f a0 a1) = App (App (Typ (f A0 A1 )) (Ana a0)) (Ana a1)
    Ana (f a0) = App (Typ (f A0 )) (Ana a0)
    Ana a = Typ a

-- Problem: in Ana (Either Char Int) -> Ana (f a) f==Either Char a=Int

-- |Type application
data App f a

-- |A data type
data Typ a


