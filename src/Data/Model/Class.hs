{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Model.Class(
  typeModel
  ,Model(..)
  ,AsType(..)

  -- *Utilities
  ,useCT

  -- *Re-exports
  ,Ana
  --,Typ
  )
  where

import           Control.Monad
import           Control.Monad.Trans.State.Lazy
import           Data.Either
import           Data.Model.Env
import           Data.Model.Types
import           Data.Typeable
import qualified GHC.Generics                   as G
import           Type.Analyse

-- | Return the model for the given type
typeModel :: AsType (Ana a) => Proxy a -> HTypeModel
typeModel p = withEnv (asTypeP p)

-- We feed to the analyser the abstract version of the type
-- in order to distinguish between variable and non variable positions
asTypeP :: forall a. AsType (Ana a) => Proxy a -> State Env HType
asTypeP _ = asType (undefined :: Ana a)

-- |Helper class used to capture the type parameters
class AsType a where
  asType :: a -> State Env HType

instance {-# OVERLAPPABLE #-} Model a => AsType (Typ a) where asType _ = envType (Proxy::Proxy a)

instance (AsType f,AsType a) => AsType (App f a) where
  asType _ = TypeApp <$> asType (undefined::f) <*> asType (undefined::a)

--instance (KnownNat t,Typeable t) => AsType (Typ (ANat t)) where asType _ = envType (Proxy::Proxy a)

-- |TypeLits are used to represent data type's parameters.
instance (KnownNat t,Typeable t) => Model (ANat t) where
  envType _ = return . TypeCon . TypVar . fromIntegral . anatVal $ (undefined :: ANat t)


-- | Class of types whose model can be calculated
-- Instances are derived automatically, provided that the data type has an instance for `GHC.Generics`
class (Typeable a,AsType (Ana a)) => Model a where
-- class (Typeable a) => Model a where
-- class Model a where

  -- |Given a type proxy, update the environment with the ADTs referred by it and return the corresponding `HType`
  envType :: Proxy a -> State Env HType

  -- |Default, Generics based implementation
  default envType :: (Generic a, GModel (Rep a)) => Proxy a -> State Env HType
  envType p = addCT_ False p $ gcons (from (undefined :: a))

-- |Use the given constructors tree as model for the given type, returns the build type
--
-- Exported so that it can be used to overwrite default definitions
useCT :: Typeable a => Maybe (ConTree String HTypeRef) -> proxy a -> State Env (Type (TypeRef QualName))
useCT ct p = addCT_ True p (return ct)

addCT_ useLocalString p mct =
  let tr = typeRep p
      (tc,ts) = splitTyConApp tr
      nm hname = if useLocalString then "" else hname
      -- nm hname = hname
      uname = tyConName tc
      qname = QualName (nm $ tyConPackage tc) (nm $ tyConModule tc) uname
  in do
    inCtx <- enterCtx qname
    unless inCtx $ do
      ct <- mct
      addDef qname $ ADT uname (fromIntegral $ length ts) $ ct
    closeCtx
    return . TypeCon . TypRef $ qname

-- |Helper class, uses Generics to capture the model of a data type
-- Adapted from the Beamable package
class GModel f where
    gcons :: f a -> State Env (Maybe (ConTree String HTypeRef))
    gcontree :: f a -> State Env (ConTree String HTypeRef)
    gtype :: f a -> State Env HType
    gtypeN :: f a -> State Env [Either HType (String,HType)]

instance GModel (M1 D d V1) where
    gcons _ = return Nothing
    gcontree = notThere
    gtype = notThere
    gtypeN = notThere

-- |Datatypes with single constructor only
instance (GModel a, Datatype d, Constructor c) => GModel (M1 D d (M1 C c a)) where
    gcons x = Just <$> gcontree (unM1 x)
    gcontree = notThere
    gtype = notThere
    gtypeN = notThere

-- | Needed to avoid overlapping instances with (M1 D d (M1 C c a))
instance (Datatype d, GModel a, GModel b) => GModel (M1 D d (a :+: b) ) where
    gcons x = Just <$> gcontree x
    gcontree x = ConTree <$> gcontree (unL . unM1 $ x) <*> gcontree (unR . unM1 $ x)
    gtype = notThere
    gtypeN = notThere

-- |Datatypes with multiple constructors
instance (GModel a, Constructor c) => GModel (M1 C c a) where
  gcons = notThere

  gcontree x = Con (conName x) . toE . partitionEithers  <$> gtypeN (unM1 x)
    where
      toE (ls,[]) = Left ls
      toE ([],rs) = Right rs

  gtype = notThere
  gtypeN = notThere

-- |Constructors
instance (GModel a, GModel b) => GModel (a :+: b) where
  gcons = notThere
  gcontree x = ConTree <$> gcontree (unL x) <*> gcontree (unR x)
  gtype = notThere
  gtypeN = notThere

instance (Selector c,GModel a) => GModel (M1 G.S c a) where
  gcons = notThere
  gcontree = notThere
  gtype = notThere
  gtypeN ~s@(M1 x) = (\t -> [let n = selName s
                               in if null n
                                  then Left t
                                  else Right (n,t)
                              ]) <$> gtype x

instance (GModel a, GModel b) => GModel (a :*: b) where
  gcons = notThere
  gcontree = notThere
  gtype = notThere
  gtypeN ~(x :*: y) = (++) <$> gtypeN x <*> gtypeN y

instance GModel U1 where
  gcons = notThere
  gcontree = notThere
  gtype = notThere
  gtypeN _ = return []

instance (AsType (Ana a),Model a) => GModel (K1 i a) where
  gcons = notThere
  gcontree = notThere
  gtype _ = asTypeP (undefined::Proxy a)
  gtypeN _ = return []

unL :: (l :+: r) a -> l a
unL = error "unL should be used only for type recovery operations"

unR :: (l :+: r) a -> r a
unR = error "unR should be used only for type recovery operations"

notThere = error "never called"
