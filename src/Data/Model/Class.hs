{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Model.Class(hType
                       ,hTypeEnv
                       ,Model(..)
                       ,AsType,Ana)
       where

import           Control.Monad
import           Control.Monad.Trans.State.Lazy
import           Data.Bifunctor
import           Data.Either
import           Data.List
import           Data.Model.Types
import           Data.Proxy
import           Data.Typeable
--import           Debug.Trace
import           GHC.Generics                   hiding (S)
import qualified GHC.Generics                   as G
import           Type.Analyse

-- hType :: Model a => Data.Proxy.Proxy a -> HType
hType :: AsType (Ana a) => Proxy a -> HType
hType = fst . hTypeEnv

-- | Return the model for the given type, that's the type plus all the data types referred to, directly or indirectly
-- hTypeEnv :: Model a => Proxy a -> (HType, HEnv)
hTypeEnv :: AsType (Ana a) => Proxy a -> (HType, [HADT])
hTypeEnv p =
  let (ht, e) = runState (asTypeP p) emptyEnv
  in (ht, map fst $ defs e) -- trace (showEnv e) ht

asTypeP :: forall a. AsType (Ana a) => Proxy a -> State Env HType
asTypeP _ = asType (undefined :: Ana a)

-- |Helper class used to capture the type parameters
class AsType a where
  asType :: a -> State Env HType

instance Model a => AsType (Typ a) where
  asType _ = envType (Proxy::Proxy a)

instance (AsType f,AsType a) => AsType (App f a) where
  asType _ = TypeApp <$> asType (undefined::f) <*> asType (undefined::a)

-- |TypeLits are used to represent data type's parameters.
instance (KnownNat t,Typeable t) => Model (ANat t) where
  envType _ = return . TypeCon . TypVar . fromIntegral . anatVal $ (undefined :: ANat t)

-- |Environment used while capturing model
-- TODO: remove seen and as not used
data Env = Env {seen::[QualName]          -- ^ Types seen already
               ,ctx::[(QualName,Int)]     -- ^ Context
               ,defs::[(HADT,[QualName])] -- ^ ADT models
               } deriving Show

emptyEnv :: Env
emptyEnv = Env [] [] []

-- |Enter a type
enterCtx :: QualName -> State Env Bool
enterCtx name = do
   f <- inCtx name
   modify (\e -> e {ctx = (name,length (seen e)) : ctx e,seen = name : seen e})
   return f

-- |Returns True if we have already seen this data type and do not need to analyse it further
inCtx :: QualName -> State Env Bool
inCtx name = do
   inContext <- (name `elem`) <$> gets (map fst . ctx)
   inDefinitions <- (name `elem`) <$> gets (map (declName . fst) . defs)
   return $ inContext || inDefinitions

-- |Add a new data type model to the environment
addDef :: HADT -> State Env ()
addDef adt = modify (\e -> let l = snd . head . ctx $ e
                               deps = take (length (seen e)-l-1) (seen e)
                           in e {defs = (adt,sort $ nub deps) : defs e})

-- |Leave current type
closeCtx :: State Env ()
closeCtx = modify (\e -> e {ctx = drop 1 (ctx e)})

-- pretty print environment
-- showEnv e = unlines [""
--                   ,show $ map locName $ seen e
--                   ,show $ map (first locName) $ ctx e
--                   ,show $ map (\(d,deps) -> (locName $ declName d,map locName deps)) $ defs e
--                   ]

-- | Class of types whose model can be calculated
-- Instances are derived automatically, provided that the data type has instances for Typeable and Generic
class (Typeable a,AsType (Ana a)) => Model a where
-- class (Typeable a) => Model a where
-- class Model a where

  -- |Given a type proxy, update the environment with the ADTs referred by it and return the corresponding HType
  envType :: Proxy a -> State Env HType

  -- |Default, Generics based implementation
  default envType :: (Generic a, GModel (Rep a)) => Proxy a -> State Env HType
  envType p = let tr = typeRep p
                  (tc,ts) = splitTyConApp tr
                  name = QualName (tyConPackage tc) (tyConModule tc) (tyConName tc)
              in do
                   inCtx <- enterCtx name
                   unless inCtx $ do
                        cs <- gcons (from (undefined :: a))
                        addDef $ ADT name (fromIntegral $ length ts) $ cs
                   closeCtx
                   return . TypeCon . TypRef $ name

-- |Helper class, uses Generics to capture the model of a data type
-- Adapted from the Beamable package
class GModel f where
    gcons :: f a -> State Env (Maybe (ConTree HTypeRef))
    gcontree :: f a -> State Env (ConTree HTypeRef)
    gtype :: f a -> State Env HType
    gtypeN :: f a -> State Env [Either HType (String,HType)]

instance GModel (M1 D d V1) where
    gcons _ = return Nothing

-- |Datatypes with single constructor only
instance (GModel a, Datatype d, Constructor c) => GModel (M1 D d (M1 C c a)) where
    gcons x = Just <$> gcontree (unM1 x)

-- | Needed to avoid overlapping instances with (M1 D d (M1 C c a))
instance (Datatype d, GModel a, GModel b) => GModel (M1 D d (a :+: b) ) where
    gcons x = Just <$> gcontree x
    gcontree x = ConTree <$> gcontree (unL . unM1 $ x) <*> gcontree (unR . unM1 $ x)

-- |Datatypes with multiple constructors
instance (GModel a, Constructor c) => GModel (M1 C c a) where
  gcontree x = Con (conName x) . toE . partitionEithers  <$> gtypeN (unM1 x)
    where
      toE (ls,[]) = Left ls
      toE ([],rs) = Right rs

-- |Constructors
instance (GModel a, GModel b) => GModel (a :+: b) where
    gcontree x = ConTree <$> gcontree (unL x) <*> gcontree (unR x)

instance (Selector c,GModel a) => GModel (M1 G.S c a) where
    gtypeN ~s@(M1 x) = (\t -> [let n = selName s
                               in if null n
                                  then Left t
                                  else Right (n,t)
                              ]) <$> gtype x

instance (GModel a, GModel b) => GModel (a :*: b) where
    gtypeN ~(x :*: y) = (++) <$> gtypeN x <*> gtypeN y

instance GModel U1 where
    gtypeN _ = return []

instance (AsType (Ana a),Model a) => GModel (K1 i a) where
    gtype x = asTypeP (undefined::Proxy a)

unL :: (l :+: r) a -> l a
unL = error "unL should be used only for type recovery operations"

unR :: (l :+: r) a -> r a
unR = error "unR should be used only for type recovery operations"
