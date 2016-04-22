{- |A model for simple algebraic data types.
-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Model.Types(
  ADT(..)
  ,ConTree(..),constructors,conTreeTypeMap,conTreeTypeList,conTreeTypeFoldMap
  ,Type(..),TypeN(..),typeN,typeA,TypeRef(..)
  ,QualName(..),qualName
  ,HADT,HType,HTypeRef,HEnv,fieldsTypes,fieldsNames
  ,module GHC.Generics,Proxy(..)) where

import           Control.DeepSeq
import           Data.Bifunctor  (second)
import           Data.Proxy
import           Data.Word       (Word8)
import           GHC.Generics

-- |Haskell ADT.
type HADT = ADT QualName HTypeRef

-- |Haskell Type
type HType = Type HTypeRef

-- |Reference to an Haskell type
type HTypeRef = TypeRef QualName

-- |An environment, a set of Haskell ADTs
type HEnv = [HADT]

-- |A fully qualified name
data QualName = QualName {pkgName,mdlName,locName :: String}
              deriving (Eq, Ord, Read, Show, NFData, Generic)

-- |Return the qualified name, minus the package name.
qualName :: QualName -> String
qualName n = mdlName n ++ "." ++ locName n

-- |Simple algebraic data type (not GADT), with a maximum of 255 type variables.
data ADT name ref =
       ADT
         { declName          :: name
         , declNumParameters :: Word8
         , declCons          :: Maybe (ConTree ref)
         }
       deriving (Eq, Ord, Read, Show, NFData, Generic, Functor, Foldable, Traversable)

-- | Constructors
data ConTree ref =
  Con {
  -- | The constructor name, unique in the data type
  constrName    :: String

  -- | Constructor fields, they can be either unnamed (Left case) or named (Right case)
  -- If they are named, they must all be named
  ,constrFields :: Either
                   [Type ref]
                   [(String,Type ref)]
  }

  {- |Constructor tree.
  Constructors are disposed in an optimally balanced, right heavier tree:

  For example, the data type:
  data N = One | Two | Three | Four | Five

  Would have its contructors ordered in the following tree:
          |
     |          |
  One Two  Three    |
                 Four Five

  To get a list of constructor in declaration order, use constructors
  -}
  | ConTree (ConTree ref) (ConTree ref)

  deriving (Eq, Ord, Read, Show, NFData, Generic)

constructors c@(Con _ _) = [c]
constructors (ConTree l r) = constructors l ++ constructors r

-- |Return just the field types
fieldsTypes :: Either [b] [(a, b)] -> [b]
fieldsTypes (Left ts)   = ts
fieldsTypes (Right nts) = map snd nts

-- |Return just the field names (or an empty list if unspecified)
fieldsNames (Left _)   = []
fieldsNames (Right nts) = map snd nts

-- GHC won't derive these instances automatically
instance Functor ConTree where
  fmap f (ConTree l r) = ConTree (fmap f l) (fmap f r)
  fmap f (Con n (Left ts)) = Con n (Left $ (fmap . fmap) f ts)
  fmap f (Con n (Right ts)) = Con n (Right $ (fmap . fmap . fmap) f ts)

instance Foldable ConTree where
   foldMap f (ConTree l r) = foldMap f l `mappend` foldMap f r
   foldMap f (Con _ (Left ts)) = mconcat . map (foldMap f) $ ts
   foldMap f (Con _ (Right nts)) = mconcat . map (foldMap f . snd) $ nts

instance Traversable ConTree where
  traverse f (ConTree l r) = ConTree <$> traverse f l <*> traverse f r
  traverse f (Con n (Left ts)) = Con n . Left <$> sequenceA (map (traverse f) ts)
  -- TODO: simplify this
  traverse f (Con n (Right nts)) = Con n . Right . zip (map fst nts) <$> sequenceA (map (traverse f . snd) nts)

-- |Map on the constructor types (used for example when eliminating variables)
conTreeTypeMap :: (Type t -> Type ref) -> ConTree t -> ConTree ref
conTreeTypeMap f (ConTree l r) = ConTree (conTreeTypeMap f l) (conTreeTypeMap f r)
conTreeTypeMap f (Con n (Left ts)) = Con n (Left $ map f ts)
conTreeTypeMap f (Con n (Right nts)) = Con n (Right $ map (second f) nts)

conTreeTypeList :: ConTree t -> [Type t]
conTreeTypeList = conTreeTypeFoldMap (:[])

conTreeTypeFoldMap :: Monoid a => (Type t -> a) -> ConTree t -> a
conTreeTypeFoldMap f (ConTree l r) = conTreeTypeFoldMap f l `mappend` conTreeTypeFoldMap f r
conTreeTypeFoldMap f (Con _ (Left ts)) = mconcat . map f $ ts
conTreeTypeFoldMap f (Con _ (Right nts)) = mconcat . map (f . snd) $ nts

-- |A type
data Type ref = TypeCon ref -- Type constructor ('Bool','Maybe')
              | TypeApp (Type ref) (Type ref)
  deriving (Eq, Ord, Read, Show, NFData, Generic, Functor, Foldable, Traversable)

-- |Another representation of type, sometime easier to work with
data TypeN r = TypeN r [TypeN r]
             deriving (Eq,Ord,Read,Show,NFData ,Generic,Functor,Foldable,Traversable)

-- |Convert from Type to TypeN
typeN :: Type r -> TypeN r
typeN (TypeApp f a) = let TypeN h ts = typeN f
                       in TypeN h (ts ++ [typeN a])
typeN (TypeCon r) = TypeN r []

--typeNN (TypeN n _) = n

-- |Convert from TypeN to Type
typeA :: TypeN ref -> Type ref
typeA (TypeN t ts) = app (TypeCon t) (map typeA ts)
  where app t [] = t
        app t (x:xs) = app (TypeApp t x) xs

-- |A reference to a type
data TypeRef name = TypVar Word8  -- Type variable
                  | TypRef name   -- Type reference
  deriving (Eq, Ord, Read, Show, NFData, Generic, Functor, Foldable, Traversable)
