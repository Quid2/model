-- |A model for simple algebraic data types.
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Model.Types(
  -- *Model
  TypeModel(..),TypeEnv,typeADTs
  ,ADT(..)
  ,ConTree(..),Fields
  ,Type(..),TypeN(..),nestedTypeNs,TypeRef(..)

  -- *Names
  ,Name(..),QualName(..),qualName

  -- *Model Utilities
  ,adtNamesMap
  ,typeN,typeA
  ,contree,constructors,constructorInfo,conTreeNameMap,conTreeNameFold,conTreeTypeMap,conTreeTypeList,conTreeTypeFoldMap,fieldsTypes,fieldsNames

  -- *Handy aliases
  ,HTypeEnv,HTypeModel,HADT,HType,HTypeRef

  -- *Utilities
  ,solve,solveAll,unVar,getHRef

  -- *Re-exports
  ,module GHC.Generics,Proxy(..)

  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Bifunctor         (first, second)
import           Data.Either.Validation
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Model.Util
import           Data.Proxy
import           Data.Word              (Word8)
import           GHC.Generics

-- |Haskell Environment
type HTypeEnv = TypeEnv String String (TypeRef QualName) QualName

-- |Haskell TypeModel
type HTypeModel = TypeModel String String (TypeRef QualName) QualName

-- |Haskell ADT
type HADT = ADT String String HTypeRef

-- |Haskell Type
type HType = Type HTypeRef

-- |Reference to an Haskell Type
type HTypeRef = TypeRef QualName

{- |
The complete model of a type, a reference to the type plus its environment:

* adtName:  type used to represent the name of a data type
* consName: type used to represent the name of a constructor
* inRef:    type used to represent a reference to a type or a type variable inside the data type definition (for example `HTypeRef`)
* exRef:    type used to represent a reference to a type in the type name (for example `QualName`)
-}
data TypeModel adtName consName inRef exRef = TypeModel {
  -- |The type application corresponding to the type
  typeName::Type exRef

  -- |The environment in which the type is defined
  ,typeEnv::TypeEnv adtName consName inRef exRef
  }
  deriving (Eq, Ord, Show, NFData, Generic)

-- |The ADTs defined in the TypeModel
typeADTs :: TypeModel adtName consName inRef k -> [ADT adtName consName inRef]
typeADTs = M.elems . typeEnv

-- |A map of all the ADTs that are directly or indirectly referred by a type, indexed by a type reference
type TypeEnv adtName consName inRef exRef = M.Map exRef (ADT adtName consName inRef)

{- |
Simple algebraic data type (not a GADT):

* declName: type used to represent the name of the data type
* consName: type used to represent the name of a constructor
* ref:      type used to represent a reference to a type or a type variable inside the data type definition (for example `HTypeRef`)
-}
data ADT name consName ref =
       ADT
         { declName          :: name   -- ^The name of the data type (for example @Bool@ for @data Bool@)
         , declNumParameters :: Word8  -- ^The number of type parameters/variable (up to a maximum of 255)
         , declCons          :: Maybe (ConTree consName ref) -- ^The constructors, if present
         }
       deriving (Eq, Ord, Show, NFData, Generic, Functor, Foldable, Traversable)

-- |Constructors are assembled in a binary tree
data ConTree name ref =
  Con {
  -- | The constructor name, unique in the data type
  constrName    :: name

  -- | Constructor fields, they can be either unnamed (Left case) or named (Right case)
  -- If they are named, they must all be named
  ,constrFields :: Fields name ref
  }

  {- |
  Constructor tree.

  Constructors are disposed in an optimally balanced, right heavier tree:

  For example, the data type:

  @data N = One | Two | Three | Four | Five@

  Would have its contructors ordered in the following tree:

>          |
>     |            |
>  One Two   Three   |
>                Four Five

  To get a list of constructor in declaration order, use `constructors`
  -}
  | ConTree (ConTree name ref) (ConTree name ref)

  deriving (Eq, Ord, Show, NFData, Generic)

type Fields name ref = Either
                   [Type ref]
                   [(name,Type ref)]

-- |Return the list of constructors in definition order
constructors :: ConTree name ref -> [(name, Fields name ref)]
constructors (Con n f)     = [(n,f)]
constructors (ConTree l r) = constructors l ++ constructors r

-- |Convert a (possibly empty) list of constructors in (maybe) a ConTree
contree :: [(name, Fields name ref)] -> Maybe (ConTree name ref)
contree [] = Nothing
contree ccs = Just . ct $ ccs
  where
    ct [(name,fields)] = Con name fields
    ct cs = let (ls,rs) = splitAt (length cs `div` 2) cs in ConTree (ct ls) (ct rs)

-- |Return just the field types
fieldsTypes :: Either [b] [(a, b)] -> [b]
fieldsTypes (Left ts)   = ts
fieldsTypes (Right nts) = map snd nts

-- |Return just the field names (or an empty list if unspecified)
fieldsNames :: Either t [(a, t1)] -> [t1]
fieldsNames (Left _)    = []
fieldsNames (Right nts) = map snd nts

-- |Return the binary encoding and parameter types of a constructor
--
-- The binary encoding is the sequence of Left (False) and Right (True) turns
-- needed to reach the constructor from the constructor tree root
constructorInfo :: Eq consName => consName -> ConTree consName t -> Maybe ([Bool], [Type t])
constructorInfo consName = (first reverse <$>) . loc []
  where
    -- |Locate constructor in tree
    loc bs (Con n ps) | n == consName = Just (bs,fieldsTypes ps)
                      | otherwise = Nothing
    loc bs (ConTree l r) = loc (False:bs) l <|> loc (True:bs) r

-- GHC won't derive these instances automatically
instance Functor (ConTree name) where
  fmap f (ConTree l r)      = ConTree (fmap f l) (fmap f r)
  fmap f (Con n (Left ts))  = Con n (Left $ (fmap . fmap) f ts)
  fmap f (Con n (Right ts)) = Con n (Right $ (fmap . fmap . fmap) f ts)

instance Foldable (ConTree name) where
   foldMap f (ConTree l r)       = foldMap f l `mappend` foldMap f r
   foldMap f (Con _ (Left ts))   = mconcat . map (foldMap f) $ ts
   foldMap f (Con _ (Right nts)) = mconcat . map (foldMap f . snd) $ nts

instance Traversable (ConTree name) where
  traverse f (ConTree l r) = ConTree <$> traverse f l <*> traverse f r
  traverse f (Con n (Left ts)) = Con n . Left <$> sequenceA (map (traverse f) ts)
  -- TODO: simplify this
  traverse f (Con n (Right nts)) = Con n . Right . zip (map fst nts) <$> sequenceA (map (traverse f . snd) nts)

-- |Map on the constructor types (used for example when eliminating variables)
conTreeTypeMap :: (Type t -> Type ref) -> ConTree name t -> ConTree name ref
conTreeTypeMap f (ConTree l r) = ConTree (conTreeTypeMap f l) (conTreeTypeMap f r)
conTreeTypeMap f (Con n (Left ts)) = Con n (Left $ map f ts)
conTreeTypeMap f (Con n (Right nts)) = Con n (Right $ map (second f) nts)

-- |Map over a constructor tree names
conTreeNameMap :: (name -> name2) -> ConTree name t -> ConTree name2 t
conTreeNameMap f (ConTree l r) = ConTree (conTreeNameMap f l) (conTreeNameMap f r)
conTreeNameMap f (Con n (Left ts)) = Con (f n) (Left ts)
conTreeNameMap f (Con n (Right nts)) = Con (f n) (Right $ map (first f) nts)

-- |Fold over a constructor tree names
conTreeNameFold :: Monoid a => (name -> a) -> ConTree name t -> a
conTreeNameFold f (ConTree l r) = conTreeNameFold f l `mappend` conTreeNameFold f r
conTreeNameFold f (Con n _) = f n

-- |Extract list of types in a constructor tree
conTreeTypeList :: ConTree name t -> [Type t]
conTreeTypeList = conTreeTypeFoldMap (:[])

-- |Fold over the types in a constructor tree
conTreeTypeFoldMap :: Monoid a => (Type t -> a) -> ConTree name t -> a
conTreeTypeFoldMap f (ConTree l r) = conTreeTypeFoldMap f l `mappend` conTreeTypeFoldMap f r
conTreeTypeFoldMap f (Con _ (Left ts)) = mconcat . map f $ ts
conTreeTypeFoldMap f (Con _ (Right nts)) = mconcat . map (f . snd) $ nts

-- |Map over the names of an ADT and of its constructors
adtNamesMap
  :: (adtName1 -> adtName2)
     -> (consName1 -> consName2)
     -> ADT adtName1 consName1 ref
     -> ADT adtName2 consName2 ref
adtNamesMap f g adt = adt {declName = f (declName adt),declCons = conTreeNameMap g <$> declCons adt}

-- |A type
data Type ref = TypeCon ref                    -- ^Type constructor ("Bool","Maybe",..)
              | TypeApp (Type ref) (Type ref)  -- ^Type application
  deriving (Eq, Ord, Show, NFData, Generic, Functor, Foldable, Traversable)

-- |Another representation of a type, sometime easier to work with
data TypeN r = TypeN r [TypeN r]
              deriving (Eq,Ord,Read,Show,NFData ,Generic,Functor,Foldable,Traversable)

-- |Convert from Type to TypeN
typeN :: Type r -> TypeN r
typeN (TypeApp f a) = let TypeN h ts = typeN f
                       in TypeN h (ts ++ [typeN a])
typeN (TypeCon r) = TypeN r []

-- |Convert from TypeN to Type
typeA :: TypeN ref -> Type ref
typeA (TypeN tf ts) = foldl TypeApp (TypeCon tf) (map typeA ts)

-- |Returns the list of nested TypeNs
--
-- >>> nestedTypeNs $ TypeN "F" [TypeN "G" [],TypeN "Z" []]
-- [TypeN "F" [TypeN "G" [],TypeN "Z" []],TypeN "G" [],TypeN "Z" []]
--
-- >>> nestedTypeNs $ TypeN "F" [TypeN "G" [TypeN "H" [TypeN "L" []]],TypeN "Z" []]
-- [TypeN "F" [TypeN "G" [TypeN "H" [TypeN "L" []]],TypeN "Z" []],TypeN "G" [TypeN "H" [TypeN "L" []]],TypeN "H" [TypeN "L" []],TypeN "L" [],TypeN "Z" []]
--
nestedTypeNs :: TypeN t -> [TypeN t]
nestedTypeNs t@(TypeN _ []) = [t]
nestedTypeNs t@(TypeN _ ps) = t : concatMap nestedTypeNs ps

-- |A reference to a type
data TypeRef name = TypVar Word8  -- ^Type variable
                  | TypRef name   -- ^Type reference
  deriving (Eq, Ord, Show, NFData, Generic, Functor, Foldable, Traversable)

-- |Remove variable references (for example if we know that a type is fully saturated and cannot contain variables)
unVar :: TypeRef t -> t
unVar (TypVar _) = error "Unexpected variable"
unVar (TypRef n) = n

-- |Extract reference
getHRef :: TypeRef a -> Maybe a
getHRef (TypRef r) = Just r
getHRef (TypVar _) = Nothing

-- |A fully qualified Haskell name
data QualName = QualName {pkgName,mdlName,locName :: String}
              deriving (Eq, Ord, Show, NFData, Generic)

{-|Return the qualified name, minus the package name.

>>> qualName (QualName {pkgName = "ab", mdlName = "cd.ef", locName = "gh"})
"cd.ef.gh"
-}
qualName :: QualName -> String
qualName n = convert $ n {pkgName=""}

instance Convertible String QualName where safeConvert = errorsToConvertResult (validationToEither . asQualName)

instance Convertible QualName String  where safeConvert n = Right $ dotted [pkgName n,mdlName n,locName n]

{-|Convert a String to a `QualName`, if possible

>>> asQualName "ab.cd.ef.gh"
Success (QualName {pkgName = "ab", mdlName = "cd.ef", locName = "gh"})

>>> asQualName "ab.cd.ef"
Success (QualName {pkgName = "ab", mdlName = "cd", locName = "ef"})

>>> asQualName "ab.cd"
Success (QualName {pkgName = "", mdlName = "ab", locName = "cd"})

>>> asQualName "ab"
Success (QualName {pkgName = "", mdlName = "", locName = "ab"})

>>> asQualName ""
Failure ["Empty qualified name"]

>>> asQualName "."
Failure ["Empty qualified name"]

The conversion assumes that the input String is a well-formed Haskell fully qualified name.

It will produce funny results if this is not the case:

>>> asQualName "**.&&.!!"
Success (QualName {pkgName = "**", mdlName = "&&", locName = "!!"})

-}
asQualName :: String -> Validation Errors QualName
asQualName =
  (\n ->
     if nullQualName n
       then Failure ["Empty qualified name"]
       else Success n) .
  asQualName_
  where
    nullQualName n = pkgName n == "" && mdlName n == "" && locName n == ""
    asQualName_ n =
      let (p, r) = span (/= '.') n
      in if null r
           then QualName "" "" p
           else let (l, r2) = span (/= '.') $ reverse $ tail r
                in if null r2
                     then QualName "" p (reverse l)
                     else let m = reverse $ tail r2
                          in QualName p m (reverse l)

-- |Simple name
data Name = Name String deriving (Eq, Ord, Show, NFData, Generic)

-- Utilities
-- |Solve all references in a data structure, using the given environment
solveAll :: (Functor f, Show k, Ord k) => M.Map k b -> f k -> f b
solveAll env t = (`solve` env) <$> t

-- |Solve a key in an environment, returns an error if the key is missing
solve :: (Ord k, Show k) => k -> M.Map k a -> a
solve k e = fromMaybe (error $ unwords ["solve:Unknown reference to",show k,"in",show $ M.keys e]) (M.lookup k e)

