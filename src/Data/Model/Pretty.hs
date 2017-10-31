{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
-- |Pretty instances for the model types
module Data.Model.Pretty(
  CompactPretty(..)
  -- *Utilities
  ,dottedP,spacedP,vspacedP,varP,varC
  -- *Re-exports
  ,Pretty(..),prettyShow
  ) where

import           Data.Char
import           Data.List
import qualified Data.Map                       as M
import           Data.Model.Types
import           Data.Model.Util
import           Text.PrettyPrint.HughesPJClass

-- |Compact representation: a value enveloped in CompactPretty will have only its first lines displayed
data CompactPretty a = CompactPretty a

instance Pretty a => Pretty (CompactPretty a) where pPrint (CompactPretty a) = text . shorter . prettyShow $ a

shorter :: String -> String
shorter s =
   let ln = lines s
       l = length ln
   in if l > 11
      then unlines $ take 5 ln ++ ["..."]  ++ drop (l-5) ln
      else s

instance {-# OVERLAPPABLE #-} (Functor t, Pretty (t Name),Pretty exRef,Ord exRef,Show exRef,Convertible adtName String,Convertible consName String,Convertible iref String) => Pretty (TypeModel adtName consName (t iref) exRef) where
  pPrint (TypeModel t e) = vcat $ [
     text "Type:"
    ,pPrint t <+> text "->" <+> pPrint (localName . declName <$> solveAll e t)
    ,text "Environment:"]
     ++ map (\(ref,adt) -> pPrint ref <+> text "->" <+> (pPrint . CompactPretty . stringADT $ adt)) (M.assocs e)
    where
      stringADT adt = ADT (localName . declName $ adt) (declNumParameters adt) (((localName <$>) <$>) . conTreeNameMap localName <$> declCons adt)

localName :: Convertible s String => s -> Name
localName = Name . convert

instance (Pretty n,Pretty cn,Pretty r) => Pretty (ADT n cn r) where pPrint = prettyADT "" '≡'

prettyADT :: (Pretty name, Pretty consName, Pretty ref) => String -> Char -> ADT name consName ref -> Doc
prettyADT pre eq adt = text pre <+> pPrint (declName adt) <+> vars adt <+> maybe (text "") (\c -> char eq <+> pPrint c) (declCons adt)

vars :: ADT name consName ref -> Doc
vars adt = sep . map varP . map (\x -> x-1) $ [1 .. declNumParameters adt]

-- |Convert a variable number (0,1,..) to a name ('a','b',..)
varP :: Integral n => n -> Doc
varP = char . varC

-- |Convert a variable number (0,1,..) to a name ('a','b',..)
varC :: Integral a => a -> Char
varC n = chr ( (ord 'a') + (fromIntegral n))

instance (Pretty name,Pretty ref) => Pretty (ConTree name ref) where
  pPrint conTree = let (h:t) = constructors conTree
                   in vcat (char ' ' <+> pPrint h : map (\c -> char '|' <+> pPrint c) t)

instance {-# OVERLAPS #-} (Pretty name,Pretty ref) => Pretty (name,Fields name ref) where
  pPrint (name,fields) = pPrint name <+> pPrint fields

instance {-# OVERLAPS #-} (Pretty name,Pretty ref) => Pretty (Fields name ref) where
  pPrint (Left fs) = sep (map (printPrettyType True) fs)
  pPrint (Right nfs) = "{" <> sep (punctuate "," (map (\(nm,t) -> pPrint nm <+> "::" <+> pPrint t) nfs)) <> "}"

instance Pretty n => Pretty (TypeRef n) where
   pPrint (TypVar v) = varP v
   pPrint (TypRef s) = pPrint s

instance Pretty r => Pretty (Type r) where pPrint = printPrettyType False

instance Pretty r => Pretty (TypeN r) where
  pPrint (TypeN f []) = pPrint f
  pPrint (TypeN f as) = parens (pPrint f <+> spacedP as)

instance Pretty QualName where pPrint (QualName p m l) = dottedP [p,m,l]

instance Pretty Name where pPrint (Name n) = text n

instance Pretty Doc where pPrint d = d

data PrettyType r = PrettyType Bool (TypeN r)

printPrettyType :: Pretty r => Bool -> Type r -> Doc
printPrettyType n = pPrint . PrettyType n . typeN

instance Pretty r => Pretty (PrettyType r) where
  pPrint (PrettyType _ (TypeN f [])) = pPrint f
  pPrint (PrettyType n (TypeN f as)) = maybeParens n (pPrint f <+> spacedP (map (PrettyType True) as))

-- |Intercalate with a space
spacedP :: Pretty a => [a] -> Doc
spacedP = sep . map pPrint

-- |Intercalate with a new line
vspacedP :: Pretty a => [a] -> Doc
vspacedP = sep . intersperse (text "") . map pPrint

-- |Intercalate a dot between the non empty elements of a list of strings
dottedP :: [String] -> Doc
dottedP = text . dotted
