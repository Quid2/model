{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
-- |Pretty instances for the model types
module Data.Model.Pretty(
  -- *Utilities
  dotted,spacedP,vspacedP,varP
  -- *Re-exports
  ,Pretty(..),prettyShow
  ) where

import           Data.Char
import           Data.List
import qualified Data.ListLike.String           as S
import qualified Data.Map                       as M
import           Data.Model.Types
import           Text.PrettyPrint.HughesPJClass

instance( Pretty exRef,Ord exRef,Show exRef,S.StringLike adtName,S.StringLike consName,S.StringLike ref) => Pretty (TypeModel adtName consName (TypeRef ref) exRef) where
  pPrint (TypeModel t e) = vcat $ [
     text "Type:"
    ,pPrint t <+> text "->" <+> pPrint (localName . declName <$> solveAll e t)
    ,text ""
    ,text "Environment:"]
     ++ map (\(ref,adt) -> pPrint ref <+> text "->" <+> pPrint (stringADT e adt)) (M.assocs e)

stringADT env adt = ADT (localName . declName $ adt) (declNumParameters adt) (((localName <$>) <$>) . conTreeNameMap localName <$> declCons adt)

localName = Name . S.toString

instance (Pretty n,Pretty cn,Pretty r) => Pretty (ADT n cn r) where pPrint = prettyADT "" '≡'

prettyADT pre eq adt = text pre <+> pPrint (declName adt) <+> vars adt <+> maybe (text "") (\c -> char eq <+> pPrint c) (declCons adt)

vars adt = sep . map varP . map (\x -> x-1) $ [1 .. declNumParameters adt]

varP n = char $ chr ( (ord 'a') + (fromIntegral n))

instance (Pretty name,Pretty ref) => Pretty (ConTree name ref) where
  pPrint (Con n (Left fs)) = pPrint n <+> sep (map (printPrettyType True) fs)
  pPrint (Con n (Right nfs)) = pPrint n <+> "{" <> sep (punctuate "," (map (\(n,t) -> pPrint n <+> "::" <+> pPrint t) nfs)) <> "}"
  -- pPrint (ConTree l r) = pPrint l <+> char '|' <+> pPrint r
  pPrint tr@(ConTree l r) = let (h:t) = constructors tr
                            in vcat (char ' ' <+> pPrint h : map (\c -> (char '|') <+> pPrint c) t)

instance Pretty n => Pretty (TypeRef n) where
   pPrint (TypVar v)   = varP v
   pPrint (TypRef s)   = pPrint s

instance Pretty r => Pretty (Type r) where pPrint = printPrettyType False

data PrettyType r = PrettyType Bool (TypeN r)
printPrettyType n = pPrint . PrettyType n . typeN

instance Pretty r => Pretty (PrettyType r) where
  pPrint (PrettyType _ (TypeN f [])) = pPrint f
  pPrint (PrettyType n (TypeN f as)) = maybeParens n (pPrint f <+> spacedP (map (PrettyType True) as))

instance Pretty r => Pretty (TypeN r) where
  pPrint (TypeN f []) = pPrint f
  pPrint (TypeN f as) = parens (pPrint f <+> spacedP as)

instance Pretty QualName where pPrint (QualName p m l) = dotted [p,m,l]

instance Pretty Name where pPrint (Name n) = text n

instance Pretty Doc where pPrint d = d

-- |Separate with a space
spacedP :: Pretty a => [a] -> Doc
spacedP = sep . map pPrint

-- |Separate with a new line
vspacedP :: Pretty a => [a] -> Doc
vspacedP = sep . intersperse (text "") . map pPrint

-- |Intercalate with '.'
dotted :: [String] -> Doc
dotted = text . intercalate "."
