{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Model.Pretty(
  Pretty(..),module Text.PrettyPrint.HughesPJClass,dotted,spacedP,varP
  ) where

import           Data.Char
import           Data.List
import           Data.Model.Types
import           Text.PrettyPrint.HughesPJClass

instance (Pretty n,Pretty r) => Pretty (ADT n r) where
  pPrint adt = text "data" <+> pPrint (declName adt) <+> vars adt <+> maybe (text "") (\c -> char '=' <+> pPrint c) (declCons adt)

vars adt = spaced . map varP . map (\x -> x-1) $ [1 .. declNumParameters adt]
varP n = char $ chr ( (ord 'a') + (fromIntegral n))

instance Pretty n => Pretty (ConTree n) where
  pPrint (Con n (Left fs)) = text n <+> spacedP fs
  pPrint (Con n (Right nfs)) = text n <+> "{" <+> sep (punctuate "," (map (\(n,t) -> text n <+> "::" <+> pPrint t) nfs)) <+> "}"
  pPrint (ConTree l r) = pPrint l <+> char '|' <+> pPrint r

instance Pretty r => Pretty (Type r) where
  pPrint = pPrint . typeN

instance Pretty r => Pretty (TypeN r) where
  pPrint (TypeN f []) = pPrint f
  pPrint (TypeN f as) = parens (pPrint f <+> spacedP as)

instance Pretty n => Pretty (TypeRef n) where
  pPrint (TypVar v)   = varP v
  pPrint (TypRef s)   = pPrint s

instance Pretty QualName where pPrint (QualName p m l) = dotted [p,m,l]

instance Pretty Doc where pPrint d = d

spacedP :: Pretty a => [a] -> Doc
spacedP = spaced . map pPrint
spaced = sep

dotted :: [String] -> Doc
dotted = text . intercalate "."
