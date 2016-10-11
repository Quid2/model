{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Model.Pretty(
  Pretty(..)
  ,module Text.PrettyPrint.HughesPJClass
  ,txt,dotted,spacedP,vspacedP,varP
  ,CompactPretty(..)
  ,prettyADT
  ) where

import           Data.Char
import           Data.List
import           Data.Model.Types
import qualified Data.Text       as T
import           Text.PrettyPrint.HughesPJClass
import Data.Foldable

-- t = renderStyle (style {mode=PageMode}) . pPrint $ "data Bool =" <> nest 5 (vcat ["False","True"])

-- Compact representation 
data CompactPretty a = CompactPretty a

instance Pretty a => Pretty (CompactPretty a) where pPrint (CompactPretty a) = text . shorter . prettyShow $ a

-- which is faster?, see also canonical and haskell code generation
-- NOTE: similar code in top-apps-ghcjs
shorter :: String -> String
shorter s =
   let ln = lines s
       l = length ln
   in if l > 11
      then unlines $ take 5 ln ++ ["..."]  ++ drop (l-5) ln
      else s

-- instance (Pretty n,Pretty r) => Pretty (ADT n r) where pPrint = prettyADT "data" '='
instance (Pretty n,Pretty cn,Pretty r) => Pretty (ADT n cn r) where pPrint = prettyADT "" '≡'

prettyADT pre eq adt = text pre <+> pPrint (declName adt) <+> vars adt <+> maybe (text "") (\c -> char eq <+> pPrint c) (declCons adt)

vars adt = spaced . map varP . map (\x -> x-1) $ [1 .. declNumParameters adt]
varP n = char $ chr ( (ord 'a') + (fromIntegral n))

instance (Pretty name,Pretty ref) => Pretty (ConTree name ref) where
  pPrint (Con n (Left fs)) = pPrint n <+> spaced (map (printPrettyType True) fs)
  pPrint (Con n (Right nfs)) = pPrint n <+> "{" <> sep (punctuate "," (map (\(n,t) -> pPrint n <+> "::" <+> pPrint t) nfs)) <> "}"
  -- pPrint (ConTree l r) = pPrint l <+> char '|' <+> pPrint r
  pPrint tr@(ConTree l r) = let (h:t) = constructors tr
                            in vcat (char ' ' <+> pPrint h : map (\c -> (char '|') <+> pPrint c) t)

instance Pretty r => Pretty (Type r) where pPrint = printPrettyType False

data PrettyType r = PrettyType Bool (TypeN r)
printPrettyType n = pPrint . PrettyType n . typeN

instance Pretty r => Pretty (PrettyType r) where
  pPrint (PrettyType _ (TypeN f [])) = pPrint f
  pPrint (PrettyType n (TypeN f as)) = maybeParens n (pPrint f <+> spacedP (map (PrettyType True) as))

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

vspacedP :: Pretty a => [a] -> Doc
vspacedP = spaced . intersperse (text "") . map pPrint

spaced :: [Doc] -> Doc
spaced = sep

dotted :: [Name] -> Doc
-- dotted = txt . T.intercalate (T.pack ".")
dotted = txt . intercalate "."

txt :: Name -> Doc
txt = text . strName
