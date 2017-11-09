{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Model.Util
  ( -- * Dependencies
    properMutualGroups
  , mutualGroups
  , transitiveClosure
  -- * Error utilities
  , Errors
  , toErrors
  , noErrors
  , errsInContext
  , inContext
  , errorToConvertResult
  , errorsToConvertResult
  , convertResultToError
  , convertResultToErrors
  , convertOrError
  -- * Convertible re-exports
  , Convertible(..)
  , convert
  , ConvertResult
  , ConvertError(..)
  -- * Formatting utilities
  , dotted
  ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Convertible
import           Data.Foldable                  (toList)
import           Data.List
import qualified Data.Map.Lazy                  as M
import           Data.Maybe
import           Data.Typeable
import           Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)

{-| Return the groups of mutually dependent entities, with more than one component

>>> properMutualGroups Just (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
Right [["b","a"]]

-}
properMutualGroups :: (Ord r, Pretty r, Foldable t) => (a -> Maybe r) -> M.Map r (t a) -> Either [String] [[r]]
properMutualGroups getRef env = filter ((> 1) . length) <$> mutualGroups getRef env

{-| Return the groups of mutually dependent entities

>>> mutualGroups Just (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
Right [["c"],["b","a"]]

-}
mutualGroups :: (Ord r, Pretty r, Foldable t) => (a -> Maybe r) -> M.Map r (t a) -> Either [String] [[r]]
mutualGroups getRef env = recs [] (M.keys env)
  where
    deps = transitiveClosure getRef env
    recs gs [] = return gs
    recs gs (n:ns) = do
      ds <- deps n
      mutual <- filterM (((n `elem`) <$>) . deps) ds
      recs (mutual:gs) (ns \\ mutual)

{-| Return the transitive closure of an element in a graph of dependencies specified as an adjacency list

>>> transitiveClosure Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "b"
Right ["c","a","d","b"]

>>> transitiveClosure Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "c"
Right ["c"]

-}
transitiveClosure :: (Foldable t, Pretty r, Ord r) => (a -> Maybe r) -> M.Map r (t a) -> r -> Either [String] [r]
transitiveClosure getRef env = execRec . deps
    where
      deps n = do
         present <- (n `elem`) <$> gets seen
         unless present $ do
           modify (\st -> st {seen=n:seen st})
           case M.lookup n env of
             Nothing -> modify (\st -> st {errors=unwords ["transitiveClosure:Unknown reference to",prettyShow n]:errors st})
             Just v  -> mapM_ deps (mapMaybe getRef . toList $ v)

execRec :: State (RecState r) a -> Either [String] [r]
execRec op = (\st -> if null (errors st) then Right (seen st) else Left (errors st)) $ execState op (RecState [] [])

data RecState r = RecState {seen::[r],errors::Errors} deriving Show

-- |A list of error messages
type Errors = [Error]

type Error = String

toErrors :: Bifunctor p => p a c -> p [a] c
toErrors = first (:[])

noErrors :: Errors -> Bool
noErrors = null

errorToConvertResult :: (Typeable b, Typeable a, Show a) => (a -> Either Error b) -> a -> ConvertResult b
errorToConvertResult conv a = either (\err -> convError err a) Right $ conv a

{-|
>>> errorsToConvertResult (const (Left ["Bad format","Invalid value"])) ".." :: ConvertResult Int
Left (ConvertError {convSourceValue = "\"..\"", convSourceType = "[Char]", convDestType = "Int", convErrorMessage = "Bad format, Invalid value"})
-}
errorsToConvertResult :: (Typeable b, Typeable t, Show t) => (t -> Either Errors b) -> t -> ConvertResult b
errorsToConvertResult conv a = either (\errs -> convError (intercalate ", " errs) a) Right $ conv a

{-|
>>> convertOrError 'a' :: Either Error Word
Right 97

>>> convertOrError (1E50::Double) :: Either Error Word
Left "Convertible: error converting source data 1.0e50 of type Double to type Word: Input value outside of bounds: (0,18446744073709551615)"
-}
convertOrError :: Convertible a c => a -> Either String c
convertOrError = convertResultToError . safeConvert

convertResultToError :: Bifunctor p => p ConvertError c -> p String c
convertResultToError = first prettyConvertError

convertResultToErrors :: Bifunctor p => p ConvertError c -> p [String] c
convertResultToErrors = toErrors . convertResultToError

instance Convertible String String where safeConvert = Right . id

-- |Prefix errors with a contextual note
errsInContext :: (Convertible ctx String, Bifunctor p) => ctx -> p [String] c -> p [String] c
errsInContext ctx = first (inContext ctx)

{-|Prefix a list of strings with a contextual note

>>> inContext "0/0" ["Zero denominator"]
["In 0/0: Zero denominator"]
-}
inContext :: Convertible ctx String => ctx -> [String] -> [String]
inContext ctx = map (\msg -> unwords ["In",convert ctx++":",msg])

{-| Intercalate a dot between the non empty elements of a list of strings.

>>> dotted []
""

>>> dotted ["","bc","de"]
"bc.de"

>>> dotted ["bc","","de"]
"bc.de"
-}
dotted :: [String] -> String
-- dotted = intercalate "." . filter (not . null)
dotted [] = ""
dotted [s] = s
dotted (h:t) = post h ++ dotted t
    where post s | null s = ""
                 | otherwise = s ++ "."

