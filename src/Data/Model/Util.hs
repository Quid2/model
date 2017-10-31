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
  -- * Convertible utilities
  , Convertible(..)
  , convert
  , ConvertResult
  , ConvertError(..)
  , errorToConvertResult
  , errorsToConvertResult
  , convertResultToError
  , convertResultToErrors
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

-- |Either an error or a valid value
-- type EitherError a = Either String a

-- |Either errors or a valid value
-- type EitherErrors a = Either [String] a

toErrors :: Either Error a -> Either Errors a
toErrors = first (:[])

noErrors :: Errors -> Bool
noErrors = null

errorToConvertResult
  :: (Typeable b, Typeable a, Show a)
  => (a -> Either Error b) -> a -> ConvertResult b
errorToConvertResult conv a = either (\err -> convError err a) Right $ conv a

errorsToConvertResult
  :: (Typeable b, Typeable a, Show a)
  => (a -> Either Errors b) -> a -> ConvertResult b
errorsToConvertResult conv a = either (\errs -> convError (unwords errs) a) Right $ conv a

convertResultToError :: ConvertResult a -> Either Error a
convertResultToError = first prettyConvertError

convertResultToErrors :: ConvertResult a -> Either Errors a
convertResultToErrors = toErrors . convertResultToError

instance Convertible String String where safeConvert = Right . id

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
