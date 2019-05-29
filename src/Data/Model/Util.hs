{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Data.Model.Util
  ( -- * Dependencies
  --   properMutualGroups
  -- , mutualGroups
  -- , transitiveClosure
  -- * Error utilities
  Errors
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

import           Data.Bifunctor
import           Data.Convertible
import           Data.Convertible.Tiny()
import           Data.Typeable
import           Data.List
-- import           Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)

-- |A list of error messages
type Errors = [Error]

type Error = String

toErrors :: Bifunctor p => p a c -> p [a] c
toErrors = first (: [])

noErrors :: Errors -> Bool
noErrors = null

errorToConvertResult
  :: (Typeable b, Typeable a, Show a)
  => (a -> Either Error b)
  -> a
  -> ConvertResult b
errorToConvertResult conv a = either (`convError` a) Right $ conv a

{-|
>>> errorsToConvertResult (const (Left ["Bad format","Invalid value"])) ".." :: ConvertResult Int
Left (ConvertError {convSourceValue = "\"..\"", convSourceType = "[Char]", convDestType = "Int", convErrorMessage = "Bad format, Invalid value"})
-}
errorsToConvertResult
  :: (Typeable b, Typeable t, Show t)
  => (t -> Either Errors b)
  -> t
  -> ConvertResult b
errorsToConvertResult conv a =
  either (\errs -> convError (intercalate ", " errs) a) Right $ conv a

{-|
>>> import Data.Word
>>> convertOrError 'a' :: Either Error Word
Right 97

>>> convertOrError (1E50::Double) :: Either Error Word64
Left "Convertible: error converting source data 1.0e50 of type Double to type Word: Input value outside of bounds: (0,18446744073709551615)"
-}
convertOrError :: Convertible a c => a -> Either String c
convertOrError = convertResultToError . safeConvert

convertResultToError :: Bifunctor p => p ConvertError c -> p String c
convertResultToError = first prettyConvertError

convertResultToErrors :: Bifunctor p => p ConvertError c -> p [String] c
convertResultToErrors = toErrors . convertResultToError

--instance Convertible String String where safeConvert = Right
-- instance Convertible a a where safeConvert = Right

-- |Prefix errors with a contextual note
errsInContext
  :: (Convertible ctx String, Bifunctor p)
  => ctx
  -> p [String] c
  -> p [String] c
errsInContext ctx = first (inContext ctx)

{-|Prefix a list of strings with a contextual note

>>> inContext "0/0" ["Zero denominator"]
["In 0/0: Zero denominator"]
-}
inContext :: Convertible ctx String => ctx -> [String] -> [String]
inContext ctx = map (\msg -> unwords ["In", convert ctx ++ ":", msg])

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
dotted []    = ""
dotted [s  ] = s
dotted (h:t) = post h ++ dotted t
 where
  post s | null s    = ""
         | otherwise = s ++ "."
