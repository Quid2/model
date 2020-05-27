{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Convert across types
module Data.Convertible.Tiny
  ( convert
  , Convertible(..)
  ,-- * Handling the results
    ConvertResult
  , ConvertError(..)
  , convError
  , prettyConvertError
  )
where

import           Data.Convertible.Base
import           Data.Convertible.Instances.Num ( )
import           Data.Convertible.Instances.Text
                                                ( )

{- | Any type can be converted to itself. -}
instance Convertible a a where
  safeConvert = return
