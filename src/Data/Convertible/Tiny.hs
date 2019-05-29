{-# LANGUAGE  MultiParamTypeClasses  ,FlexibleInstances  #-}
module Data.Convertible.Tiny ( -- * The conversion process
convert,
Convertible(..),
-- * Handling the results
ConvertResult,
ConvertError(..),
convError,
prettyConvertError
) where

import Data.Convertible.Base
import Data.Convertible.Instances.Text()
import Data.Convertible.Instances.Num()

{- | Any type can be converted to itself. -}
instance Convertible a a where safeConvert = return

