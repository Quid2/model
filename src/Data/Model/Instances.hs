-- |Instances of `Model` for common types (Bool,Maybe,Either).
module Data.Model.Instances where

import           Data.Model.Class

instance Model Bool

instance Model a => Model (Maybe a)

instance (Model a,Model b) => Model (Either a b)

