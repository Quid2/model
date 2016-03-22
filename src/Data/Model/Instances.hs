{-# LANGUAGE ScopedTypeVariables #-}
module Data.Model.Instances where

import           Data.Model.Class
import           Data.Model.Types

instance Model Bool

instance Model a => Model (Maybe a)

instance (Model a,Model b) => Model (Either a b)

-- instance (Model a,Model b) => Model (a,b)
-- instance (Model a,Model b,Model c) => Model (a,b,c)
-- instance (Model a,Model b,Model c,Model d) => Model (a,b,c,d)






