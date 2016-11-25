{-# LANGUAGE TypeOperators #-}
-- |Environment used while capturing model
module Data.Model.Env(State,Env,withEnv,enterCtx,addDef,closeCtx) where

import           Control.Monad.Trans.State.Lazy
import           Data.Bifunctor
import qualified Data.Map                       as M
import           Data.Model.Types

-- |Environment used while capturing model
data Env = Env {ctx::[QualName]  -- ^The stack of entered types
               ,env::HTypeEnv    -- ^The environment
               } deriving Show

-- |Run the model capturing computation
withEnv :: State Env HType -> HTypeModel
withEnv m = (\(t,e) -> TypeModel (unVar <$> t) e) . second env $ runState m (Env [] M.empty)

-- |Enter a type
enterCtx :: QualName -> State Env Bool
enterCtx name = do
   f <- inCtx name
   modify (\e -> e {ctx = name : ctx e})
   return f

-- |Returns True if we have already seen this data type and do not need to analyse it further
inCtx :: QualName -> State Env Bool
inCtx name = (name `elem`) <$> gets (\e -> ctx e ++ M.keys (env e))

-- |Add a new data type model to the environment
addDef :: QualName -> HADT -> State Env ()
addDef ref adt  = modify (\e -> e {env = M.insert ref adt (env e)})

-- |Leave current type
closeCtx :: State Env ()
closeCtx = modify (\e -> e {ctx = drop 1 (ctx e)})

-- pretty print environment
-- showEnv e = unlines [""
--                   ,show $ map locName $ seen e
--                   ,show $ map (first locName) $ ctx e
--                   ,show $ map (\(d,deps) -> (locName $ declName d,map locName deps)) $ defs e
--                   ]
