module Data.Model.Util(mutualGroups,unsafely,recursively,Errors) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable             (toList)
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe

-- >>> mutualGroups Just (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
-- [["c"],["a","b"]]
mutualGroups :: (Ord r, Show r, Foldable t) => (a -> Maybe r) -> M.Map r (t a) -> [[r]]
mutualGroups getRef env = recs [] (M.keys env)
  where
    deps n = unsafely (recursively getRef env n)
    recs gs [] = gs
    recs gs (n:ns) =
      let mutual = filter (\o -> n `elem` deps o) (deps n)
      in recs ((n:mutual):gs) (ns \\ mutual)

type Errors = [String]

data RecState r = RecState {seen::[r],errors::Errors} deriving Show

execRec :: State (RecState r) a -> Either [String] [r]
execRec op = (\st -> if null (errors st) then Right (tail . reverse . seen $ st) else Left (errors st)) $ execState op (RecState [] [])

unsafely :: Either Errors c -> c
unsafely = either (error.unlines) id

-- |Return a list of the unique recursive dependencies of n in env
-- excluding n unless is recursive
-- >>> recursively Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "a"
-- Right ["b","d","c"]
--- >>> recursively Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "b"
-- Right ["d","a","c"]
recursively
  :: (Ord r, Show r, Foldable t) =>
     (a -> Maybe r) -> M.Map r (t a) -> r -> Either [String] [r]
recursively getRef env = execRec . deps
    where
      deps n = do
         present <- (n `elem`) <$> gets seen
         unless present $ do
           modify (\st -> st {seen=n:seen st})
           case M.lookup n env of
             Nothing -> modify (\st -> st {errors=(unwords ["Unknown reference to",show n]):errors st})
             Just v  -> mapM_ deps (mapMaybe getRef . toList $ v)
