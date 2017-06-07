module Data.Model.Util (mutualGroups, transitiveClosure, Errors) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable             (toList)
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe

-- |Return the groups of entities that are mutually dependent
--
-- >>> mutualGroups Just (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
-- [["c"],["b","a"]]
mutualGroups :: (Ord r, Show r, Foldable t) => (a -> Maybe r) -> M.Map r (t a) -> [[r]]
mutualGroups getRef env = recs [] (M.keys env)
  where
    deps n = unsafely (transitiveClosure getRef env n)
    recs gs [] = gs
    recs gs (n:ns) =
      let mutual = filter (\o -> n `elem` deps o) (deps n)
      in recs (mutual:gs) (ns \\ mutual)

-- >>>mutualDeps (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
-- fromList [("a",["b"]),("b",["a"]),("c",[])]
-- mutualDeps :: (Ord a, Show a) => M.Map a [a] -> M.Map a [a]
-- mutualDeps deps = M.mapWithKey (\n ds -> filter (\o -> n `elem` (solve o deps)) ds) deps

-- |Return the transitive closure of an element in a graph of dependencies specified as an adjacency list
--
-- >>> transitiveClosure Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "b"
-- Right ["c","a","d","b"]
-- 
-- >>> transitiveClosure Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "c"
-- Right ["c"]
transitiveClosure :: (Ord r, Show r, Foldable t) => (a -> Maybe r) -> M.Map r (t a) -> r -> Either Errors [r]
transitiveClosure getRef env = execRec . deps
    where
      deps n = do
         present <- (n `elem`) <$> gets seen
         unless present $ do
           modify (\st -> st {seen=n:seen st})
           case M.lookup n env of
             Nothing -> modify (\st -> st {errors=unwords ["transitiveClosure:Unknown reference to",show n]:errors st})
             Just v  -> mapM_ deps (mapMaybe getRef . toList $ v)

-- |Extract a Right value from an Either, throw an error if it is Left
unsafely :: Either Errors c -> c
unsafely = either (error.unlines) id

-- execRec :: State (RecState r) a -> Either [String] [r]
-- execRec op = (\st -> if null (errors st) then Right (tail . reverse . seen $ st) else Left (errors st)) $ execState op (RecState [] [])

execRec :: State (RecState r) a -> Either [String] [r]
execRec op = (\st -> if null (errors st) then Right (seen st) else Left (errors st)) $ execState op (RecState [] [])

data RecState r = RecState {seen::[r],errors::Errors} deriving Show

-- |A list of error messages
type Errors = [String]
