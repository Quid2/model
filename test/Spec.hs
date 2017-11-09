{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import qualified Data.Map         as M
import           Data.Model
import           Info
import           Test.Data
import           Test.Data.Model  ()
import           Test.Tasty
import           Test.Tasty.HUnit

t :: IO ()
t = main

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [
    modelTests
    , transformTests
    , namesTests
    , prettyTests
    ]

transformTests :: TestTree
transformTests = testGroup "Transform Tests" [
  testTrc [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])] "b" ["c","a","d","b"]
  ,testTrc [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])] "c" ["c"]
  ,testMutual ([("a",["b","c"]),("b",["a","c"]),("c",[])]) (Right [["c"],["b","a"]])
  ,testMutual ([("a",["b","c"]),("b",["a","c","b"]),("c",["d"]),("d",[])]) (Right [["d"],["c"],["b","a"]])
  ,testMutual ([("a",["z","c"]),("b",["a","c","b"]),("c",["d"])]) (Left ["transitiveClosure:Unknown reference to \"d\"","transitiveClosure:Unknown reference to \"z\""])
  ]
  where
    testTrc adjs start etrc =
      let Right trc = transitiveClosure Just (M.fromList adjs) start
      in testCase (unwords ["transitiveClosure",show adjs,start]) $ trc @?= etrc

    testMutual adjs emut =
      let mut = mutualGroups Just (M.fromList adjs)
      in testCase (unwords ["mutualGroups",show adjs]) $ mut @?= emut

namesTests :: TestTree
namesTests = testGroup "QualName Tests" [
  tstErr "" "Empty qualified name"
  ,tst "ab" $ QualName "" "" "ab"
  --,tstErr "a.b.c.d" "Too many components in qualified name 'a.b.c.d'"
  ,tst "ab.cd" $ QualName "" "ab" "cd"
  ,tst "Data.List" $ QualName "" "Data" "List"
  ,tst "ab.cd.ef" $ QualName "ab" "cd" "ef"
  ,tst "list.Data.List" $ QualName "list" "Data" "List"
  ,tst "ab.cd.ef.gh" $ QualName "ab" "cd.ef" "gh"
  ]
  where
    tstErr s err = testCase (unwords ["Parse QualName Fail",s]) $ convertResultToError (safeConvert s::ConvertResult QualName) @?= Left ("Convertible: error converting source data \"\" of type [Char] to type QualName: "++ err)
    tst s q = testGroup (unwords ["Parse QualName",s])
      [testCase "parse" $ safeConvert s @?= Right q
       ,testCase "roundtrip" $ convert (convert s::QualName) @?= s
      ]

prettyTests :: TestTree
prettyTests = testGroup "Pretty Tests" $ map tst $ zip models2 pretty2
  where
    tst (model,pretty) = testCase (unwords ["Pretty"]) $ prettyShow model @?= pretty
    -- let env = typeEnv model in prettyShow (env,model) @?= pretty

    models2 = [typeModel (Proxy :: Proxy (List Bool))]

    pretty2 = ["Type:\nmain.Test.Data.List ghc-prim.GHC.Types.Bool -> List Bool\nEnvironment:\nghc-prim.GHC.Types.Bool ->  Bool \8801   False\n        | True\nmain.Test.Data.List ->  List a \8801   C a (main.Test.Data.List a)\n          | N"]

