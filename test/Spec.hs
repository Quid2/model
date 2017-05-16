{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import qualified Data.Either
import           Data.List
import qualified Data.Map         as M
import           Data.Model
import           Data.Word
import qualified GHC.Base
import qualified GHC.Types
import           Test.Data
import           Test.Data.Model  ()
import qualified Test.Data2
import qualified Test.Data3
import           Test.Tasty
import           Test.Tasty.HUnit

-- main = makeTests
main = mainTest

mainTest = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, transformTests, unitTests]

properties = testGroup "Properties" []

models = [typeModel (Proxy :: Proxy Void)
       ,typeModel (Proxy :: Proxy Unit)
       ,typeModel (Proxy :: Proxy Bool)
       ,typeModel (Proxy :: Proxy Char)
       ,typeModel (Proxy :: Proxy String)
       ,typeModel (Proxy :: Proxy [Bool])
       ,typeModel (Proxy :: Proxy N)
       ,typeModel (Proxy :: Proxy Un)
       ,typeModel (Proxy :: Proxy D2)
       ,typeModel (Proxy :: Proxy D4)
       ,typeModel (Proxy :: Proxy A0)
       ,typeModel (Proxy :: Proxy B0)
       -- ,typeModel (Proxy :: Proxy Various)
       -- ghc chokes on these heavily mutually dependent types
       -- ,typeModel (Proxy :: Proxy MM1)
       -- ,typeModel (Proxy :: Proxy MM2)
       -- ,typeModel (Proxy :: Proxy MM3)
       ,typeModel (Proxy :: Proxy (Phantom Unit))
       ,typeModel (Proxy :: Proxy (List Bool))
       ,typeModel (Proxy :: Proxy (Test.Data2.List Bool))
       ,typeModel (Proxy :: Proxy (Test.Data3.List Bool))
       ,typeModel (Proxy :: Proxy (List (Test.Data2.List (Test.Data3.List Bool))))
       ,typeModel (Proxy :: Proxy (Maybe Void))
       ,typeModel (Proxy :: Proxy (Either Bool Unit))
       ,typeModel (Proxy :: Proxy (RR Un Unit N))
       ,typeModel (Proxy :: Proxy (Either Bool (List Unit)))
       ,typeModel (Proxy :: Proxy (Tr (Maybe Unit)))
       ,typeModel (Proxy :: Proxy (Perfect Bool))

        -- Unsupported: higher kind
        --,typeModel (Proxy :: Proxy (PerfectF Maybe Bool))
        --,typeModel (Proxy :: Proxy (Free Maybe Bool))
       ]

tst p e = let tm = typeModel p
              s = prettyShow . simpleType $ typeName tm
          in testCase (unwords ["typeModel of",s]) $ simpleHTypeEnv tm @?= e

transformTests = testGroup "Transform Tests" [
  testTrc [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])] "b" ["c","a","d","b"]
  ,testTrc [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])] "c" ["c"]
  ,testMutual ([("a",["b","c"]),("b",["a","c"]),("c",[])]) [["c"],["b","a"]]
  ]
  where
    testTrc adjs start etrc =
      let Right trc = transitiveClosure Just (M.fromList adjs) start
      in testCase (unwords ["transitiveClosure",show adjs,start]) $ trc @?= etrc

    testMutual adjs emut =
      let mut = mutualGroups Just (M.fromList adjs)
      in testCase (unwords ["mutualGroups",show adjs]) $ mut @?= emut

----- Pretty printing
-- Simplify type for test
simpleHTypeEnv tm = (simpleType $ typeName tm
                    ,sort . map simpleADT $ M.assocs $ typeEnv tm)

simpleType = (TypRef . asName <$>)
simpleADT (qname,adt) = ADT (qualName qname) (declNumParameters adt) ((mdlRef <$>) <$> declCons adt)

mdlRef :: HTypeRef -> TypeRef Name
mdlRef (TypVar v) = TypVar v
mdlRef (TypRef n) = TypRef (asName n)

asName = Name . qualName

pr = print
pp = putStrLn . prettyShow

-- THIS STOPPED WORKING (ghc 8.01?), Char not instance of Generic
-- instance Model Char

-- Some fake instance declaration for primitive types
instance Model Char where envType _ = envType (Proxy::Proxy CharSI)
data CharSI deriving Generic
instance Model CharSI

-- Provide models for Word8 .. using stand-in classes
instance Model Word8 where envType _ = envType (Proxy::Proxy Word8SI)
data Word8SI deriving Generic
instance Model Word8SI

-- TODO: Fix problems with types using symbolic constructors
instance Model a => Model [a] where envType _ = envType (Proxy::Proxy (ListSI a))

data ListSI a deriving Generic
instance Model a => Model (ListSI a)

instance Model ()

----- Create tests
makeTests = makeTest models
-- unitTests = undefined

thisFile = "test/Spec.hs"

makeTest ts = appendFile thisFile $ ("\n-- Appended by makeTest\nunitTests = testGroup \"Unit tests\" [" ++ (intercalate "\n\n  ," $ map (\tm -> unwords ["tst (Proxy :: Proxy (",prettyShow . simpleType . typeName $ tm,")) (",show . simpleHTypeEnv $ tm,")"]) ts)) ++ " ]"


-- Appended by makeTest
unitTests = testGroup "Unit tests" [tst (Proxy :: Proxy ( Test.Data.Void )) ( (TypeCon (TypRef (Name "Test.Data.Void")),[ADT {declName = "Test.Data.Void", declNumParameters = 0, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy ( Test.Data.Unit )) ( (TypeCon (TypRef (Name "Test.Data.Unit")),[ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy ( GHC.Types.Bool )) ( (TypeCon (TypRef (Name "GHC.Types.Bool")),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy ( Main.CharSI )) ( (TypeCon (TypRef (Name "Main.CharSI")),[ADT {declName = "Main.CharSI", declNumParameters = 0, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy ( Main.ListSI Main.CharSI )) ( (TypeApp (TypeCon (TypRef (Name "Main.ListSI"))) (TypeCon (TypRef (Name "Main.CharSI"))),[ADT {declName = "Main.CharSI", declNumParameters = 0, declCons = Nothing},ADT {declName = "Main.ListSI", declNumParameters = 1, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy ( Main.ListSI GHC.Types.Bool )) ( (TypeApp (TypeCon (TypRef (Name "Main.ListSI"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Main.ListSI", declNumParameters = 1, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy ( Test.Data.N )) ( (TypeCon (TypRef (Name "Test.Data.N")),[ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))}]) )

  ,tst (Proxy :: Proxy ( Test.Data.Un )) ( (TypeCon (TypRef (Name "Test.Data.Un")),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.Un", declNumParameters = 0, declCons = Just (Con {constrName = "Un", constrFields = Right [("un",TypeCon (TypRef (Name "GHC.Types.Bool")))]})}]) )

  ,tst (Proxy :: Proxy ( Test.Data.D2 )) ( (TypeCon (TypRef (Name "Test.Data.D2")),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.D2", declNumParameters = 0, declCons = Just (Con {constrName = "D2", constrFields = Left [TypeCon (TypRef (Name "GHC.Types.Bool")),TypeCon (TypRef (Name "Test.Data.N"))]})},ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))}]) )

  ,tst (Proxy :: Proxy ( Test.Data.D4 )) ( (TypeCon (TypRef (Name "Test.Data.D4")),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.D4", declNumParameters = 0, declCons = Just (Con {constrName = "D4", constrFields = Left [TypeCon (TypRef (Name "GHC.Types.Bool")),TypeCon (TypRef (Name "Test.Data.N")),TypeCon (TypRef (Name "Test.Data.Unit")),TypeCon (TypRef (Name "Test.Data.N3"))]})},ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))},ADT {declName = "Test.Data.N3", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "N1", constrFields = Left []}) (ConTree (Con {constrName = "N2", constrFields = Left []}) (Con {constrName = "N3", constrFields = Left []})))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy ( Test.Data.A0 )) ( (TypeCon (TypRef (Name "Test.Data.A0")),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.A0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "A0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.D0")),TypeCon (TypRef (Name "GHC.Types.Bool"))]}) (Con {constrName = "A1", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "Test.Data.Unit"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))]}))},ADT {declName = "Test.Data.B0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "B0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.C0"))]}) (Con {constrName = "B1", constrFields = Left []}))},ADT {declName = "Test.Data.C0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "C0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.A0"))]}) (Con {constrName = "C1", constrFields = Left []}))},ADT {declName = "Test.Data.D0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "D0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.E0"))]}) (Con {constrName = "D1", constrFields = Left []}))},ADT {declName = "Test.Data.E0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "E0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.D0"))]}) (Con {constrName = "E1", constrFields = Left []}))},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data2.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy ( Test.Data.B0 )) ( (TypeCon (TypRef (Name "Test.Data.B0")),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.A0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "A0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.D0")),TypeCon (TypRef (Name "GHC.Types.Bool"))]}) (Con {constrName = "A1", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "Test.Data.Unit"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))]}))},ADT {declName = "Test.Data.B0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "B0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.C0"))]}) (Con {constrName = "B1", constrFields = Left []}))},ADT {declName = "Test.Data.C0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "C0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.A0"))]}) (Con {constrName = "C1", constrFields = Left []}))},ADT {declName = "Test.Data.D0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "D0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.E0"))]}) (Con {constrName = "D1", constrFields = Left []}))},ADT {declName = "Test.Data.E0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "E0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.D0"))]}) (Con {constrName = "E1", constrFields = Left []}))},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data2.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy ( Test.Data.Phantom Test.Data.Unit )) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Phantom"))) (TypeCon (TypRef (Name "Test.Data.Unit"))),[ADT {declName = "Test.Data.Phantom", declNumParameters = 1, declCons = Just (Con {constrName = "Phantom", constrFields = Left []})},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy ( Test.Data.List GHC.Types.Bool )) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy ( Test.Data2.List GHC.Types.Bool )) ( (TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data2.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy ( Test.Data3.List GHC.Types.Bool )) ( (TypeApp (TypeCon (TypRef (Name "Test.Data3.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data3.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data3.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy ( Test.Data.List (Test.Data2.List (Test.Data3.List GHC.Types.Bool)) )) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeApp (TypeCon (TypRef (Name "Test.Data3.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "Test.Data2.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))},ADT {declName = "Test.Data3.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data3.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy ( GHC.Base.Maybe Test.Data.Void )) ( (TypeApp (TypeCon (TypRef (Name "GHC.Base.Maybe"))) (TypeCon (TypRef (Name "Test.Data.Void"))),[ADT {declName = "GHC.Base.Maybe", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Nothing", constrFields = Left []}) (Con {constrName = "Just", constrFields = Left [TypeCon (TypVar 0)]}))},ADT {declName = "Test.Data.Void", declNumParameters = 0, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy ( Data.Either.Either GHC.Types.Bool Test.Data.Unit )) ( (TypeApp (TypeApp (TypeCon (TypRef (Name "Data.Either.Either"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))) (TypeCon (TypRef (Name "Test.Data.Unit"))),[ADT {declName = "Data.Either.Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy ( Test.Data.RR Test.Data.Un Test.Data.Unit Test.Data.N )) ( (TypeApp (TypeApp (TypeApp (TypeCon (TypRef (Name "Test.Data.RR"))) (TypeCon (TypRef (Name "Test.Data.Un")))) (TypeCon (TypRef (Name "Test.Data.Unit")))) (TypeCon (TypRef (Name "Test.Data.N"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))},ADT {declName = "Test.Data.RR", declNumParameters = 3, declCons = Just (ConTree (Con {constrName = "RN", constrFields = Right [("rna",TypeCon (TypVar 0)),("rnb",TypeCon (TypVar 1)),("rnc",TypeCon (TypVar 2))]}) (ConTree (Con {constrName = "RA", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeApp (TypeApp (TypeCon (TypRef (Name "Test.Data.RR"))) (TypeCon (TypVar 0))) (TypeCon (TypVar 0))) (TypeCon (TypVar 2)),TypeCon (TypVar 1)]}) (Con {constrName = "RAB", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeApp (TypeApp (TypeCon (TypRef (Name "Test.Data.RR"))) (TypeCon (TypVar 2))) (TypeCon (TypVar 1))) (TypeCon (TypVar 0)),TypeCon (TypVar 1)]})))},ADT {declName = "Test.Data.Un", declNumParameters = 0, declCons = Just (Con {constrName = "Un", constrFields = Right [("un",TypeCon (TypRef (Name "GHC.Types.Bool")))]})},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy ( Data.Either.Either GHC.Types.Bool (Test.Data.List Test.Data.Unit) )) ( (TypeApp (TypeApp (TypeCon (TypRef (Name "Data.Either.Either"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))) (TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "Test.Data.Unit")))),[ADT {declName = "Data.Either.Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy ( Test.Data.Tr (GHC.Base.Maybe Test.Data.Unit) )) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Tr"))) (TypeApp (TypeCon (TypRef (Name "GHC.Base.Maybe"))) (TypeCon (TypRef (Name "Test.Data.Unit")))),[ADT {declName = "GHC.Base.Maybe", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Nothing", constrFields = Left []}) (Con {constrName = "Just", constrFields = Left [TypeCon (TypVar 0)]}))},ADT {declName = "Test.Data.Forest", declNumParameters = 1, declCons = Just (Con {constrName = "Forest", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeApp (TypeCon (TypRef (Name "Test.Data.Tr"))) (TypeCon (TypVar 0)))]})},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "Test.Data.Tr", declNumParameters = 1, declCons = Just (Con {constrName = "Tr", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.Forest"))) (TypeCon (TypVar 0))]})},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy ( Test.Data.Perfect GHC.Types.Bool )) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Perfect"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.Fork", declNumParameters = 1, declCons = Just (Con {constrName = "Fork", constrFields = Left [TypeCon (TypVar 0),TypeCon (TypVar 0)]})},ADT {declName = "Test.Data.Perfect", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "ZeroP", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "SuccP", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.Perfect"))) (TypeApp (TypeCon (TypRef (Name "Test.Data.Fork"))) (TypeCon (TypVar 0)))]}))}]) ) ]
