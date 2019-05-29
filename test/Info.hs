{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Info
  ( modelTests
  )
where
import           Data.Model
import           Data.Word
import qualified GHC.Base
import           Test.Data
import           Test.Data.Model                ( )
import qualified Test.Data2
import qualified Test.Data3
import qualified Data.Either
import           Data.List
import qualified Data.Map                      as M
import           Test.Tasty
import           Test.Tasty.HUnit

testSimpleModel
  :: AsType (Ana a)
  => Proxy a
  -> (Type (TypeRef Name), [ADT String String (TypeRef Name)])
  -> TestTree
testSimpleModel p e =
  let tm = typeModel p
      s  = prettyShow . simpleType $ typeName tm
  in  testCase (unwords ["simple typeModel of", s]) $ simpleHTypeEnv tm @?= e

testModel
  :: AsType (Ana a)
  => Proxy a
  -> (Type QualName, [(QualName, ADT String String (TypeRef QualName))])
  -> TestTree
testModel p (etn, ete) =
  let tm = typeModel p
      s  = prettyShow . simpleType $ typeName tm
  in  testCase (unwords ["typeModel of", s]) $ tm @?= TypeModel
        etn
        (M.fromList ete)

models :: [HTypeModel]
models =
  [ typeModel (Proxy :: Proxy Void)
  , typeModel (Proxy :: Proxy Unit)
  , typeModel (Proxy :: Proxy Bool)
  , typeModel (Proxy :: Proxy Char)
  , typeModel (Proxy :: Proxy String)
  , typeModel (Proxy :: Proxy [Bool])
  , typeModel (Proxy :: Proxy N)
  , typeModel (Proxy :: Proxy Un)
  , typeModel (Proxy :: Proxy D2)
  , typeModel (Proxy :: Proxy D4)
  , typeModel (Proxy :: Proxy A0)
  , typeModel (Proxy :: Proxy B0)
       -- ,typeModel (Proxy :: Proxy Various)
       -- ghc chokes on these heavily mutually dependent types
       -- ,typeModel (Proxy :: Proxy MM1)
       -- ,typeModel (Proxy :: Proxy MM2)
       -- ,typeModel (Proxy :: Proxy MM3)
  , typeModel (Proxy :: Proxy (Phantom Unit))
  , typeModel (Proxy :: Proxy (List Bool))
  , typeModel (Proxy :: Proxy (Test.Data2.List Bool))
  , typeModel (Proxy :: Proxy (Test.Data3.List Bool))
  , typeModel (Proxy :: Proxy (List (Test.Data2.List (Test.Data3.List Bool))))
  , typeModel (Proxy :: Proxy (Either Void Bool))
  , typeModel (Proxy :: Proxy (Either Bool Unit))
  , typeModel (Proxy :: Proxy (RR Un Unit N))
  , typeModel (Proxy :: Proxy (Either Bool (List Unit)))
  , typeModel (Proxy :: Proxy (Tr (Either Unit N)))
  , typeModel (Proxy :: Proxy (Perfect Bool))
        -- Unsupported: higher kind
        --,typeModel (Proxy :: Proxy (PerfectF Maybe Bool))
        --,typeModel (Proxy :: Proxy (Free Maybe Bool))
  ]

--------- Some (fake) instance declaration for primitive types, for testing only
instance Model Char where
  envType _ = envType (Proxy :: Proxy CharSI)
data CharSI deriving Generic
instance Model CharSI

-- Provide models for Word8 .. using stand-in classes
instance Model Word8 where
  envType _ = envType (Proxy :: Proxy Word8SI)
data Word8SI deriving Generic
instance Model Word8SI

-- instance Model a => Model [a] where envType _ = envType (Proxy::Proxy (ListSI a))
-- data ListSI a deriving Generic
-- instance Model a => Model (ListSI a)

instance Model a => Model [a]

instance Model ()

----- Pretty printing
-- Simplify types for test
simpleHTypeEnv
  :: Ord consName
  => TypeModel name consName HTypeRef QualName
  -> (Type (TypeRef Name), [ADT String consName (TypeRef Name)])
simpleHTypeEnv tm =
  (simpleType $ typeName tm, sort . map simpleADT $ M.assocs $ typeEnv tm)

hTypeEnv
  :: TypeModel adtName consName inRef k
  -> (Type k, [(k, ADT adtName consName inRef)])
hTypeEnv tm = (typeName tm, M.toList $ typeEnv tm)

simpleType :: Functor f => f QualName -> f (TypeRef Name)
simpleType = (TypRef . asName <$>)

simpleADT
  :: (QualName, ADT name consName HTypeRef)
  -> ADT String consName (TypeRef Name)
simpleADT (qname, adt) =
  ADT (qualName qname) (declNumParameters adt) ((mdlRef <$>) <$> declCons adt)

mdlRef :: HTypeRef -> TypeRef Name
mdlRef (TypVar v) = TypVar v
mdlRef (TypRef n) = TypRef (asName n)

asName :: QualName -> Name
asName qn | mdlName qn == "GHC.Types" = Name $ locName qn
asName n                              = Name . qualName $ n

pr :: Show a => a -> IO ()
pr = print
pp :: Pretty a => a -> IO ()
pp = putStrLn . prettyShow

----- Create tests
makeTests :: IO ()
makeTests = makeTest models

thisFile :: [Char]
thisFile = "test/Info.hs"

-- makeTest ts = appendFile thisFile $ ("\n-- Appended by makeTest\nmodelTests = testGroup \"Unit tests\" [" ++ (intercalate "\n\n  ," $ map (\tm -> unwords ["testModel (Proxy :: Proxy (",prettyShow . simpleType . typeName $ tm,")) (",show . simpleHTypeEnv $ tm,")"]) ts)) ++ " ]"

makeSimpleTest
  :: (Ord consName, Show consName)
  => [TypeModel name consName HTypeRef QualName]
  -> IO ()
makeSimpleTest ts =
  appendFile thisFile
    $  (  "\n-- Appended by makeTest\nmodelTests = testGroup \"Unit tests\" ["
       ++ (intercalate "\n\n  ," $ map
            (\tm -> unwords
              [ "testModel (Proxy :: Proxy ("
              , prettyShow . simpleType . typeName $ tm
              , ")) ("
              , show . simpleHTypeEnv $ tm
              , ")"
              ]
            )
            ts
          )
       )
    ++ " ]"

makeTest
  :: (Show adtName, Show inRef, Show consName)
  => [TypeModel adtName consName inRef QualName]
  -> IO ()
makeTest ts =
  appendFile thisFile
    $  (  "\n-- Appended by makeTest\nmodelTests = testGroup \"Unit tests\" ["
       ++ (intercalate "\n\n  ," $ map
            (\tm -> unwords
              [ "testModel (Proxy :: Proxy ("
              , prettyShow . simpleType . typeName $ tm
              , ")) ("
              , show . hTypeEnv $ tm
              , ")"
              ]
            )
            ts
          )
       )
    ++ " ]"

-- modelTests = undefined


-- Appended by makeTest
modelTests = testGroup "Unit tests" [testModel (Proxy :: Proxy ( Test.Data.Void )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Void"}),[(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Void"},ADT {declName = "Void", declNumParameters = 0, declCons = Nothing})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.Unit )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"}),[(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})})]) )

  ,testModel (Proxy :: Proxy ( Bool )) ( (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))})]) )

  ,testModel (Proxy :: Proxy ( Info.CharSI )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Info", locName = "CharSI"}),[(QualName {pkgName = "main", mdlName = "Info", locName = "CharSI"},ADT {declName = "CharSI", declNumParameters = 0, declCons = Nothing})]) )

  ,testModel (Proxy :: Proxy ( [] Info.CharSI )) ( (TypeApp (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "[]"})) (TypeCon (QualName {pkgName = "main", mdlName = "Info", locName = "CharSI"})),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "[]"},ADT {declName = "[]", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "[]", constrFields = Left []}) (Con {constrName = ":", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "[]"}))) (TypeCon (TypVar 0))]}))}),(QualName {pkgName = "main", mdlName = "Info", locName = "CharSI"},ADT {declName = "CharSI", declNumParameters = 0, declCons = Nothing})]) )

  ,testModel (Proxy :: Proxy ( [] Bool )) ( (TypeApp (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "[]"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "[]"},ADT {declName = "[]", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "[]", constrFields = Left []}) (Con {constrName = ":", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "[]"}))) (TypeCon (TypVar 0))]}))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.N )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"}),[(QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"},ADT {declName = "N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.Un )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Un"}),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Un"},ADT {declName = "Un", declNumParameters = 0, declCons = Just (Con {constrName = "Un", constrFields = Right [("un",TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})))]})})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.D2 )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "D2"}),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "D2"},ADT {declName = "D2", declNumParameters = 0, declCons = Just (Con {constrName = "D2", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"}))]})}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"},ADT {declName = "N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.D4 )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "D4"}),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "D4"},ADT {declName = "D4", declNumParameters = 0, declCons = Just (Con {constrName = "D4", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "N3"}))]})}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"},ADT {declName = "N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "N3"},ADT {declName = "N3", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "N1", constrFields = Left []}) (ConTree (Con {constrName = "N2", constrFields = Left []}) (Con {constrName = "N3", constrFields = Left []})))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.A0 )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "A0"}),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "A0"},ADT {declName = "A0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "A0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "B0"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "B0"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "D0"})),TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))]}) (Con {constrName = "A1", constrFields = Left [TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"}))),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})))]}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "B0"},ADT {declName = "B0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "B0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "C0"}))]}) (Con {constrName = "B1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "C0"},ADT {declName = "C0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "C0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "A0"}))]}) (Con {constrName = "C1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "D0"},ADT {declName = "D0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "D0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "E0"}))]}) (Con {constrName = "D1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "E0"},ADT {declName = "E0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "E0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "D0"}))]}) (Con {constrName = "E1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}),(QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.B0 )) ( (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "B0"}),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "A0"},ADT {declName = "A0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "A0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "B0"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "B0"})),TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "D0"})),TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))]}) (Con {constrName = "A1", constrFields = Left [TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"}))),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})))]}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "B0"},ADT {declName = "B0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "B0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "C0"}))]}) (Con {constrName = "B1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "C0"},ADT {declName = "C0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "C0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "A0"}))]}) (Con {constrName = "C1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "D0"},ADT {declName = "D0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "D0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "E0"}))]}) (Con {constrName = "D1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "E0"},ADT {declName = "E0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "E0", constrFields = Left [TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "D0"}))]}) (Con {constrName = "E1", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}),(QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.Phantom Test.Data.Unit )) ( (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Phantom"})) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"})),[(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Phantom"},ADT {declName = "Phantom", declNumParameters = 1, declCons = Just (Con {constrName = "Phantom", constrFields = Left []})}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.List Bool )) ( (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data2.List Bool )) ( (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data3.List Bool )) ( (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data3", locName = "List"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data3", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data3", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.List (Test.Data2.List (Test.Data3.List Bool)) )) ( (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"})) (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"})) (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data3", locName = "List"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})))),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data2", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data3", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data3", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))})]) )

  ,testModel (Proxy :: Proxy ( Data.Either.Either Test.Data.Void Bool )) ( (TypeApp (TypeApp (TypeCon (QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"})) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Void"}))) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),[(QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"},ADT {declName = "Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))}),(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Void"},ADT {declName = "Void", declNumParameters = 0, declCons = Nothing})]) )

  ,testModel (Proxy :: Proxy ( Data.Either.Either Bool Test.Data.Unit )) ( (TypeApp (TypeApp (TypeCon (QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"})),[(QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"},ADT {declName = "Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))}),(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.RR Test.Data.Un Test.Data.Unit Test.Data.N )) ( (TypeApp (TypeApp (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "RR"})) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Un"}))) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"}))) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"})),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"},ADT {declName = "N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "RR"},ADT {declName = "RR", declNumParameters = 3, declCons = Just (ConTree (Con {constrName = "RN", constrFields = Right [("rna",TypeCon (TypVar 0)),("rnb",TypeCon (TypVar 1)),("rnc",TypeCon (TypVar 2))]}) (ConTree (Con {constrName = "RA", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeApp (TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "RR"}))) (TypeCon (TypVar 0))) (TypeCon (TypVar 0))) (TypeCon (TypVar 2)),TypeCon (TypVar 1)]}) (Con {constrName = "RAB", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeApp (TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "RR"}))) (TypeCon (TypVar 2))) (TypeCon (TypVar 1))) (TypeCon (TypVar 0)),TypeCon (TypVar 1)]})))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Un"},ADT {declName = "Un", declNumParameters = 0, declCons = Just (Con {constrName = "Un", constrFields = Right [("un",TypeCon (TypRef (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})))]})}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})})]) )

  ,testModel (Proxy :: Proxy ( Data.Either.Either Bool (Test.Data.List Test.Data.Unit) )) ( (TypeApp (TypeApp (TypeCon (QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"}))) (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"})) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"}))),[(QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"},ADT {declName = "Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))}),(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.Tr (Data.Either.Either Test.Data.Unit Test.Data.N) )) ( (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Tr"})) (TypeApp (TypeApp (TypeCon (QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"})) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"}))) (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"}))),[(QualName {pkgName = "base", mdlName = "Data.Either", locName = "Either"},ADT {declName = "Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Forest"},ADT {declName = "Forest", declNumParameters = 1, declCons = Just (Con {constrName = "Forest", constrFields = Left [TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Tr"}))) (TypeCon (TypVar 0)))]})}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"},ADT {declName = "List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "List"}))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "N"},ADT {declName = "N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Tr"},ADT {declName = "Tr", declNumParameters = 1, declCons = Just (Con {constrName = "Tr", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Forest"}))) (TypeCon (TypVar 0))]})}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Unit"},ADT {declName = "Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})})]) )

  ,testModel (Proxy :: Proxy ( Test.Data.Perfect Bool )) ( (TypeApp (TypeCon (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Perfect"})) (TypeCon (QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"})),[(QualName {pkgName = "ghc-prim", mdlName = "GHC.Types", locName = "Bool"},ADT {declName = "Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Fork"},ADT {declName = "Fork", declNumParameters = 1, declCons = Just (Con {constrName = "Fork", constrFields = Left [TypeCon (TypVar 0),TypeCon (TypVar 0)]})}),(QualName {pkgName = "main", mdlName = "Test.Data", locName = "Perfect"},ADT {declName = "Perfect", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "ZeroP", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "SuccP", constrFields = Left [TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Perfect"}))) (TypeApp (TypeCon (TypRef (QualName {pkgName = "main", mdlName = "Test.Data", locName = "Fork"}))) (TypeCon (TypVar 0)))]}))})]) ) ]