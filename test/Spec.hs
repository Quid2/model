{-# LANGUAGE FlexibleContexts ,NoMonomorphismRestriction #-}
import qualified Data.Either
import           Data.List
import           Data.Model
import           Data.Traversable
import           Data.Typeable
import qualified GHC.Base
import qualified GHC.Types
import           Test.Data
import           Test.Data.Model
import qualified Test.Data2            as Data2
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

t = main

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties,  unitTests]

properties = testGroup "Properties" []

tst p e = let te@(t,_) = hTypeEnv p
              s = prettyShow . simpleHType $ t
          in testCase (unwords ["hTypeEnv of",s]) $ simpleHTypeEnv te @?= e

v = let tr = typeRep (undefined::Proxy (Phantom Char))
    in (typeRepFingerprint tr,splitTyConApp tr)

x = hTypeEnv (Proxy :: Proxy ([Bool]))
z = hTypeEnv (Proxy :: Proxy String)

qhType = (mdlRef <$>) . hType

tsts = [hTypeEnv (Proxy :: Proxy Void)
       ,hTypeEnv (Proxy :: Proxy Unit)
       ,hTypeEnv (Proxy :: Proxy Bool)
       ,hTypeEnv (Proxy :: Proxy Char)
       ,hTypeEnv (Proxy :: Proxy String)
       ,hTypeEnv (Proxy :: Proxy ([Bool]))
       ,hTypeEnv (Proxy :: Proxy N)
       ,hTypeEnv (Proxy :: Proxy Un)
       ,hTypeEnv (Proxy :: Proxy D2)
       ,hTypeEnv (Proxy :: Proxy D4)
       ,hTypeEnv (Proxy :: Proxy A0)
       ,hTypeEnv (Proxy :: Proxy B0)
       --,hTypeEnv (Proxy :: Proxy Various)
       -- ,hTypeEnv (Proxy :: Proxy MM1)
       -- ,hTypeEnv (Proxy :: Proxy MM2)
       -- ,hTypeEnv (Proxy :: Proxy MM3)
       ,hTypeEnv (Proxy :: Proxy (Phantom Unit))
       ,hTypeEnv (Proxy :: Proxy (List Bool))
       ,hTypeEnv (Proxy :: Proxy (Maybe Void))
       ,hTypeEnv (Proxy :: Proxy (Either Bool Unit))
       ,hTypeEnv (Proxy :: Proxy (RR Un Unit N))
       ,hTypeEnv (Proxy :: Proxy (Either Bool (List Unit)))
        -- ,hTypeEnv (Proxy :: Proxy (ADT String String))
       ,hTypeEnv (Proxy :: Proxy (Tr (Maybe Unit)))
       ,hTypeEnv (Proxy :: Proxy (Perfect Bool))
        --,hTypeEnv (Proxy :: Proxy (PerfectF Maybe Bool))
        --,hTypeEnv (Proxy :: Proxy (Free Maybe Bool))
       ]

-- tst = hTypeEnv

-- tst :: HasModel a => Proxy a -> String
-- tst p = let at = qhType p in unlines [prettyShow at,show at]
-- print tests

----- Pretty printing
simpleHType = (mdlRef <$>)

simpleHTypeEnv (t,e) = (mdlRef <$> t, map simpleADT e) -- (mdlRef <$>) <$> e)

simpleADT adt = ADT (qualName . declName $ adt) (declNumParameters adt) ((mdlRef <$>) <$> declCons adt)

mdlRef :: HTypeRef -> TypeRef Name
mdlRef (TypVar v)   = TypVar v
mdlRef (TypRef n)   = TypRef (Name $ qualName n)

data Name = Name String deriving (Eq,Show)
instance Pretty Name where pPrint (Name n)= text n

pr = print
pp = putStrLn . prettyShow

----- Create tests
m = makeTest tsts
--unitTests = undefined

-- thisFile = "/Users/titto/workspace/module/test/Spec.hs"
thisFile = "test/Spec.hs"

makeTest ts = appendFile thisFile $ ("\n-- Appended by makeTest\nunitTests = testGroup \"Unit tests\" [" ++ (intercalate "\n\n  ," $ map (\at -> unwords ["tst (Proxy :: Proxy",prettyShow . simpleHType . fst $ at,") (",show . simpleHTypeEnv $ at,")"]) ts)) ++ " ]"



-- Appended by makeTest
unitTests = testGroup "Unit tests" [tst (Proxy :: Proxy Test.Data.Void ) ( (TypeCon (TypRef (Name "Test.Data.Void")),[ADT {declName = "Test.Data.Void", declNumParameters = 0, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy Test.Data.Unit ) ( (TypeCon (TypRef (Name "Test.Data.Unit")),[ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy GHC.Types.Bool ) ( (TypeCon (TypRef (Name "GHC.Types.Bool")),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy GHC.Types.Char ) ( (TypeCon (TypRef (Name "GHC.Types.Char")),[ADT {declName = "GHC.Types.Char", declNumParameters = 0, declCons = Just (Con {constrName = "", constrFields = Left [TypeCon (TypRef (Name "GHC.Types.Char"))]})}]) )

  ,tst (Proxy :: Proxy (Test.Data.Model.ListSI GHC.Types.Char) ) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Model.ListSI"))) (TypeCon (TypRef (Name "GHC.Types.Char"))),[ADT {declName = "GHC.Types.Char", declNumParameters = 0, declCons = Just (Con {constrName = "", constrFields = Left [TypeCon (TypRef (Name "GHC.Types.Char"))]})},ADT {declName = "Test.Data.Model.ListSI", declNumParameters = 1, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy (Test.Data.Model.ListSI GHC.Types.Bool) ) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Model.ListSI"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.Model.ListSI", declNumParameters = 1, declCons = Nothing}]) )

  ,tst (Proxy :: Proxy Test.Data.N ) ( (TypeCon (TypRef (Name "Test.Data.N")),[ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))}]) )

  ,tst (Proxy :: Proxy Test.Data.Un ) ( (TypeCon (TypRef (Name "Test.Data.Un")),[ADT {declName = "Test.Data.Un", declNumParameters = 0, declCons = Just (Con {constrName = "Un", constrFields = Right [("un",TypeCon (TypRef (Name "GHC.Types.Bool")))]})},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy Test.Data.D2 ) ( (TypeCon (TypRef (Name "Test.Data.D2")),[ADT {declName = "Test.Data.D2", declNumParameters = 0, declCons = Just (Con {constrName = "D2", constrFields = Left [TypeCon (TypRef (Name "GHC.Types.Bool")),TypeCon (TypRef (Name "Test.Data.N"))]})},ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy Test.Data.D4 ) ( (TypeCon (TypRef (Name "Test.Data.D4")),[ADT {declName = "Test.Data.D4", declNumParameters = 0, declCons = Just (Con {constrName = "D4", constrFields = Left [TypeCon (TypRef (Name "GHC.Types.Bool")),TypeCon (TypRef (Name "Test.Data.N")),TypeCon (TypRef (Name "Test.Data.Unit")),TypeCon (TypRef (Name "Test.Data.N3"))]})},ADT {declName = "Test.Data.N3", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "N1", constrFields = Left []}) (ConTree (Con {constrName = "N2", constrFields = Left []}) (Con {constrName = "N3", constrFields = Left []})))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy Test.Data.A0 ) ( (TypeCon (TypRef (Name "Test.Data.A0")),[ADT {declName = "Test.Data.A0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "A0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.D0")),TypeCon (TypRef (Name "GHC.Types.Bool"))]}) (Con {constrName = "A1", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "Test.Data.Unit"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))]}))},ADT {declName = "Test.Data2.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.D0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "D0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.E0"))]}) (Con {constrName = "D1", constrFields = Left []}))},ADT {declName = "Test.Data.E0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "E0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.D0"))]}) (Con {constrName = "E1", constrFields = Left []}))},ADT {declName = "Test.Data.B0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "B0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.C0"))]}) (Con {constrName = "B1", constrFields = Left []}))},ADT {declName = "Test.Data.C0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "C0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.A0"))]}) (Con {constrName = "C1", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy Test.Data.B0 ) ( (TypeCon (TypRef (Name "Test.Data.B0")),[ADT {declName = "Test.Data.B0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "B0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.C0"))]}) (Con {constrName = "B1", constrFields = Left []}))},ADT {declName = "Test.Data.C0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "C0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.A0"))]}) (Con {constrName = "C1", constrFields = Left []}))},ADT {declName = "Test.Data.A0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "A0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.B0")),TypeCon (TypRef (Name "Test.Data.D0")),TypeCon (TypRef (Name "GHC.Types.Bool"))]}) (Con {constrName = "A1", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "Test.Data.Unit"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))]}))},ADT {declName = "Test.Data2.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Cons2", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data2.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "Nil2", constrFields = Left []}))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.D0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "D0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.E0"))]}) (Con {constrName = "D1", constrFields = Left []}))},ADT {declName = "Test.Data.E0", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "E0", constrFields = Left [TypeCon (TypRef (Name "Test.Data.D0"))]}) (Con {constrName = "E1", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy (Test.Data.Phantom Test.Data.Unit) ) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Phantom"))) (TypeCon (TypRef (Name "Test.Data.Unit"))),[ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data.Phantom", declNumParameters = 1, declCons = Just (Con {constrName = "Phantom", constrFields = Left []})}]) )

  ,tst (Proxy :: Proxy (Test.Data.List GHC.Types.Bool) ) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy (GHC.Base.Maybe Test.Data.Void) ) ( (TypeApp (TypeCon (TypRef (Name "GHC.Base.Maybe"))) (TypeCon (TypRef (Name "Test.Data.Void"))),[ADT {declName = "Test.Data.Void", declNumParameters = 0, declCons = Nothing},ADT {declName = "GHC.Base.Maybe", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Nothing", constrFields = Left []}) (Con {constrName = "Just", constrFields = Left [TypeCon (TypVar 0)]}))}]) )

  ,tst (Proxy :: Proxy (Data.Either.Either GHC.Types.Bool Test.Data.Unit) ) ( (TypeApp (TypeApp (TypeCon (TypRef (Name "Data.Either.Either"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))) (TypeCon (TypRef (Name "Test.Data.Unit"))),[ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Data.Either.Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))}]) )

  ,tst (Proxy :: Proxy (Test.Data.RR Test.Data.Un Test.Data.Unit Test.Data.N) ) ( (TypeApp (TypeApp (TypeApp (TypeCon (TypRef (Name "Test.Data.RR"))) (TypeCon (TypRef (Name "Test.Data.Un")))) (TypeCon (TypRef (Name "Test.Data.Unit")))) (TypeCon (TypRef (Name "Test.Data.N"))),[ADT {declName = "Test.Data.N", declNumParameters = 0, declCons = Just (ConTree (ConTree (Con {constrName = "One", constrFields = Left []}) (Con {constrName = "Two", constrFields = Left []})) (ConTree (Con {constrName = "Three", constrFields = Left []}) (ConTree (Con {constrName = "Four", constrFields = Left []}) (Con {constrName = "Five", constrFields = Left []}))))},ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data.Un", declNumParameters = 0, declCons = Just (Con {constrName = "Un", constrFields = Right [("un",TypeCon (TypRef (Name "GHC.Types.Bool")))]})},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.RR", declNumParameters = 3, declCons = Just (ConTree (Con {constrName = "RN", constrFields = Right [("rna",TypeCon (TypVar 0)),("rnb",TypeCon (TypVar 1)),("rnc",TypeCon (TypVar 2))]}) (ConTree (Con {constrName = "RA", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeApp (TypeApp (TypeCon (TypRef (Name "Test.Data.RR"))) (TypeCon (TypVar 0))) (TypeCon (TypVar 0))) (TypeCon (TypVar 2)),TypeCon (TypVar 1)]}) (Con {constrName = "RAB", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeApp (TypeApp (TypeCon (TypRef (Name "Test.Data.RR"))) (TypeCon (TypVar 2))) (TypeCon (TypVar 1))) (TypeCon (TypVar 0)),TypeCon (TypVar 1)]})))}]) )

  ,tst (Proxy :: Proxy (Data.Either.Either GHC.Types.Bool (Test.Data.List Test.Data.Unit)) ) ( (TypeApp (TypeApp (TypeCon (TypRef (Name "Data.Either.Either"))) (TypeCon (TypRef (Name "GHC.Types.Bool")))) (TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypRef (Name "Test.Data.Unit")))),[ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))},ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Data.Either.Either", declNumParameters = 2, declCons = Just (ConTree (Con {constrName = "Left", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "Right", constrFields = Left [TypeCon (TypVar 1)]}))}]) )

  ,tst (Proxy :: Proxy (Test.Data.Tr (GHC.Base.Maybe Test.Data.Unit)) ) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Tr"))) (TypeApp (TypeCon (TypRef (Name "GHC.Base.Maybe"))) (TypeCon (TypRef (Name "Test.Data.Unit")))),[ADT {declName = "Test.Data.Unit", declNumParameters = 0, declCons = Just (Con {constrName = "Unit", constrFields = Left []})},ADT {declName = "GHC.Base.Maybe", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "Nothing", constrFields = Left []}) (Con {constrName = "Just", constrFields = Left [TypeCon (TypVar 0)]}))},ADT {declName = "Test.Data.Tr", declNumParameters = 1, declCons = Just (Con {constrName = "Tr", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.Forest"))) (TypeCon (TypVar 0))]})},ADT {declName = "Test.Data.Forest", declNumParameters = 1, declCons = Just (Con {constrName = "Forest", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeApp (TypeCon (TypRef (Name "Test.Data.Tr"))) (TypeCon (TypVar 0)))]})},ADT {declName = "Test.Data.List", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "C", constrFields = Left [TypeCon (TypVar 0),TypeApp (TypeCon (TypRef (Name "Test.Data.List"))) (TypeCon (TypVar 0))]}) (Con {constrName = "N", constrFields = Left []}))}]) )

  ,tst (Proxy :: Proxy (Test.Data.Perfect GHC.Types.Bool) ) ( (TypeApp (TypeCon (TypRef (Name "Test.Data.Perfect"))) (TypeCon (TypRef (Name "GHC.Types.Bool"))),[ADT {declName = "GHC.Types.Bool", declNumParameters = 0, declCons = Just (ConTree (Con {constrName = "False", constrFields = Left []}) (Con {constrName = "True", constrFields = Left []}))},ADT {declName = "Test.Data.Perfect", declNumParameters = 1, declCons = Just (ConTree (Con {constrName = "ZeroP", constrFields = Left [TypeCon (TypVar 0)]}) (Con {constrName = "SuccP", constrFields = Left [TypeApp (TypeCon (TypRef (Name "Test.Data.Perfect"))) (TypeApp (TypeCon (TypRef (Name "Test.Data.Fork"))) (TypeCon (TypVar 0)))]}))},ADT {declName = "Test.Data.Fork", declNumParameters = 1, declCons = Just (Con {constrName = "Fork", constrFields = Left [TypeCon (TypVar 0),TypeCon (TypVar 0)]})}]) ) ]
