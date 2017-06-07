-- {-# OPTIONS_GHC -F -pgmF doctest-discover -optF doctest.json #-}
-- doctest-discover is broken so we use this instead
module Main where
import Test.DocTest
import System.FilePath.Find
--import System.FilePath.Glob (namesMatching)
main :: IO ()
-- main = doctest ["src"]
main = do
  -- files <- namesMatching "src/**/*.hs"
  files <- find always (extension ==? ".hs") "src"   --  ||? extension ==? ".lhs"
  -- writeFile "/tmp/ok" $ show files
  doctest files

-- docTest files = doctest $ ["src"] ++ files
