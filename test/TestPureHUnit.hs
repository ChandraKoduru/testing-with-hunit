{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
import Data.Char (toUpper, toLower)
import Control.Monad ((>>), (>>=), when)
import Control.Exception (bracket)
import System.IO (writeFile)
import System.Directory (doesFileExist, removeFile)
import Test.HUnit(Test(..), (~:), (~=?), test,
        assertEqual, runTestTT, assertFailure, assertBool)

-- To run the tests from ghci
-- :runTestTT <test> e.g :runTestTT test1

{--
 -- Test datatype definition
 --
 -- data Test
 -- = TestCase Test.HUnit.Lang.Assertion
 -- | TestList [Test]
 -- | TestLabel String Test
 --}

test1 :: Test
test1 = TestCase $ assertEqual "test upCase" "FOO" (map toUpper "foo")

test1' :: Test
test1' = "test upCase" ~: "FOO" ~=? (map toUpper "foo")

test2 :: Test
test2 = TestList 
  [ "test upCase" ~: "FOO" ~=? (map toUpper "foo")
  , "test downCase" ~: "bar" ~=? (map toLower "BAR")
  ]

test3 :: Test
test3 = TestList 
  [ "test upCase" ~: "FOO" ~=? (map toUpper "foo")
  , "test downCase" ~: "bar" ~=? (map toLower "Bar")
  , "create file" ~: do 
                      let fileName = "testfile.txt"
                      writeFile fileName "AUnit rocks"
                      b <- doesFileExist fileName
                      assertBool "no-file" b
  ]

test4 :: Test
test4 = TestCase $ return "testfile2.txt" >>= 
                   \fileName -> writeFile fileName "HUnit rocks" >>
                   doesFileExist fileName >>= 
                   \b -> assertBool "no file" b

setupFile :: FilePath -> IO FilePath
setupFile fileName =
  doesFileExist fileName >>= 
  \isPresent -> when isPresent 
                  (assertFailure ("setupFile: " ++ fileName ++ " already exists!"))  >>
  return fileName

teardownFile :: FilePath -> IO ()
teardownFile fileName =
  doesFileExist fileName >>=
  \isPresent -> when isPresent
                  $ removeFile fileName

withFile :: FilePath -> (FilePath -> IO c) -> IO c
withFile fileName = bracket (setupFile fileName) teardownFile

test5 :: Test
test5 = "create file" ~: withFile "testfile3.txt" $
                          \fileName -> writeFile fileName "HUnit rocks!" >>
                          doesFileExist fileName >>=
                          \isPresent -> assertBool ("file not avaliable:" ++ fileName) isPresent

allTests :: Test
allTests = test [test1, test1', test2, test3, test4, test5]

main :: IO ()
main = runTestTT allTests >>
       return ()
