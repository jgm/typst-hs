{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.IO as TIO
import System.FilePath (replaceExtension)
import Test.Tasty (TestTree, Timeout (..), defaultMain, localOption, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Text.Show.Pretty (ppShow)
import Typst.Evaluate (evaluateTypst)
import Typst.Parse (parseTypst)
import Typst.Types (Val (VContent), repr, Operations(..))
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

main :: IO ()
main = defaultMain =<< goldenTests

operations :: Operations IO
operations = Operations
  { loadBytes = BS.readFile
  , currentUTCTime = getCurrentTime
  , lookupEnvVar = lookupEnv
  , checkExistence = doesFileExist
  }

goldenTests :: IO TestTree
goldenTests = do
  inputs <- findByExtension [".typ"] "test/typ"
  pure $
    localOption (Timeout 1000000 "1s") $
      testGroup "golden tests" (map runTest inputs)

runTest :: FilePath -> TestTree
runTest input =
  goldenVsStringDiff
    input
    (\ref new -> ["diff", "-u", ref, new])
    ("test/out" <> drop 8 (replaceExtension input ".out"))
    (writeTest input)

writeTest :: FilePath -> IO BL.ByteString
writeTest input = do
  let fromText = BL.fromStrict . TE.encodeUtf8 . (<> "\n")
  let testCommand =
        "#let test = (x,y) => { if x == y [✅] else [❌(#repr(x) /= #repr(y))] }\n"
  contents <- TIO.readFile input
  if "// Error"
    `T.isInfixOf` contents
    || "cycle1.typ"
    `T.isInfixOf` contents
    || "cycle2.typ"
    `T.isInfixOf` contents
    then pure $ fromText "--- skipped ---\n"
    else do
      let parseResult = parseTypst input (testCommand <> contents)
      case parseResult of
        Left e -> pure $ fromText $ T.pack $ show e
        Right parsed -> do
          evalResult <- evaluateTypst operations input parsed
          let parseOutput = "--- parse tree ---\n" <> T.pack (ppShow parsed) <> "\n"
          case evalResult of
            Left e ->
              pure $
                fromText $
                  parseOutput <> T.pack (show e)
            Right cs -> do
              let evalOutput = "--- evaluated ---\n" <> repr (VContent cs)
              pure $ fromText $ parseOutput <> evalOutput
