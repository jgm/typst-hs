{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Typst.Evaluate ( evaluateTypst )
import Typst.Parse ( parseTypst )
import Typst.Types ( repr, Val(VContent) )
import Typst.Pandoc ( contentToPandoc )
import Test.Tasty (defaultMain, TestTree, testGroup, localOption, Timeout(..))
import Test.Tasty.Golden (goldenVsStringDiff, findByExtension)
import System.FilePath (replaceExtension)
import Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Text.Show.Pretty (ppShow)
import qualified Data.ByteString as BS

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  inputs <- findByExtension [".typ"] "test/typ"
  pure $ localOption (Timeout 100000 "100ms") $
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
  if "// Error" `T.isInfixOf` contents ||
     "cycle1.typ" `T.isInfixOf` contents ||
     "cycle2.typ" `T.isInfixOf` contents
     then pure $ fromText "--- skipped ---\n"
     else do
       let parseResult = parseTypst input (testCommand <> contents)
       case parseResult of
         Left e -> pure $ fromText $ T.pack $ show e
         Right parsed -> do
           evalResult <- evaluateTypst BS.readFile input parsed
           let parseOutput = "--- parse tree ---\n" <> T.pack (ppShow parsed) <> "\n"
           case evalResult of
             Left e -> pure $ fromText $
               parseOutput <> T.pack (show e)
             Right cs -> do
               let evalOutput = "--- evaluated ---\n" <> repr (VContent cs) <> "\n"
               pandocResult <- contentToPandoc (const (pure ())) cs
               case pandocResult of
                 Left e -> pure $ fromText $
                   parseOutput <> evalOutput <> T.pack (show e)
                 Right bls -> pure $ fromText $
                   parseOutput <> evalOutput <> "--- pandoc ---\n" <> T.pack (ppShow bls)
