{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text.IO as TIO
import Typst (parseTypst, evaluateTypst)
import Typst.Types (repr, Val(..))
import System.IO (hPutStrLn, stderr)
import Text.Show.Pretty (pPrint)
import System.Exit
import System.Environment (getArgs)
import Control.Monad (when, foldM)
import Data.Maybe (fromMaybe)
import Text.Pandoc (writeLaTeX, writeHtml5String, runIOorExplode, WriterOptions(..), def, HTMLMathMethod(..))
import Text.Pandoc.Templates (compileDefaultTemplate)
import Typst.Pandoc (contentToPandoc)
import qualified Data.ByteString as BS

data Opts =
  Opts
  { optShowParse :: Bool
  , optShowEval :: Bool
  , optShowRepr :: Bool
  , optShowPandoc :: Bool
  , optShowLaTeX :: Bool
  , optShowHtml :: Bool
  , optStandalone :: Bool }
  deriving (Show, Eq)

err :: String -> IO a
err msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

parseArgs :: [String] -> IO (Maybe FilePath, Opts)
parseArgs = foldM go (Nothing, Opts False False False False False False False)
 where
   go (f, opts) "--parse" = pure (f, opts{ optShowParse = True })
   go (f, opts) "--eval" = pure (f, opts{ optShowEval = True })
   go (f, opts) "--repr" = pure (f, opts{ optShowRepr = True })
   go (f, opts) "--pandoc" = pure (f, opts{ optShowPandoc = True })
   go (f, opts) "--latex" = pure (f, opts{ optShowLaTeX = True })
   go (f, opts) "--html" = pure (f, opts{ optShowHtml = True })
   go (f, opts) "--standalone" = pure (f, opts{ optStandalone = True })
   go _ ('-':xs) = err $ "Unknown option -" ++ xs
   go (Nothing, opts) f = pure (Just f, opts)
   go _ _ = err $ "Only one file can be specified as input."

main :: IO ()
main = do
  (mbfile, opts) <- getArgs >>= parseArgs
  let showAll = opts == Opts False False False False False False False
  t <- maybe TIO.getContents TIO.readFile mbfile
  case parseTypst (fromMaybe "stdin" mbfile) t of
    Left e -> err $ show e
    Right parseResult -> do
      when (optShowParse opts || showAll) $ do
        when showAll $ putStrLn "--- parse tree ---"
        pPrint parseResult
      result <- evaluateTypst BS.readFile "stdin" parseResult
      case result of
        Left e -> err $ show e
        Right cs -> do
          when (optShowEval opts || showAll) $ do
            when showAll $ putStrLn "--- evaluated ---"
            pPrint cs
          when (optShowRepr opts || showAll) $ do
            when showAll $ putStrLn "--- repr ---"
            TIO.putStrLn $ repr $ VContent cs
          pandocResult <- contentToPandoc (TIO.hPutStrLn stderr) cs
          case pandocResult of
            Left e -> err $ show e
            Right pdoc -> do
                when (optShowPandoc opts || showAll) $ do
                  when showAll $ putStrLn "--- pandoc ---"
                  pPrint pdoc
                when showAll $ putStrLn "--- latex ---"
                when (optShowLaTeX opts || showAll) $ runIOorExplode (do
                  wopts <- if optStandalone opts
                              then do
                                templ <- compileDefaultTemplate "latex"
                                pure $ def{ writerTemplate = Just templ }
                              else pure def
                  writeLaTeX wopts pdoc) >>= TIO.putStrLn
                when showAll $ putStrLn "--- html ---"
                when (optShowHtml opts || showAll) $ runIOorExplode (do
                  wopts <- if optStandalone opts
                              then do
                                templ <- compileDefaultTemplate "html"
                                pure $ def{ writerTemplate = Just templ
                                          , writerHTMLMathMethod = MathML }
                              else pure def{ writerHTMLMathMethod = MathML }
                  writeHtml5String wopts pdoc) >>= TIO.putStrLn
      exitSuccess
