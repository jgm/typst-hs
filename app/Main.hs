{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Text.Show.Pretty (pPrint)
import Typst (evaluateTypst, parseTypst)
import Typst.Types (Val (..), repr, Operations(..))
import Data.Time (getCurrentTime)
import qualified Data.ByteString as B

data Opts = Opts
  { optShowParse :: Bool,
    optShowEval :: Bool,
    optShowRepr :: Bool,
    optShowLaTeX :: Bool,
    optShowHtml :: Bool,
    optStandalone :: Bool,
    optTimeout :: Maybe Int,
    optFile :: Maybe FilePath
  }
  deriving (Show, Eq)

err :: String -> IO a
err msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> switch (long "parse" <> help "Show parse tree")
    <*> switch (long "eval" <> help "Show evaluated result")
    <*> switch (long "repr" <> help "Show repr output")
    <*> switch (long "latex" <> help "Show LaTeX output")
    <*> switch (long "html" <> help "Show HTML output")
    <*> switch (long "standalone" <> help "Standalone mode")
    <*> optional (option auto (long "timeout" <> metavar "MS" <> help "Timeout in milliseconds"))
    <*> optional (argument str (metavar "FILE" <> help "Input file (reads stdin if omitted)"))

operations :: Operations IO
operations = Operations
  { loadBytes = BS.readFile
  , currentUTCTime = getCurrentTime
  , lookupEnvVar = lookupEnv
  , checkExistence = doesFileExist
  }

main :: IO ()
main =
  () <$ do
    opts <- execParser (info (optsParser <**> helper)
              (fullDesc <> progDesc "Parse and evaluate Typst documents"))
    let mbfile = optFile opts
    let showAll = case opts of
          Opts False False False False False False _ _ -> True
          _ -> False
    ( case optTimeout opts of
        Nothing -> fmap Just
        Just ms -> timeout (ms * 1000)
      )
      $ do
        bs <- maybe B.getContents B.readFile mbfile
        let t = TE.decodeUtf8 bs
        case parseTypst (fromMaybe "stdin" mbfile) t of
          Left e -> err $ show e
          Right parseResult -> do
            when (optShowParse opts || showAll) $ do
              when showAll $ putStrLn "--- parse tree ---"
              pPrint parseResult
            let inputs = [] -- TODO
            result <- evaluateTypst operations inputs "stdin" parseResult
            case result of
              Left e -> err $ show e
              Right c -> do
                when (optShowEval opts || showAll) $ do
                  when showAll $ putStrLn "--- evaluated ---"
                  pPrint c
                when (optShowRepr opts || showAll) $ do
                  when showAll $ putStrLn "--- repr ---"
                  TIO.putStrLn $ repr $ VContent [c]
            exitSuccess
