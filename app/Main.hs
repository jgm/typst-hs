{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (foldM, when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Directory (doesFileExist, getXdgDirectory, getHomeDirectory)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Text.Show.Pretty (pPrint)
import Typst (evaluateTypst, parseTypst)
import Typst.Types (Val (..), repr, Operations(..))
import Data.Time (getCurrentTime)

data Opts = Opts
  { optShowParse :: Bool,
    optShowEval :: Bool,
    optShowRepr :: Bool,
    optShowLaTeX :: Bool,
    optShowHtml :: Bool,
    optStandalone :: Bool,
    optTimeout :: Maybe (Maybe Int)
  }
  deriving (Show, Eq)

err :: String -> IO a
err msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

parseArgs :: [String] -> IO (Maybe FilePath, Opts)
parseArgs = foldM go (Nothing, Opts False False False False False False Nothing)
  where
    go (f, opts) "--parse" = pure (f, opts {optShowParse = True})
    go (f, opts) "--eval" = pure (f, opts {optShowEval = True})
    go (f, opts) "--repr" = pure (f, opts {optShowRepr = True})
    go (f, opts) "--latex" = pure (f, opts {optShowLaTeX = True})
    go (f, opts) "--html" = pure (f, opts {optShowHtml = True})
    go (f, opts) "--standalone" = pure (f, opts {optStandalone = True})
    go (f, opts) "--timeout" = pure (f, opts {optTimeout = Just Nothing })
    go (f, opts) x
      | optTimeout opts == Just Nothing =
          pure (f, opts {optTimeout = Just (readMaybe x) })
    go _ ('-' : xs) = err $ "Unknown option -" ++ xs
    go (Nothing, opts) f = pure (Just f, opts)
    go _ _ = err $ "Only one file can be specified as input."

operations :: Operations IO
operations = Operations
  { loadBytes = BS.readFile
  , currentUTCTime = getCurrentTime
  , getXdgDir = getXdgDirectory
  , getHomeDir = getHomeDirectory
  , checkExistence = doesFileExist
  }

main :: IO ()
main =
  () <$ do
    (mbfile, opts) <- getArgs >>= parseArgs
    let showAll = case opts of
          Opts False False False False False False _ -> True
          _ -> False
    ( case optTimeout opts of
        Nothing -> fmap Just
        Just Nothing -> timeout 1000
        Just (Just ms) -> timeout (ms * 1000)
      )
      $ do
        t <- maybe TIO.getContents TIO.readFile mbfile
        case parseTypst (fromMaybe "stdin" mbfile) t of
          Left e -> err $ show e
          Right parseResult -> do
            when (optShowParse opts || showAll) $ do
              when showAll $ putStrLn "--- parse tree ---"
              pPrint parseResult
            result <- evaluateTypst operations "stdin" parseResult
            case result of
              Left e -> err $ show e
              Right cs -> do
                when (optShowEval opts || showAll) $ do
                  when showAll $ putStrLn "--- evaluated ---"
                  pPrint cs
                when (optShowRepr opts || showAll) $ do
                  when showAll $ putStrLn "--- repr ---"
                  TIO.putStrLn $ repr $ VContent cs
            exitSuccess
