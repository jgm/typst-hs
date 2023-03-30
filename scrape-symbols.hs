{- cabal:
build-depends: base, scalpel, pretty-show, text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Text.HTML.Scalpel
import Control.Applicative
import Text.Show.Pretty
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment

type Sym = (Text, Bool, Text)

getSymbolTable :: String -> IO (Maybe [Sym])
getSymbolTable arg =
  scrapeURLWithConfig
    Config{ decoder = utf8Decoder, manager = Nothing}
    ("https://typst.app/docs/reference/symbols/" <> arg <> "/")
    fetchSymbols
 where
  fetchSymbols = chroots ("li" @: [match codepoint]) pair

  codepoint "data-codepoint" _ = True
  codepoint _ _ = False

  pair = do
    accent <- (== "true") <$> attr "data-accent" "li"
    chroot "button" $ do
      txt <- T.pack <$> text "span"
      name <- T.pack <$> text "code"
      pure (name, accent, txt)

main = do
  args <- getArgs
  case args of
    x:_ -> getSymbolTable x >>= maybe (error "Got Nothing!") pPrint
    [] -> putStrLn $ "Provide either sym or emoji as argument"

