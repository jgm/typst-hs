{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Typst.Constructors
  ( getConstructor
  )
where

import qualified Data.Vector as V
import Typst.Types
import Typst.Util (makeFunction, makeFunctionWithScope, namedArg, nthArg, allArgs)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Typst.Regex (makeRE)
import Data.List (genericTake)
import Control.Monad.Reader (asks)
import Control.Monad (mplus)
import Data.Char (ord, chr)

getConstructor :: ValType -> Maybe Val
getConstructor typ =
  case typ of
    TFloat -> Just $ makeFunction $ VFloat <$> nthArg 1
    TInteger -> Just $ makeFunction $ VInteger <$> nthArg 1
    TRegex -> Just $ makeFunction $ VRegex <$> (nthArg 1 >>= makeRE)
    TVersion -> Just $ makeFunction $ VVersion <$> (asks positional >>= mapM fromVal)
    TString -> Just $ makeFunctionWithScope
      (do
        val <- nthArg 1
        base <- namedArg "base" (10 :: Integer)
        let digitVector :: V.Vector Char
            digitVector = V.fromList $ ['0'..'9'] ++ ['A'..'Z']
        let renderDigit n = digitVector V.!? (fromIntegral n)
        VString <$>
          case val of
            VInteger n | base /= 10
              -> case mDigits base n of
                   Nothing -> fail "Could not convert number to base"
                   Just ds -> maybe
                     (fail "Could not convert number to base")
                     (pure . T.pack)
                     (mapM renderDigit ds)
            _ -> fromVal val `mplus` pure (repr val))
      [ ( "to-unicode",
           makeFunction $ do
             (val :: Text) <- nthArg 1
             case T.uncons val of
               Just (c, t) | T.null t ->
                 pure $ VInteger $ fromIntegral $ ord c
               _ -> fail "to-unicode expects a single character" )
      , ( "from-unicode",
           makeFunction $ do
             (val :: Int) <- nthArg 1
             pure $ VString $ T.pack [chr val] )
      ]
    TLabel -> Just $ makeFunction $ VLabel <$> nthArg 1
    TSymbol -> Just $ makeFunction $ do
        (t :: Text) <- nthArg 1
        vs <- drop 1 <$> allArgs
        variants <-
          mapM
            ( \case
                VArray [VString k, VString v] ->
                  pure (Set.fromList (T.split (== '.') k), v)
                _ -> fail "wrong type in symbol arguments"
            )
            vs
        pure $ VSymbol $ Symbol t False variants
    TDateTime -> Nothing -- TODO https://typst.app/docs/reference/foundations/datetime/
    TDict -> Nothing -- TODO https://typst.app/docs/reference/foundations/dictionary/
    TArguments -> Nothing -- TODO https://typst.app/docs/reference/foundations/arguments/
    TSelector -> Nothing -- TODO https://typst.app/docs/reference/foundations/selector/
    TCounter -> Nothing -- TODO https://typst.app/docs/reference/introspection/counter/
    _ -> Nothing


-- mDigitsRev, mDigits from the unmaintained digits package
-- https://hackage.haskell.org/package/digits-0.3.1
-- (c) 2009-2016 Henry Bucklow, Charlie Harvey -- BSD-3-Clause license.
mDigitsRev :: Integral n
    => n         -- ^ The base to use.
    -> n         -- ^ The number to convert to digit form.
    -> Maybe [n] -- ^ Nothing or Just the digits of the number in list form, in reverse.
mDigitsRev base i = if base < 1
                    then Nothing -- We do not support zero or negative bases
                    else Just $ dr base i
    where
      dr _ 0 = []
      dr b x = case base of
                1 -> genericTake x $ repeat 1
                _ -> let (rest, lastDigit) = quotRem x b
                     in lastDigit : dr b rest

-- | Returns the digits of a positive integer as a Maybe list.
--   or Nothing if a zero or negative base is given
mDigits :: Integral n
    => n -- ^ The base to use.
    -> n -- ^ The number to convert to digit form.
    -> Maybe [n] -- ^ Nothing or Just the digits of the number in list form
mDigits base i = reverse <$> mDigitsRev base i
