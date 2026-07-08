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
import qualified Data.Map.Ordered as OM
import qualified Data.Map as M
import Data.Time (fromGregorian, secondsToDiffTime)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.ByteString as B
import Typst.Types
import Typst.Util (makeFunction, makeFunctionWithScope, namedArg, nthArg, allArgs)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Typst.Regex (makeRE)
import Data.List (genericTake)
import Control.Monad.Reader (asks)
import Control.Monad (mplus)
import Data.Char (ord, chr, isDigit, isAsciiLower, isAsciiUpper)

getConstructor :: ValType -> Maybe Val
getConstructor typ =
  case typ of
    TFloat -> Just $ makeFunction $ VFloat <$> nthArg 1
    TInteger -> Just $ makeFunctionWithScope
      (do
        val <- nthArg 1
        (base :: Integer) <- namedArg "base" 10
        case val of
          _ | base == 10 -> VInteger <$> fromVal val
          VString s
            | base >= 2 && base <= 36 ->
                maybe (fail "invalid digits for the given base")
                      (pure . VInteger)
                      (parseInBase base s)
            | otherwise -> fail "base must be between 2 and 36"
          _ -> fail "base is only supported when parsing strings")
      [ ("min", VInteger (-9223372036854775808)),  -- i64 bounds, as in typst
        ("max", VInteger 9223372036854775807)
      ]
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
    TDateTime -> Just $ makeFunction $ do
      mbyr <- namedArg "year" Nothing
      mbmo <- namedArg "month" Nothing
      mbda <- namedArg "day" Nothing
      mbhr <- namedArg "hour" Nothing
      mbmn <- namedArg "minute" Nothing
      mbsc <- namedArg "second" Nothing
      let mbday = case (mbyr, mbmo, mbda) of
                     (Nothing, _, _) -> Nothing
                     (Just yr, _, _) -> Just $
                       fromGregorian yr (fromMaybe 1 mbmo) (fromMaybe 1 mbda)
      let mbdifftime = case (mbhr, mbmn, mbsc) of
                              (Nothing, _, _) -> Nothing
                              (Just hr, _, _) -> Just $ secondsToDiffTime $
                                (hr * 60 * 60) + maybe 0 (* 60) mbmn +
                                  fromMaybe 0 mbsc
      pure $ VDateTime mbday mbdifftime
    TDict -> Just $ makeFunction $ do
      a <- nthArg 1
      case a of
        VModule _ m -> pure $ VDict $ OM.fromList $ M.toList m
        _ -> fail "dictionary constructor requires a module as argument"
    TBytes -> Just $ makeFunction $ do
      x <- nthArg 1
      let extractWord8 (VInteger w) = Just $ fromIntegral w
          extractWord8 _ = Nothing
      case x of
        VString s -> pure $ VBytes $ TE.encodeUtf8 s
        VArray xs -> pure $ VBytes $ B.pack $ mapMaybe extractWord8 $ V.toList xs
        _ -> fail "bytes constructor requires a string or array as argument"
    TArguments -> Nothing
    -- TODO https://typst.app/docs/reference/foundations/arguments/
    TSelector -> Nothing
    -- TODO https://typst.app/docs/reference/foundations/selector/
    TCounter -> Nothing
    -- TODO https://typst.app/docs/reference/introspection/counter/
    _ -> Nothing


-- | Parse an integer (with optional sign) in the given base (2-36).
parseInBase :: Integer -> Text -> Maybe Integer
parseInBase base t =
  case T.uncons t of
    Just ('-', rest) -> negate <$> go rest
    Just ('+', rest) -> go rest
    _ -> go t
  where
    go s
      | T.null s = Nothing
      | otherwise = T.foldl' step (Just 0) s
    step macc c = do
      acc <- macc
      d <- digitVal c
      if d < base
        then Just (acc * base + d)
        else Nothing
    digitVal c
      | isDigit c = Just $ fromIntegral (ord c - ord '0')
      | isAsciiLower c = Just $ fromIntegral (ord c - ord 'a' + 10)
      | isAsciiUpper c = Just $ fromIntegral (ord c - ord 'A' + 10)
      | otherwise = Nothing

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
