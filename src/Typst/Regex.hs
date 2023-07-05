{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typst.Regex
  ( RE (..),
    RegexMatch (..),
    replaceRegex,
    splitRegex,
    makeLiteralRE,
    makeRE,
    match,
    matchAll,
    -- re-export
    extract,
  )
where

import qualified Data.Array as Array
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Text.Regex.TDFA (Regex, extract)
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.TDFA.Text as TDFA

-- import Debug.Trace

data RE = RE !Text !Regex
  deriving (Typeable)

instance Eq RE where
  RE t1 _ == RE t2 _ = t1 == t2

instance Ord RE where
  compare (RE t1 _) (RE t2 _) = compare t1 t2

instance Show RE where
  show (RE t _) = "/" <> T.unpack t <> "/"

data RegexMatch = RegexMatch
  { matchStart :: Int,
    matchEnd :: Int,
    matchText :: Text,
    matchCaptures :: [Text]
  }
  deriving (Eq, Ord, Typeable)

replaceRegex :: RE -> Maybe Int -> (RegexMatch -> Text) -> Text -> Text
replaceRegex (RE _ re) mbCount replaceFn strIn =
  let matches = maybe id take mbCount $ TDFA.matchAll re strIn
      getCaptures m =
        map
          (\(off, len) -> extract (off, len) strIn)
          (drop 1 (Array.elems m))
      go i [] = T.drop i strIn
      go i (m : rest) =
        seq i $
          let (off, len) = m Array.! 0
           in ( if off > i
                  then slice i (off - i) strIn
                  else mempty
              )
                <> replaceFn
                  RegexMatch
                    { matchStart = off,
                      matchEnd = off + len,
                      matchText = extract (off, len) strIn,
                      matchCaptures = getCaptures m
                    }
                <> go (off + len) rest
      slice pos len = T.take len . T.drop pos
   in go 0 matches

makeRE :: MonadFail m => Text -> m RE
makeRE t =
  RE t'
    <$> either
      fail
      pure
      (TDFA.compile compopts TDFA.defaultExecOpt t')
  where
    (caseSensitive, t') =
      if "(?i)" `T.isPrefixOf` t
        then (False, T.pack . go . T.unpack $ T.drop 4 t)
        else (True, T.pack . go . T.unpack $ t)
    compopts = TDFA.defaultCompOpt {TDFA.caseSensitive = caseSensitive}
    -- handle things not supported in TFFA (posix) regexes, e.g. \d \w \s, +, ?
    go [] = []
    go ('?' : cs) = "{0,1}" ++ go cs
    go ('+' : cs) = "{1,}" ++ go cs
    go ('\\' : c : cs)
      | c == 'd' = "[[:digit:]]" ++ go cs
      | c == 'D' = "[^[:digit:]]" ++ go cs
      | c == 's' = "[[:space:]]" ++ go cs
      | c == 'S' = "[^[:space:]]" ++ go cs
      | c == 'w' = "[[:word:]]" ++ go cs
      | c == 'W' = "[^[:word:]]" ++ go cs
      | otherwise = '\\' : c : go cs
    go (c : cs) = c : go cs

match :: TDFA.RegexContext Regex source target => RE -> source -> target
match (RE _ re) t = TDFA.match re t

matchAll :: TDFA.RegexLike Regex source => RE -> source -> [TDFA.MatchArray]
matchAll (RE _ re) t = TDFA.matchAll re t

makeLiteralRE :: MonadFail m => Text -> m RE
makeLiteralRE t
  | T.null t = makeRE ".{0,0}" -- experimentally behaves as typst does
  | otherwise = makeRE $ T.foldl go mempty t
  where
    go acc c = if isSpecial c then acc <> T.pack ['\\', c] else T.snoc acc c
    isSpecial c = c `elem` (".*?+(){}[]|\\^$" :: [Char])

-- from regex-compat but for Text
splitRegex :: RE -> Text -> [Text]
splitRegex (RE _ delim) strIn =
  let matches = map (Array.! 0) (TDFA.matchAll delim strIn)
      go _i str [] = str : []
      go i str ((off, len) : rest) =
        let i' = off + len
            firstline = T.take (off - i) str
            remainder = T.drop (i' - i) str
         in seq i' $
              if T.null remainder
                then [firstline, ""]
                else firstline : go i' remainder rest
   in go 0 strIn matches
