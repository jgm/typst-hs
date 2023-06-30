{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Typst.Methods
  ( getMethod,
    applyPureFunction,
    formatNumber,
  )
where

import Control.Monad (MonadPlus (mplus), foldM, void)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift))
import qualified Data.Array as Array
import qualified Data.Foldable as F
import Data.List (intersperse, sort, sortOn)
import qualified Data.Map as M
import qualified Data.Map.Ordered as OM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser)
import Typst.Module.Standard (standardModule)
import Typst.Regex
  ( RE (..),
    RegexMatch (..),
    extract,
    makeRE,
    match,
    matchAll,
    replaceRegex,
    splitRegex,
  )
import Typst.Types
import Typst.Util (allArgs, makeFunction, namedArg, nthArg)
import Data.Time (toGregorian, dayOfWeek, formatTime, defaultTimeLocale, UTCTime(..))

-- import Debug.Trace

getMethod ::
  MonadFail m =>
  (forall n. Monad n => Val -> MP n ()) ->
  Val ->
  Text ->
  m Val
getMethod updateVal val fld = do
  let methodUnimplemented name =
        fail $
          "Method "
            <> show name
            <> " is not yet implemented"
  let noMethod typename name =
        fail $
          typename
            <> " does not have a method "
            <> show name
  case val of
    VDict m ->
      case fld of
        "len" ->
          pure $ makeFunction $ pure $ VInteger (fromIntegral $ OM.size m)
        "at" ->
          pure $ makeFunction $ do
            key <- nthArg 1
            defval <- namedArg "default" `mplus` pure VNone
            case OM.lookup (Identifier key) m of
              Nothing -> pure defval
              Just v -> pure v
        "insert" ->
          pure $ makeFunction $ do
            key <- nthArg 1
            v <- nthArg 2
            lift $ updateVal $ VDict $ m OM.|> (Identifier key, v)
            pure v
        "keys" ->
          pure $
            makeFunction $
              pure $
                VArray $
                  V.fromList $
                    map (\(Identifier t, _) -> VString t) $
                      OM.assocs m
        "values" ->
          pure $ makeFunction $ pure $ VArray $ V.fromList $ map snd $ OM.assocs m
        "pairs" ->
          pure $ makeFunction $ do
            pure $
              VArray $
                V.fromList $
                  map
                    ( \(Identifier k, v) ->
                        VArray (V.fromList [VString k, v])
                    )
                    (OM.assocs m)
        "remove" ->
          pure $ makeFunction $ do
            key <- nthArg 1
            case OM.lookup (Identifier key) m of
              Nothing -> pure VNone
              Just oldval -> do
                lift $ updateVal $ VDict $ OM.delete (Identifier key) m
                pure oldval
        _ -> case OM.lookup (Identifier fld) m of
          Just x -> pure x
          Nothing -> fail $ show (Identifier fld) <> " not found"
    VColor col ->
      case fld of
        "darken" -> pure $ makeFunction $ do
          (n :: Rational) <- nthArg 1
          pure $ VColor $ case col of
            RGB r g b o -> RGB (r * (1 - n)) (g * (1 - n)) (b * (1 - n)) o
            CMYK c m y k -> CMYK (c * (1 - n)) (m * (1 - n)) (y * (1 - n)) (k * (1 - n))
            Luma x -> Luma (x * (1 - n))
        "lighten" -> pure $ makeFunction $ do
          (n :: Rational) <- nthArg 1
          pure $ VColor $ case col of
            RGB r g b o ->
              RGB
                (r + ((1 - r) * n))
                (g + ((1 - g) * n))
                (b + ((1 - b) * n))
                o
            CMYK c m y k ->
              CMYK
                (c + ((1 - c) * n))
                (m + ((1 - m) * n))
                (y + ((1 - y) * n))
                (k + ((1 - k) * n))
            Luma x -> Luma (x + ((1 - x) * n))
        "negate" -> pure $ makeFunction $ do
          pure $ VColor $ case col of
            RGB r g b o -> RGB (1 - r) (1 - g) (1 - b) o
            CMYK c m y k -> CMYK (1 - c) (1 - m) (1 - y) k
            Luma x -> Luma (1 - x)
        _ -> noMethod "Color" fld
    VString t -> do
      let toPos n =
            if n < 0
              then T.length t + n
              else n
      case fld of
        "len" ->
          pure $ makeFunction $ pure $ VInteger (fromIntegral $ T.length t)
        "first" ->
          if T.null t
            then fail "string is empty"
            else pure $ makeFunction $ pure $ VString $ T.take 1 t
        "last" ->
          if T.null t
            then fail "string is empty"
            else pure $ makeFunction $ pure $ VString $ T.takeEnd 1 t
        "at" ->
          pure $ makeFunction $ do
            n <- toPos <$> nthArg 1
            pure $ VString $ T.take 1 $ T.drop n t
        "slice" ->
          pure $ makeFunction $ do
            start <- toPos <$> nthArg 1
            end <-
              (toPos <$> nthArg 2)
                `mplus` ((+ start) <$> namedArg "count")
                `mplus` pure (T.length t)
            if end < start
              then pure $ VString ""
              else pure $ VString $ T.take (end - start) $ T.drop start t
        "clusters" -> pure $ makeFunction $ do
          -- TODO this isn't right, but we'd need fancier libraries
          -- to get at grapheme clusters
          pure $ VArray $ V.fromList $ map VString $ T.chunksOf 1 t
        "codepoints" -> pure $ makeFunction $ do
          pure $ VArray $ V.fromList $ map VString $ T.chunksOf 1 t
        "contains" -> pure $ makeFunction $ do
          (patt :: RE) <- nthArg 1
          pure $ VBoolean $ match patt t
        "starts-with" -> pure $ makeFunction $ do
          (RE reStr _) <- nthArg 1
          patt <- makeRE ("^" <> reStr)
          pure $ VBoolean $ match patt t
        "ends-with" -> pure $ makeFunction $ do
          (RE reStr _) <- nthArg 1
          patt <- makeRE (reStr <> "$")
          pure $ VBoolean $ match patt t
        "find" -> pure $ makeFunction $ do
          (patt :: RE) <- nthArg 1
          pure $
            let ((_, m, _) :: (Text, Text, Text)) = match patt t
             in VString m
        "position" -> pure $ makeFunction $ do
          (patt :: RE) <- nthArg 1
          pure $
            let ((off, _) :: (Int, Int)) = match patt t
             in VInteger (fromIntegral off)
        "match" -> pure $ makeFunction $ do
          (patt :: RE) <- nthArg 1
          let (pre, whole, (_post :: Text), subs) = match patt t
          if T.null whole
            then pure VNone
            else
              pure $
                VDict $
                  OM.fromList
                    [ ("start", VInteger (fromIntegral $ T.length pre)),
                      ("end", VInteger (fromIntegral $ T.length pre + T.length whole)),
                      ("text", VString whole),
                      ("captures", VArray $ V.fromList $ map VString subs)
                    ]
        "matches" -> pure $ makeFunction $ do
          (patt :: RE) <- nthArg 1
          let matchToDict matchArray =
                case Array.elems matchArray of
                  [] -> VNone
                  (off, len) : subs ->
                    let submatches = map (\(o, l) -> VString $ extract (o, l) t) subs
                     in VDict $
                          OM.fromList
                            [ ("start", VInteger (fromIntegral off)),
                              ("end", VInteger (fromIntegral off + fromIntegral len)),
                              ("text", VString $ extract (off, len) t),
                              ("captures", VArray $ V.fromList submatches)
                            ]
          let matches = map matchToDict $ matchAll patt t
          pure $ VArray $ V.fromList matches
        "replace" -> pure $ makeFunction $ do
          patt :: RE <- nthArg 1
          (replacement :: Val) <- nthArg 2
          mbCount :: Maybe Int <- namedArg "count" `mplus` pure Nothing
          case mbCount of
            Just 0 -> pure $ VString t
            _ ->
              case replacement of
                VString r ->
                  pure $ VString $ replaceRegex patt mbCount (const r) t
                VFunction _ _ f ->
                  pure $
                    VString $
                      replaceRegex
                        patt
                        mbCount
                        ( \(RegexMatch start end txt captures) ->
                            case applyPureFunction
                              f
                              [ VDict $
                                  OM.fromList
                                    [ ("start", VInteger (fromIntegral start)),
                                      ("end", VInteger (fromIntegral end)),
                                      ("text", VString txt),
                                      ("captures", VArray (V.fromList (map VString captures)))
                                    ]
                              ] of
                              Success (VString s) -> s
                              _ -> ""
                        )
                        t
                _ -> fail "replacement must be string or function"
        "trim" -> pure $ makeFunction $ do
          (RE patt _) <- nthArg 1 `mplus` makeRE "[[:space:]]*"
          (repeated :: Bool) <- namedArg "repeat" `mplus` pure True
          (mbAt :: Maybe Val) <- namedArg "at" `mplus` pure Nothing
          let patt' =
                if repeated
                  then "(" <> patt <> ")*"
                  else patt
          patt'' <- case mbAt of
            Just (VAlignment (Just HorizStart) _) -> makeRE $ "^" <> patt'
            Just (VAlignment (Just HorizEnd) _) -> makeRE $ patt' <> "$"
            Nothing -> makeRE $ "(^" <> patt' <> ")|(" <> patt' <> "$)"
            _ -> fail "'at' expected either 'start' or 'end'"
          pure $ VString $ replaceRegex patt'' Nothing (const mempty) t
        "split" -> pure $ makeFunction $ do
          arg <- nthArg 1
          case arg of
            VString "" ->
              pure $ VArray $ V.fromList $ map VString $ "" : T.chunksOf 1 t ++ [""]
            VString patt -> pure $ VArray $ V.fromList $ map VString $ T.splitOn patt t
            VRegex patt ->
              pure $
                VArray $
                  V.fromList $
                    map VString $
                      splitRegex patt t
            _ ->
              -- defaults to split on whitespace
              pure $ VArray $ V.fromList $ map VString $ T.words t
        _ -> noMethod "String" fld
    VCounter key ->
      case fld of
        "display" -> pure $ makeFunction $ do
          mbnum <- M.lookup key . evalCounters <$> lift getState
          maybe (fail "counter not defined") (pure . VInteger) mbnum
        "step" -> pure $ makeFunction $ do
          lift $ updateState $ \st ->
            st {evalCounters = M.adjust (+ 1) key $ evalCounters st}
          pure VNone
        "update" -> pure $ makeFunction $ do
          mbnum <- M.lookup key . evalCounters <$> lift getState
          case mbnum of
            Nothing -> fail "counter not defined"
            Just num -> do
              newval <- nthArg 1
              (newnum :: Integer) <-
                case newval of
                  VFunction _ _ fn ->
                    case applyPureFunction fn [VInteger num] of
                      Failure e -> fail e
                      Success v -> fromVal v
                  _ -> fromVal newval
              lift $ updateState $ \st ->
                st {evalCounters = M.adjust (const newnum) key $ evalCounters st}
              pure VNone
        "at" -> methodUnimplemented fld
        "final" -> methodUnimplemented fld
        _ -> noMethod "Counter" fld
    VContent cs ->
      case fld of
        "func" -> pure $ makeFunction $ do
          case F.toList cs of
            [Elt name _ _] -> lift $ lookupIdentifier name
            [Txt _] -> lift $ lookupIdentifier "text"
            _ -> pure $ makeFunction $ do
              xs <- allArgs
              pure $ VContent $ foldMap valToContent xs
        "has" -> pure $ makeFunction $ do
          f <- nthArg 1
          case F.toList cs of
            [Elt _ _ fields] -> do
              case M.lookup (Identifier f) fields of
                Just _ -> pure $ VBoolean True
                Nothing -> pure $ VBoolean False
            _ | f == "children" -> pure $ VBoolean True
            _ ->
              fail $
                "Content is not a single element: "
                  <> T.unpack (repr (VContent cs))
        "at" -> pure $ makeFunction $ do
          (field :: Text) <- ask >>= getPositionalArg 1 >>= fromVal
          defval <- namedArg "default" `mplus` pure VNone
          case F.toList cs of
            [Elt _ _ fields] ->
              case M.lookup (Identifier field) fields of
                Just v -> pure v
                Nothing -> pure defval
            _ -> pure defval
        "location" -> methodUnimplemented fld
        "text" ->
          case F.toList cs of
            [Txt t] -> pure $ VString t
            [Elt "text" _ [("body", VContent [Txt t])]] -> pure $ VString t
            [Elt _ _ fields]
              | Just x <- M.lookup "text" fields -> pure x
            _ -> fail "Content is not a single text element"
        "fields" -> pure $ makeFunction $ do
          VDict <$>
            case F.toList cs of
              (Elt _ _ fields:_) -> pure $ OM.fromList $ M.toList fields
              _ -> pure OM.empty
        _ ->
          let childrenOrFallback =
                if fld == "children"
                  then
                    pure $
                      VArray $
                        V.fromList $
                          map (\x -> VContent [x]) $
                            F.toList cs
                  else noMethod "Content" fld
           in case cs of
                [Elt _name _ fields] ->
                  maybe childrenOrFallback pure $ M.lookup (Identifier fld) fields
                _ -> childrenOrFallback
    VTermItem t d ->
      case fld of
        "term" -> pure $ VContent t
        "description" -> pure $ VContent d
        _ -> noMethod "TermItem" fld
    VArray v -> do
      let toPos n =
            if n < 0
              then V.length v + n
              else n
      case fld of
        "len" ->
          pure $ makeFunction $ pure $ VInteger (fromIntegral $ V.length v)
        "first" ->
          pure $
            makeFunction $
              if V.null v
                then fail "empty array"
                else pure $ V.head v
        "last" ->
          pure $
            makeFunction $
              if V.null v
                then fail "empty array"
                else pure $ V.last v
        "at" -> pure $ makeFunction $ do
          pos <- toPos <$> nthArg 1
          defval <- namedArg "default" `mplus` pure VNone
          pure $ fromMaybe defval $ v V.!? pos
        "push" -> pure $ makeFunction $ do
          x <- nthArg 1
          lift $ updateVal $ VArray $ V.snoc v x
          pure VNone
        "pop" ->
          pure $
            makeFunction $
              if V.null v
                then fail "empty array"
                else do
                  lift $ updateVal $ VArray $ V.init v
                  pure $ V.last v
        "slice" -> pure $ makeFunction $ do
          start <- toPos <$> nthArg 1
          end <-
            (toPos <$> nthArg 2)
              `mplus` ((+ start) <$> namedArg "count")
              `mplus` pure (V.length v)
          if V.length v < end
            then fail "array contains insufficient elements for slice"
            else
              if end < start
                then pure $ VArray mempty
                else pure $ VArray $ V.slice start (end - start) v
        "split" -> pure $ makeFunction $ do
          spliton <- nthArg 1
          let go v' = case V.break (== spliton) v' of
                (a, b) | V.null b -> if V.null a then [] else [VArray a]
                (a, b) -> VArray a : go (V.drop 1 b)
          pure $ VArray $ V.fromList $ go v
        "insert" -> pure $ makeFunction $ do
          pos <- toPos <$> nthArg 1
          newval <- nthArg 2
          if pos >= V.length v || pos < 0
            then fail "insert position out of bounds in array"
            else do
              lift $ updateVal $ VArray $ V.snoc (V.take pos v) newval <> V.drop pos v
              pure VNone
        "remove" -> pure $ makeFunction $ do
          pos <- toPos <$> nthArg 1
          if pos >= V.length v || pos < 0
            then fail "remove position out of bounds in array"
            else do
              lift $ updateVal $ VArray $ V.take pos v <> V.drop (pos + 1) v
              pure $ fromMaybe VNone $ v V.!? pos
        "contains" -> pure $ makeFunction $ do
          item <- nthArg 1
          pure $ VBoolean $ V.elem item v
        "find" -> pure $ makeFunction $ do
          Function fn <- nthArg 1
          let go Nothing y = do
                res <- lift $ fn Arguments {positional = [y], named = OM.empty}
                case res of
                  VBoolean True -> pure $ Just y
                  VBoolean False -> pure Nothing
                  _ -> fail "function does not return a boolean"
              go (Just z) _ = pure $ Just z
          res <- foldM go Nothing v
          case res of
            Just z -> pure z
            Nothing -> pure VNone
        "position" -> pure $ makeFunction $ do
          Function fn <- nthArg 1
          let go (Left i) y = do
                res <- lift $ fn Arguments {positional = [y], named = OM.empty}
                case res of
                  VBoolean True -> pure $ Right i
                  VBoolean False -> pure $ Left (i + 1)
                  _ -> fail "function does not return a boolean"
              go (Right i) _ = pure $ Right i
          res <- foldM go (Left 0) v
          case res of
            Right i -> pure $ VInteger i
            Left _ -> pure VNone
        "filter" -> pure $ makeFunction $ do
          Function fn <- nthArg 1
          let predicate y = do
                res <- lift $ fn Arguments {positional = [y], named = OM.empty}
                case res of
                  VBoolean True -> pure True
                  VBoolean False -> pure False
                  _ -> fail "function does not return a boolean"
          VArray <$> V.filterM predicate v
        "map" -> pure $ makeFunction $ do
          Function fn <- nthArg 1
          let f y = lift $ fn Arguments {positional = [y], named = OM.empty}
          VArray <$> V.mapM f v
        "flatten" ->
          pure $
            makeFunction $
              pure $
                VArray $
                  V.concat [v' | VArray v' <- V.toList v]
        "enumerate" ->
          pure $
            makeFunction $
              pure $
                VArray $
                  V.zipWith
                    (\x y -> VArray [x, y])
                    (V.map VInteger [0 .. (fromIntegral $ V.length v)])
                    v
        "fold" -> pure $ makeFunction $ do
          (start :: Val) <- nthArg 1
          Function fn <- nthArg 2
          let f acc y = fn Arguments {positional = [acc, y], named = OM.empty}
          lift $ foldM f start $ V.toList v
        "any" -> pure $ makeFunction $ do
          Function fn <- nthArg 1
          let predicate y = do
                res <- lift $ fn Arguments {positional = [y], named = OM.empty}
                case res of
                  VBoolean True -> pure True
                  VBoolean False -> pure False
                  _ -> fail "function not return a boolean"
          (VBoolean . V.any id) <$> mapM predicate v
        "all" -> pure $ makeFunction $ do
          Function fn <- nthArg 1
          let predicate y = do
                res <- lift $ fn Arguments {positional = [y], named = OM.empty}
                case res of
                  VBoolean True -> pure True
                  VBoolean False -> pure False
                  _ -> fail "function not return a boolean"
          (VBoolean . V.all id) <$> mapM predicate v
        "rev" -> pure $ makeFunction $ pure $ VArray $ V.reverse v
        "join" -> pure $ makeFunction $ do
          separator <- nthArg 1
          lastsep <- namedArg "last" `mplus` pure separator
          let xs' = F.toList v
          let xs = case xs' of
                [] -> []
                _ -> intersperse separator (init xs') ++ [lastsep, last xs']
          foldM joinVals VNone xs
        "sorted" -> pure $ makeFunction $ do
          (mbKeyFn :: Maybe Function) <- namedArg "key" `mplus` pure Nothing
          case mbKeyFn of
            Nothing -> pure $ VArray $ V.fromList $ sort $ V.toList v
            Just (Function kf) -> do
              let kf' x = lift $ kf Arguments {positional = [x], named = OM.empty}
              VArray . V.fromList . map fst . sortOn snd
                <$> (mapM (\x -> (x,) <$> kf' x) (V.toList v))
        "zip" -> pure $ makeFunction $ do
          (v' :: V.Vector Val) <- ask >>= getPositionalArg 1
          pure $ VArray $ V.map pairToArray $ V.zip v v'
        "sum" -> pure $ makeFunction $ do
          mbv <- namedArg "default" `mplus` pure Nothing
          case V.uncons v of
            Nothing ->
              maybe
                (fail "sum of empty array with no default value")
                pure
                mbv
            Just (h, rest) ->
              pure $
                fromMaybe VNone $
                  V.foldl
                    ( \mbsum x -> case mbsum of
                        Nothing -> Nothing
                        Just y -> maybePlus y x
                    )
                    (Just h)
                    rest
        "product" -> pure $ makeFunction $ do
          mbv <- namedArg "default" `mplus` pure Nothing
          case V.uncons v of
            Nothing ->
              maybe
                (fail "product of empty array with no default value")
                pure
                mbv
            Just (h, rest) ->
              pure $
                fromMaybe VNone $
                  V.foldl
                    ( \mbsum x -> case mbsum of
                        Nothing -> Nothing
                        Just y -> maybeTimes y x
                    )
                    (Just h)
                    rest
        _ -> noMethod "Array" fld
    VFunction mbName scope (Function f) ->
      case fld of
        "with" -> pure $ makeFunction $ do
          args <- ask
          pure $
            VFunction mbName scope $
              Function $
                \args' -> f (args <> args')
        "where" -> pure $ makeFunction $ do
          args <- ask
          case mbName of
            Nothing -> fail "function is not an element function"
            Just name ->
              pure $
                VSelector $
                  SelectElement name (OM.assocs (named args))
        _ -> noMethod "Function" fld
    VSelector sel ->
      case fld of
        "or" -> pure $ makeFunction $ do
          (other :: Selector) <- nthArg 1
          pure $ VSelector $ SelectOr other sel
        "and" -> pure $ makeFunction $ do
          (other :: Selector) <- nthArg 1
          pure $ VSelector $ SelectAnd other sel
        "before" -> pure $ makeFunction $ do
          (other :: Selector) <- nthArg 1
          pure $ VSelector $ SelectBefore other sel
        "after" -> pure $ makeFunction $ do
          (other :: Selector) <- nthArg 1
          pure $ VSelector $ SelectAfter other sel
        _ -> noMethod "Selector" fld
    VArguments args ->
      case fld of
        "pos" -> pure $ makeFunction $ pure $ VArray $ V.fromList (positional args)
        "named" -> pure $ makeFunction $ pure $ VDict $ named args
        _ -> noMethod "Arguments" fld
    VDateTime mbdate mbtime -> do
      let toSeconds = (floor :: Double -> Integer) . realToFrac
      case fld of
        "year" -> pure $ makeFunction $
          pure $ case toGregorian <$> mbdate of
                   Nothing -> VNone
                   Just (y,_,_) -> VInteger (fromIntegral y)
        "month" -> pure $ makeFunction $
          pure $ case toGregorian <$> mbdate of
                   Nothing -> VNone
                   Just (_,m,_) -> VInteger (fromIntegral m)
        "day" -> pure $ makeFunction $
          pure $ case toGregorian <$> mbdate of
                   Nothing -> VNone
                   Just (_,_,d) -> VInteger (fromIntegral d)
        "weekday" -> pure $ makeFunction $
          pure $ case dayOfWeek <$> mbdate of
                   Nothing -> VNone
                   Just d-> VInteger (fromIntegral $ fromEnum d)
        "hour" -> pure $ makeFunction $
          pure $ case toSeconds <$> mbtime of
            Nothing -> VNone
            Just t -> VInteger $ t `div` 3600
        "minute" -> pure $ makeFunction $
          pure $ case toSeconds <$> mbtime of
            Nothing -> VNone
            Just t -> VInteger $ (t `mod` 3600) `div` 60
        "second" -> pure $ makeFunction $
          pure $ case toSeconds <$> mbtime of
            Nothing -> VNone
            Just t -> VInteger $ t `mod` 60
        "display" -> pure $ makeFunction $ do
          mbfmt <- nthArg 1 `mplus` pure Nothing
          mbformat <- case mbfmt of
            Nothing -> pure Nothing
            Just fmt ->
              case toTimeFormat <$> parseDisplayFormat fmt of
                Left e -> fail $ "Could not parse display format: " <> show e
                Right f -> pure $ Just f
          pure $ VString $ T.pack $
            case (mbdate, mbtime) of
              (Nothing, Just t) -> formatTime defaultTimeLocale (fromMaybe "%X" mbformat) t
              (Just d, Nothing) -> formatTime defaultTimeLocale (fromMaybe "%F" mbformat) d
              (Nothing, Nothing) -> ""
              (Just d, Just t) -> formatTime defaultTimeLocale (fromMaybe "%X %F" mbformat)
                                    (UTCTime d t)
        _ -> noMethod "DateTime" fld
    _ -> noMethod (drop 1 $ takeWhile (/= ' ') $ show val) fld

pairToArray :: (Val, Val) -> Val
pairToArray (x, y) = VArray $ V.fromList [x, y]

applyPureFunction :: Function -> [Val] -> Attempt Val
applyPureFunction (Function f) vals =
  let args = Arguments vals OM.empty
   in case runParserT (f args) initialEvalState "" [] of
        Failure s -> Failure s
        Success (Left s) -> Failure $ show s
        Success (Right v) -> Success v

initialEvalState :: MonadFail m => EvalState m
initialEvalState =
  emptyEvalState { evalIdentifiers = [(BlockScope, standardModule)] }

formatNumber :: Text -> Int -> Text
formatNumber t n = F.foldMap go $ T.unpack t
  where
    go '1' | n >= 0 = T.pack (show n)
    go 'a' | n >= 1 = T.singleton $ cycle ['a' .. 'z'] !! (n - 1 `mod` 26)
    go 'A' | n >= 1 = T.singleton $ cycle ['A' .. 'Z'] !! (n - 1 `mod` 26)
    go 'i' | n >= 1 = T.toLower $ toRomanNumeral n
    go 'I' | n >= 1 = toRomanNumeral n
    go 'い' | n >= 1 = T.pack (show n) -- TODO
    go 'イ' | n >= 1 = T.pack (show n) -- TODO
    go 'א' | n >= 1 = T.pack (show n) -- TODO
    go '*'
      | n >= 1 =
          T.singleton $ cycle ['*', '†', '‡', '§', '¶', '‖'] !! (n - 1 `mod` 6)
      | otherwise = "-"
    go c = T.singleton c

toRomanNumeral :: Int -> T.Text
toRomanNumeral x
  | x >= 4000 || x < 0 = "?"
  | x >= 1000 = "M" <> toRomanNumeral (x - 1000)
  | x >= 900 = "CM" <> toRomanNumeral (x - 900)
  | x >= 500 = "D" <> toRomanNumeral (x - 500)
  | x >= 400 = "CD" <> toRomanNumeral (x - 400)
  | x >= 100 = "C" <> toRomanNumeral (x - 100)
  | x >= 90 = "XC" <> toRomanNumeral (x - 90)
  | x >= 50 = "L" <> toRomanNumeral (x - 50)
  | x >= 40 = "XL" <> toRomanNumeral (x - 40)
  | x >= 10 = "X" <> toRomanNumeral (x - 10)
  | x == 9 = "IX"
  | x >= 5 = "V" <> toRomanNumeral (x - 5)
  | x == 4 = "IV"
  | x >= 1 = "I" <> toRomanNumeral (x - 1)
  | otherwise = ""

-- parser for DateTime display format

data FormatPart =
    Literal String
  | Variable String [(String, String)]
  deriving Show

parseDisplayFormat :: String -> Either ParseError [FormatPart]
parseDisplayFormat = parse (many pFormatPart <* eof) ""

pFormatPart :: Parser FormatPart
pFormatPart = pVariable <|> pLiteral

pLiteral :: Parser FormatPart
pLiteral = Literal <$> many1 (satisfy (/='['))

pVariable :: Parser FormatPart
pVariable = do
  void $ char '['
  name <- many1 letter
  spaces
  modifiers <- many pModifier
  void $ char ']'
  pure $ Variable name modifiers

pModifier :: Parser (String, String)
pModifier = do
  name <- many1 letter
  void $ char ':'
  spaces
  val <- many1 alphaNum
  spaces
  pure (name, val)

-- convert formatparts into Data.Time format string

toTimeFormat :: [FormatPart] -> String
toTimeFormat = concatMap toTimeFormatPart

toTimeFormatPart :: FormatPart -> String
toTimeFormatPart (Literal s) = foldr esc "" s
 where
  esc '%' = ("%%" ++)
  esc '\t' = ("%t" ++)
  esc '\n' = ("%n" ++)
  esc c = (c:)
toTimeFormatPart (Variable "year" mods) =
  withPadding mods $
    case lookup "repr" mods of
       Just "last_two" -> "y"
       _ -> "Y"
toTimeFormatPart (Variable "month" mods) =
  withPadding mods $
    case lookup "repr" mods of
       Just "numerical" -> "%m"
       Just "long" -> "b"
       Just "short" -> "h"
       _ -> "m"
toTimeFormatPart (Variable "day" mods) =
  case lookup "padding" mods of
    Just "space" -> "%e"
    Just "zero" -> "%d"
    _ -> "%e"
toTimeFormatPart (Variable "week_number" mods) =
  withPadding mods $
    case lookup "repr" mods of
      Just "ISO" -> "V"
      Just "sunday" -> "U"
      Just "monday" -> "W"
      _ -> "V"
toTimeFormatPart (Variable "weekday" mods) =
  withPadding mods $
     case lookup "repr" mods of
      Just "long" -> "A"
      Just "short" -> "a"
      Just "sunday" -> "w"
      Just "monday" -> "u"
      _ -> ""
toTimeFormatPart (Variable "hour" mods) =
  case lookup "hour" mods of
    Just "24" | lookup "padding" mods == Just "zero" -> "%H"
              | otherwise -> "%k"
    Just "12" | lookup "padding" mods == Just "zero" -> "%I"
              | otherwise -> "%l"
    _ -> "%k"
toTimeFormatPart (Variable "period" mods) =
  case lookup "case" mods of
    Just "lower" -> "%P"
    _ -> "%p"
toTimeFormatPart (Variable "minute" _) = "%M"
toTimeFormatPart (Variable "second" _) = "%S"
toTimeFormatPart _ = "?"

withPadding :: [(String, String)] -> String -> String
withPadding mods s = '%' :
  case lookup "padding" mods of
       Just "zero" -> '0' : s
       Just "space" -> '_' : s
       _ -> s
