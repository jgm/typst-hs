{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typst.Module.Standard
  ( standardModule,
    symModule,
    sysModule,
    loadFileText,
    applyPureFunction
  )
where

import Data.Char (ord, chr)
import Control.Applicative ((<|>))
import Control.Monad (mplus, unless)
import Control.Monad.Reader (lift)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Map as M
import qualified Data.Map.Ordered as OM
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Text.Parsec (getPosition, getState, updateState, runParserT)
import Text.Read (readMaybe)
import qualified Text.XML as XML
import qualified Toml
import Typst.Emoji (typstEmojis)
import Typst.Module.Calc (calcModule)
import Typst.Module.Math (mathModule)
import Typst.Regex (makeRE)
import Typst.Symbols (typstSymbols)
import Typst.Types
import Typst.Util
import Data.Time (UTCTime(..))
import Data.Time.Calendar (fromGregorianValid)
import Data.Time.Clock (secondsToDiffTime)
import Data.Digits (mDigits)

standardModule :: M.Map Identifier Val
standardModule =
  M.fromList $
    [ ("math", VModule "math" mathModule),
      ("sym", VModule "sym" symModule),
      ("sys", VModule "sys" sysModule),
      ("emoji", VModule "emoji" emojiModule),
      ("calc", VModule "calc" calcModule)
    ]
      ++ colors
      ++ directions
      ++ alignments
      ++ textual
      ++ layout
      ++ visualize
      ++ meta
      ++ foundations
      ++ construct
      ++ time
      ++ dataLoading

sysModule :: M.Map Identifier Val
sysModule =
  M.fromList [ makeElement (Just "sys") "version" [] ]

symModule :: M.Map Identifier Val
symModule = M.map VSymbol $ makeSymbolMap typstSymbols

emojiModule :: M.Map Identifier Val
emojiModule = M.map VSymbol $ makeSymbolMap typstEmojis

textual :: [(Identifier, Val)]
textual =
  [ makeElement
      Nothing
      "text"
      [ ("color", One TColor),
        ("size", One TLength),
        ("body", One (TContent :|: TString :|: TSymbol))
      ],
    makeElement Nothing "emph" [("body", One TContent)],
    makeElement Nothing "linebreak" [],
    makeElement Nothing "strong" [("body", One TContent)],
    makeElement Nothing "sub" [("body", One TContent)],
    makeElement Nothing "super" [("body", One TContent)],
    makeElement Nothing "strike" [("body", One TContent)],
    makeElement Nothing "smallcaps" [("body", One TContent)],
    makeElement Nothing "underline" [("body", One TContent)],
    makeElement Nothing "overline" [("body", One TContent)],
    makeElement Nothing "highlight" [("body", One TContent)],
    makeElement Nothing "raw" [("text", One TString)],
    makeElement Nothing "smartquote" [],
    makeElement Nothing "lower" [("text", One (TString :|: TContent))],
    ( "lower",
      makeFunction $ do
        val <- nthArg 1
        case val of
          VString t -> pure $ VString $ T.toLower t
          VContent cs -> do
            pos <- lift getPosition
            pure $ VContent . Seq.singleton $ Elt "lower" (Just pos) [("text", VContent cs)]
          _ -> fail "argument must be string or content"
    ),
    ( "upper",
      makeFunction $ do
        val <- nthArg 1
        case val of
          VString t -> pure $ VString $ T.toUpper t
          VContent cs -> do
            pos <- lift getPosition
            pure $ VContent . Seq.singleton $ Elt "upper" (Just pos) [("text", VContent cs)]
          _ -> fail "argument must be string or content"
    )
  ]

layout :: [(Identifier, Val)]
layout =
  [ makeElement
      Nothing
      "align"
      [ ("alignment", One TAlignment),
        ("body", One TContent)
      ],
    makeElement Nothing "block" [("body", One TContent)],
    makeElement Nothing "box" [("body", One TContent)],
    makeElement Nothing "colbreak" [],
    makeElement Nothing "columns" [("count", One TInteger), ("body", One TContent)],
    makeElement Nothing "grid" [("children", Many TContent)],
    makeElement Nothing "h" [("amount", One (TLength :|: TRatio :|: TFraction))],
    makeElement Nothing "v" [("amount", One (TLength :|: TRatio :|: TFraction))],
    makeElement Nothing "hide" [("body", One TContent)],
    makeElementWithScope
      Nothing
      "enum"
      [("children", Many TContent)]
      [ makeElement
          (Just "enum")
          "item"
          [ ("number", One (TInteger :|: TNone)),
            ("body", One TContent)
          ]
      ],
    makeElementWithScope
      Nothing
      "list"
      [("children", Many TContent)]
      [makeElement (Just "list") "item" [("body", One TContent)]],
    -- for "measure" see below
    makeElement Nothing "move" [("body", One TContent)],
    -- the fact that pad can take a positional param for a length (= rest) is undocumented!
    makeElement Nothing "pad" [("rest", One (TLength :|: TRatio :|: TNone)), ("body", One TContent)],
    makeElement Nothing "page" [("body", One TContent)],
    makeElement Nothing "pagebreak" [],
    makeElement Nothing "par" [("body", One TContent)],
    makeElement Nothing "parbreak" [],
    makeElement Nothing "place" [("alignment", One (TAlignment :|: TNone)), ("body", One TContent)],
    makeElement Nothing "repeat" [("body", One TContent)],
    makeElement Nothing "rotate" [("angle", One TAngle), ("body", One TContent)],
    -- the fact that scale can take a positional factor is undocumented!
    makeElement Nothing "scale" [("factor", One (TRatio :|: TNone)), ("body", One TContent)],
    makeElement
      Nothing
      "stack"
      [("children", Many (TLength :|: TRatio :|: TFraction :|: TContent))],
    makeElement Nothing "table" [("children", Many TContent)],
    makeElementWithScope
      Nothing
      "terms"
      [("children", Many TTermItem)]
      [ makeElement
          (Just "terms")
          "item"
          [ ("term", One TContent),
            ("description", One TContent)
          ]
      ],
    ( "measure",
      makeFunction $ do
        -- content <- nthArg 1
        -- styles <- nthArg 2
        pure $
          VDict $
            OM.fromList
              [ ("width", VLength (LExact 1.0 LEm)),
                ("height", VLength (LExact 1.0 LEm))
              ]
    )
    -- these are fake widths so we don't crash...
  ]

visualize :: [(Identifier, Val)]
visualize =
  [ makeElement Nothing "circle" [("body", One (TContent :|: TNone))],
    makeElement Nothing "ellipse" [("body", One (TContent :|: TNone))],
    makeElement Nothing "image" [("path", One TString)],
    makeElement Nothing "line" [],
    makeElement Nothing "path" [("vertices", Many TArray)],
    makeElement Nothing "polygon" [("vertices", Many TArray)],
    makeElement Nothing "rect" [("body", One (TContent :|: TNone))],
    makeElement Nothing "square" [("body", One (TContent :|: TNone))]
  ]

meta :: [(Identifier, Val)]
meta =
  [ makeElement Nothing "bibliography" [("path", One (TString :|: TArray))],
    makeElement Nothing "cite" [("key", One TLabel)],
    makeElement Nothing "document" [],
    makeElement Nothing "figure" [("body", One TContent)],
    makeElement Nothing "heading" [("body", One TContent)],
    makeElement Nothing "quote" [("body", One TContent)],
    makeElement Nothing "layout" [("func", One TFunction)],
    makeElement
      Nothing
      "link"
      [ ("dest", One (TString :|: TLabel :|: TDict :|: TLocation)),
        ("body", One TContent)
      ],
    makeElement Nothing "locate" [("func", One TFunction)],
    makeElement
      Nothing
      "numbering"
      [ ("numbering", One (TString :|: TFunction)),
        ("numbers", Many TInteger)
      ],
    makeElementWithScope Nothing "outline"
      []
      [makeElement (Just "outline") "entry"
        [("level", One TInteger),
         ("element", One TContent),
         ("body", One TContent),
         ("fill", One (TContent :|: TNone)),
         ("page", One TContent)]],
    makeElement
      Nothing
      "query"
      [ ("target", One (TLabel :|: TFunction)),
        ("location", One TLocation)
      ],
    makeElement Nothing "metadata" [ ("value", One TAny) ],
    makeElement Nothing "ref" [("target", One TLabel)],
    makeElement Nothing "state" [("key", One TString), ("init", One TAny)],
    makeElementWithScope
      Nothing
      "footnote"
      [("body", One TContent)]
      [makeElement (Just "footnote") "entry" [("note", One TContent)]],
    ("style", makeFunction $ do
        Function f <- nthArg 1
        case applyPureFunction (Function f) [VStyles] of
          Success x -> pure x
          Failure e -> fail e)
  ]

colors :: [(Identifier, Val)]
colors =
  [ ("red", VColor $ RGB (0xff % 0xff) (0x41 % 0xff) (0x36 % 0xff) 1),
    ("blue", VColor $ RGB (0x00 % 0xff) (0x74 % 0xff) (0xd9 % 0xff) 1),
    ("black", VColor $ RGB (0x00 % 0xff) (0x00 % 0xff) (0x00 % 0xff) 1),
    ("gray", VColor $ RGB (0xaa % 0xff) (0xaa % 0xff) (0xaa % 0xff) 1),
    ("silver", VColor $ RGB (0xdd % 0xff) (0xdd % 0xff) (0xdd % 0xff) 1),
    ("white", VColor $ RGB (0xff % 0xff) (0xff % 0xff) (0xff % 0xff) 1),
    ("navy", VColor $ RGB (0x00 % 0xff) (0x1f % 0xff) (0x3f % 0xff) 1),
    ("aqua", VColor $ RGB (0x7f % 0xff) (0xdb % 0xff) (0xff % 0xff) 1),
    ("teal", VColor $ RGB (0x39 % 0xff) (0xcc % 0xff) (0xcc % 0xff) 1),
    ("eastern", VColor $ RGB (0x23 % 0xff) (0x9d % 0xff) (0xad % 0xff) 1),
    ("purple", VColor $ RGB (0xb1 % 0xff) (0x0d % 0xff) (0xc9 % 0xff) 1),
    ("fuchsia", VColor $ RGB (0xf0 % 0xff) (0x12 % 0xff) (0xbe % 0xff) 1),
    ("maroon", VColor $ RGB (0x85 % 0xff) (0x14 % 0xff) (0x4b % 0xff) 1),
    ("yellow", VColor $ RGB (0xff % 0xff) (0xdc % 0xff) (0x00 % 0xff) 1),
    ("orange", VColor $ RGB (0xff % 0xff) (0x85 % 0xff) (0x1b % 0xff) 1),
    ("olive", VColor $ RGB (0x3d % 0xff) (0x99 % 0xff) (0x70 % 0xff) 1),
    ("green", VColor $ RGB (0x2e % 0xff) (0xcc % 0xff) (0x40 % 0xff) 1),
    ("lime", VColor $ RGB (0x01 % 0xff) (0xff % 0xff) (0x70 % 0xff) 1)
  ]

directions :: [(Identifier, Val)]
directions =
  [ ("ltr", VDirection Ltr),
    ("rtl", VDirection Rtl),
    ("ttb", VDirection Ttb),
    ("btt", VDirection Btt)
  ]

alignments :: [(Identifier, Val)]
alignments =
  [ ("start", VAlignment (Just HorizStart) Nothing),
    ("end", VAlignment (Just HorizEnd) Nothing),
    ("left", VAlignment (Just HorizLeft) Nothing),
    ("center", VAlignment (Just HorizCenter) Nothing),
    ("right", VAlignment (Just HorizRight) Nothing),
    ("top", VAlignment Nothing (Just VertTop)),
    ("horizon", VAlignment Nothing (Just VertHorizon)),
    ("bottom", VAlignment Nothing (Just VertBottom))
  ]

foundations :: [(Identifier, Val)]
foundations =
  [ ( "assert",
      makeFunctionWithScope
        ( do
            (cond :: Bool) <- nthArg 1
            unless cond $ do
              (msg :: String) <- namedArg "message" "Assertion failed"
              fail msg
            pure VNone
        )
        [ ( "eq",
            makeFunction $ do
              (v1 :: Val) <- nthArg 1
              (v2 :: Val) <- nthArg 2
              unless (comp v1 v2 == Just EQ) $ fail "Assertion failed"
              pure VNone
          ),
          ( "ne",
            makeFunction $ do
              (v1 :: Val) <- nthArg 1
              (v2 :: Val) <- nthArg 2
              unless (comp v1 v2 /= Just EQ) $ fail "Assertion failed"
              pure VNone
          )
        ]
    ),
    ("panic", makeFunction $ allArgs >>= fail . T.unpack .
                 (("panicked with: " <>) . T.unlines . map repr)),
    ("repr", makeFunction $ nthArg 1 >>= pure . VString . repr),
    ( "type",
      makeFunction $ do
        (x :: Val) <- nthArg 1
        pure $ VType $ valType x
    )
  ]

construct :: [(Identifier, Val)]
construct =
  [ ( "cmyk",
      makeFunction $
        VColor <$> (CMYK <$> nthArg 1 <*> nthArg 2 <*> nthArg 3 <*> nthArg 4)
    ),
    ("float", makeFunction $ VFloat <$> nthArg 1),
    ("int", makeFunction $ VInteger <$> nthArg 1),
    ("label", makeFunction $ VLabel <$> nthArg 1),
    ( "counter",
      makeFunction $ do
        (counter :: Counter) <- nthArg 1
        let initializeIfMissing Nothing = Just 0
            initializeIfMissing (Just x) = Just x
        lift $ updateState $ \st ->
          st {evalCounters = M.alter initializeIfMissing counter $ evalCounters st}
        pure $ VCounter counter
    ),
    ("luma", makeFunction $ VColor <$> (Luma <$> nthArg 1)),
    ( "range",
      makeFunction $ do
        first <- nthArg 1
        mbsecond <- nthArg 2
        step <- namedArg "step" 1
        pure $
          VArray $
            V.fromList $
              map VInteger $
                case (first, mbsecond) of
                  (end, Nothing) -> enumFromThenTo 0 step (end - 1)
                  (start, Just end) ->
                    enumFromThenTo
                      start
                      (start + step)
                      ( if start < end
                          then end - 1
                          else end + 1
                      )
    ),
    ("regex", makeFunction $ VRegex <$> (nthArg 1 >>= makeRE)),
    ( "rgb",
      makeFunction $
        VColor
          <$> ( ( RGB
                    <$> (nthArg 1 >>= toRatio)
                    <*> (nthArg 2 >>= toRatio)
                    <*> (nthArg 3 >>= toRatio)
                    <*> ((nthArg 4 >>= toRatio) `mplus` pure 1.0)
                )
                  <|> (nthArg 1 >>= hexToRGB)
              )
    ),
    ( "str",
      makeFunctionWithScope
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
    ),
    ( "symbol",
      makeFunction $ do
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
    ),
    ( "lorem",
      makeFunction $ do
        (num :: Int) <- nthArg 1
        pure $ VString $ T.unwords $ take num loremWords
    )
  ]

loremWords :: [Text]
loremWords =
  cycle $
    T.words $
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do\
      \ eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut\
      \ enim ad minim veniam, quis nostrud exercitation ullamco laboris\
      \ nisi ut aliquip ex ea commodo consequat.  Duis aute irure dolor in\
      \ reprehenderit in voluptate velit esse cillum dolore eu fugiat\
      \ nulla pariatur. Excepteur sint occaecat cupidatat non proident,\
      \ sunt in culpa qui officia deserunt mollit anim id est laborum."

toRatio :: MonadFail m => Val -> m Rational
toRatio (VRatio r) = pure r
toRatio (VInteger i) = pure $ i % 255
toRatio _ = fail "cannot convert to rational"

hexToRGB :: MonadFail m => Val -> m Color
hexToRGB (VString s) = do
  let s' = T.dropWhile (== '#') s
  parts <-
    map (fmap (% 255) . readMaybe . T.unpack . ("0x" <>))
      <$> case T.length s' of
        3 -> pure $ T.chunksOf 1 s'
        4 -> pure $ T.chunksOf 1 s'
        6 -> pure $ T.chunksOf 2 s'
        8 -> pure $ T.chunksOf 2 s'
        _ -> fail "hex string must be 3, 4, 6, or 8 digits"
  case parts of
    [Just r, Just g, Just b] -> pure $ RGB r g b 1.0
    [Just r, Just g, Just b, Just o] -> pure $ RGB r g b o
    _ -> fail "could not read string as hex color"
hexToRGB _ = fail "expected string"

loadFileLazyBytes :: Monad m => FilePath -> MP m BL.ByteString
loadFileLazyBytes fp = do
  operations <- evalOperations <$> getState
  lift $ BL.fromStrict <$> loadBytes operations fp

loadFileText :: Monad m => FilePath -> MP m T.Text
loadFileText fp = do
  operations <- evalOperations <$> getState
  lift $ TE.decodeUtf8 <$> loadBytes operations fp

getUTCTime :: Monad m => MP m UTCTime
getUTCTime = (currentUTCTime . evalOperations <$> getState) >>= lift

time :: [(Identifier, Val)]
time =
  [ ( "datetime", makeFunctionWithScope
      (do
         mbyear <- namedArg "year" Nothing
         mbmonth <- namedArg "month" Nothing
         mbday <- namedArg "day" Nothing
         let mbdate = case (mbyear, mbmonth, mbday) of
                        (Just yr, Just mo, Just da) -> fromGregorianValid yr mo da
                        _ -> Nothing
         mbhour <- namedArg "hour" Nothing
         mbminute <- namedArg "minute" Nothing
         mbsecond <- namedArg "second" Nothing
         let mbtime = case (mbhour, mbminute, mbsecond) of
                        (Just hr, Just mi, Just se) ->
                          Just $ secondsToDiffTime $ (hr * 60 * 60) + (mi * 60) + se
                        _ -> Nothing
         pure $ VDateTime mbdate mbtime)
      [ ("today", makeFunction $ do
            utcTime <- lift getUTCTime
            pure $ VDateTime (Just (utctDay utcTime)) (Just (utctDayTime utcTime)) ) ]
     )
  ]

dataLoading :: [(Identifier, Val)]
dataLoading =
  [ ( "csv",
      makeFunction $ do
        fp <- nthArg 1
        bs <- lift $ loadFileLazyBytes fp
        case Csv.decode Csv.NoHeader bs of
          Left e -> fail e
          Right (v :: V.Vector (V.Vector String)) ->
            pure $ VArray $ V.map (VArray . V.map (VString . T.pack)) v
    ),
    ( "json",
      makeFunction $ do
        fp <- nthArg 1
        bs <- lift $ loadFileLazyBytes fp
        case Aeson.eitherDecode bs of
          Left e -> fail e
          Right (v :: Val) -> pure v
    ),
    ( "yaml",
      makeFunction $ do
        fp <- nthArg 1
        bs <- lift $ loadFileLazyBytes fp
        case Yaml.decodeEither' (BL.toStrict bs) of
          Left e -> fail $ show e
          Right (v :: Val) -> pure v
    ),
    ( "read",
      makeFunction $ do
        fp <- nthArg 1
        t <- lift $ loadFileText fp
        pure $ VString t
    ),
    ( "toml",
      makeFunction $ do
        fp <- nthArg 1
        t <- lift $ loadFileText fp
        case Toml.decode (T.unpack t) of
          Toml.Failure e -> fail (unlines ("toml errors:" : e))
          Toml.Success _ v -> pure v
    ),
    ( "xml",
      makeFunction $ do
        fp <- nthArg 1
        bs <- lift $ loadFileLazyBytes fp
        case XML.parseLBS XML.def bs of
          Left e -> fail $ show e
          Right doc ->
            pure $
              VArray $
                V.fromList $
                  mapMaybe
                    nodeToVal
                    [XML.NodeElement (XML.documentRoot doc)]
            where
              showname n = XML.nameLocalName n
              nodeToVal (XML.NodeElement elt) = Just $ eltToDict elt
              nodeToVal (XML.NodeContent t) = Just $ VString t
              nodeToVal _ = Nothing
              eltToDict elt =
                VDict $
                  OM.fromList
                    [ ("tag", VString $ showname (XML.elementName elt)),
                      ( "attrs",
                        VDict $
                          OM.fromList $
                            map
                              (\(k, v) -> (Identifier (showname k), VString v))
                              (M.toList $ XML.elementAttributes elt)
                      ),
                      ( "children",
                        VArray $
                          V.fromList $
                            mapMaybe nodeToVal (XML.elementNodes elt)
                      )
                    ]
    )
  ]

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
