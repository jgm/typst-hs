{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Typst.Pandoc (
    contentToPandoc
) where

import Typst.Types
import qualified Text.Pandoc.Builder as B
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit, isAlphaNum)
import Typst.Methods (applyPureFunction, formatNumber)
import Text.Parsec
import Text.TeXMath (writeTeX)
import Text.TeXMath.Types (Exp(..), FractionType(..), TextType(..),
                           TeXSymbolType(..), Alignment(..), TeXSymbolType(Pun))
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Text.TeXMath.Shared (getSpaceChars)
import Text.Pandoc.Walk
import Control.Monad (MonadPlus(mplus))
import Control.Monad.Reader (lift)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing, catMaybes)
-- import Debug.Trace

contentToPandoc :: Monad m => (Text -> m ()) -> Seq Content -> m (Either ParseError B.Pandoc)
contentToPandoc warn' = runParserT pPandoc warn' "" . F.toList

type P m a = ParsecT [Content] (Text -> m ()) m a

warn :: Monad m => Text -> P m ()
warn msg = do
  warn' <- getState
  lift $ warn' msg

data AttachmentStyle = Limits | Scripts
  deriving (Eq, Show)

getField :: (MonadFail m, MonadPlus m, FromVal a) =>
            Identifier -> M.Map Identifier Val -> m a
getField name fields = fromVal $ fromMaybe VNone $ M.lookup name fields

pPandoc :: Monad m => P m B.Pandoc
pPandoc = B.doc <$> pBlocks

pBlocks :: Monad m => P m B.Blocks
pBlocks = mconcat <$> many pBlock

pBlock :: Monad m => P m B.Blocks
pBlock = pPara <|> pBlockElt

pBlockElt :: Monad m => P m B.Blocks
pBlockElt = pTok isBlock >>= handleBlock

handleBlock :: Monad m => Content -> P m B.Blocks
handleBlock tok =
  case tok of
    Txt{} -> fail "pBlockElt encountered Txt"
    Lab{} -> pure mempty
    Elt "heading" _ fields -> do
      body <- getField "body" fields
      lev <- getField "level" fields <|> pure 1
      B.header lev <$> pWithContents pInlines body
    Elt "list" _ fields -> do
      children <- V.toList <$> getField "children" fields
      B.bulletList <$> mapM (pWithContents pBlocks) children
    Elt "list.item" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt "enum" _ fields -> do
      children <- V.toList <$> getField "children" fields
      mbstart <- getField "start" fields
      start <- case mbstart of
                 Nothing -> pure 1
                 Just x | x >= 0 -> pure x
                        | otherwise -> fail "number must be positive"
      (numbering :: Text) <- getField "numbering" fields `mplus` pure ""
      let (sty, delim) =
            case numbering of
              "1." -> (B.Decimal, B.Period)
              "1)" -> (B.Decimal, B.OneParen)
              "(1)" -> (B.Decimal, B.TwoParens)
              "a." -> (B.LowerAlpha, B.Period)
              "a)" -> (B.LowerAlpha, B.OneParen)
              "(a)" -> (B.LowerAlpha, B.TwoParens)
              "A." -> (B.UpperAlpha, B.Period)
              "A)" -> (B.UpperAlpha, B.OneParen)
              "(A)" -> (B.UpperAlpha, B.TwoParens)
              "i." -> (B.LowerRoman, B.Period)
              "i)" -> (B.LowerRoman, B.OneParen)
              "(i)" -> (B.LowerRoman, B.TwoParens)
              "I." -> (B.UpperRoman, B.Period)
              "I)" -> (B.UpperRoman, B.OneParen)
              "(I)" -> (B.UpperRoman, B.TwoParens)
              _ -> (B.DefaultStyle, B.DefaultDelim)
      let listAttr = (start, sty, delim)
      B.orderedListWith listAttr <$> mapM (pWithContents pBlocks) children
    Elt "enum.item" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt "terms" _ fields -> do
      children <- V.toList <$> getField "children" fields
      B.definitionList <$> mapM (\case
                                    VTermItem t d -> do
                                      t' <- pWithContents pInlines t
                                      d' <- pWithContents pBlocks d
                                      pure (t',[d'])
                                    _ -> pure (mempty, [])) children
    Elt "terms.item" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt "raw" _ fields -> do
      txt <- getField "text" fields
      mblang <- getField "lang" fields
      let attr = ("", maybe [] (\l -> [l]) mblang, [])
      pure $ B.codeBlockWith attr txt
    Elt "parbreak" _ _ -> pure mempty
    Elt "block" _ fields ->
      B.divWith ("",[],[]) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "place" pos fields -> do
      warn "Ignoring parameters of place"
      handleBlock (Elt "block" pos fields)
    Elt "columns" _ fields -> do
      (cnt :: Integer) <- getField "count" fields
      B.divWith ("",["columns-flow"],[("count", T.pack (show cnt))])
        <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "rect" _ fields ->
      B.divWith ("",["rect"],[]) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "circle" _ fields ->
      B.divWith ("",["circle"],[]) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "ellipse" _ fields ->
      B.divWith ("",["ellipse"],[]) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "polygon" _ fields ->
      B.divWith ("",["polygon"],[]) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "square" _ fields ->
      B.divWith ("",["square"],[]) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "align" _ fields -> do
      alignment <- getField "alignment" fields
      B.divWith ("",[],[("align", repr alignment)])
        <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "stack" _ fields -> do
      (dir :: Direction) <- getField "dir" fields `mplus` pure Ltr
      rawchildren <- getField "children" fields
      children <- mapM (\case
                           val@(VFraction{}) ->
                             pure $ B.divWith ("",[],[("space", repr val)]) mempty
                           val -> fromVal val >>= pWithContents pBlocks)
                    (V.toList rawchildren)
      pure $ B.divWith ("",[],[("stack", repr (VDirection dir))]) $
        mconcat $ map (B.divWith ("",[],[])) children
    Elt "grid" _ fields -> do
      children <- getField "children" fields >>= mapM (pWithContents pBlocks) . V.toList
      (columns :: Val) <- getField "columns" fields
      let toWidth (VFraction f) = Just (floor $ 1000 * f)
          toWidth _ = Nothing
      let normalizeWidths xs =
            let (totwidth :: Int) = sum $ catMaybes xs
             in map (\case
                        Just x -> B.ColWidth (fromIntegral x / fromIntegral totwidth)
                        Nothing -> B.ColWidthDefault) xs
      widths <- case columns of
                      VInteger x -> pure $ replicate (fromIntegral x) B.ColWidthDefault
                      VArray x -> pure $ normalizeWidths $ map toWidth (V.toList x)
                      VNone -> pure [B.ColWidthDefault]
                      _ -> fail $ "Could not determine number of columns: " <> show columns
      let numcols = length widths
      align <- getField "align" fields
      let toAlign (VAlignment (Just horiz) _) =
             case horiz of
                  HorizStart -> B.AlignLeft
                  HorizLeft -> B.AlignLeft
                  HorizEnd -> B.AlignRight
                  HorizRight -> B.AlignRight
                  HorizCenter -> B.AlignCenter
          toAlign _ = B.AlignDefault
      aligns <-
        case align of
          VAlignment{} -> pure $ replicate numcols (toAlign align)
          VArray v -> pure $ map toAlign (V.toList v)
          VFunction _ _ f -> do
            mapM (\colnum -> case applyPureFunction f
                                      [VInteger colnum, VInteger 0] of
                                Success x -> pure $ toAlign x
                                Failure e -> fail e)
                        [0..(fromIntegral numcols - 1)]
          _ -> pure $ replicate numcols B.AlignDefault
      let colspecs = zip (aligns ++ repeat B.AlignDefault) widths
      let rows = map (B.Row B.nullAttr) $ chunks numcols $
                  map (B.Cell B.nullAttr B.AlignDefault
                        (B.RowSpan 1) (B.ColSpan 1) . B.toList) children
      pure $ B.table (B.Caption mempty mempty) colspecs (B.TableHead B.nullAttr [])
              [B.TableBody B.nullAttr 0 [] rows]
              (B.TableFoot B.nullAttr [])
    Elt "table" pos fields -> handleBlock (Elt "grid" pos fields)
    Elt "figure" _ fields -> do
      body <- getField "body" fields >>= pWithContents pBlocks
      (mbCaption :: Maybe (Seq Content)) <- getField "caption" fields
      (caption :: B.Blocks) <- maybe mempty (pWithContents pBlocks) mbCaption
      pure $ case B.toList body of
        [B.Table attr _ colspecs thead tbodies tfoot] ->
          B.singleton
           (B.Table attr (B.Caption Nothing (B.toList caption)) colspecs thead tbodies tfoot)
        _ -> B.figure (B.Caption Nothing (B.toList caption)) body
    Elt "line" _ fields
      | isNothing ( M.lookup "start" fields
          >> M.lookup "end" fields
          >> M.lookup "angle" fields ) -> do
      pure $ B.horizontalRule
    Elt "numbering" _ fields -> do
      numStyle <- getField "numbering" fields
      (nums :: V.Vector Integer) <- getField "numbers" fields
      let toText v = fromMaybe "" $ fromVal v
      let toNum n =
            case numStyle of
              VString t -> formatNumber t (fromIntegral n)
              VFunction _ _ f ->
                case applyPureFunction f [VInteger n] of
                  Success x -> toText x
                  Failure _ -> "?"
              _ -> "?"
      pure $ B.plain . B.text . mconcat . map toNum $ V.toList nums
    Elt "footnote.entry" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt (Identifier tname) _ _ -> do
      warn ("Skipping unknown block element " <> tname)
      pure mempty

pPara :: Monad m => P m B.Blocks
pPara =
  B.para . B.trimInlines . mconcat <$> (many1 pInline <* optional pParBreak)

pTok :: Monad m => (Content -> Bool) -> P m Content
pTok f = tokenPrim show showPos match
 where
   showPos _oldpos (Elt _ (Just pos) _) _ = pos
   showPos oldpos _ _ = oldpos
   match x | f x = Just x
   match _ = Nothing

pParBreak :: Monad m => P m ()
pParBreak = () <$ pTok (\case
                           Elt "parbreak" _ _ -> True
                           _  -> False)

isBlock :: Content -> Bool
isBlock (Txt{}) = False
isBlock (Lab{}) = True
isBlock (Elt name _ fields) =
  case name of
    "align" -> True
    "bibliography" -> True
    "block" -> True
    "circle" -> True
    "colbreak" -> True
    "columns" -> True
    "csv" -> True
    "ellipse" -> True
    "enum" -> True
    "enum.item" -> True
    "figure" -> True
    "grid" -> True
    "heading" -> True
    "json" -> True
    "line" -> True
    "list" -> True
    "list.item" -> True
    "numbering" -> True
    "footnote.entry" -> True
    "outline" -> True
    "page" -> True
    "pagebreak" -> True
    "par" -> True
    "parbreak" -> True
    "place" -> True
    "polygon" -> True
    "raw" -> M.lookup "block" fields == Just (VBoolean True)
    "read" -> True
    "rect" -> True
    "square" -> True
    "stack" -> True
    "table" -> True
    "terms" -> True
    "terms.item" -> True
    "toml" -> True
    "v" -> True
    "xml" -> True
    "yaml" -> True
    _ -> False

pWithContents :: Monad m => P m a -> Seq Content -> P m a
pWithContents pa cs = do
  inp <- getInput
  setInput $ F.toList cs
  res <- pa
  setInput inp
  pure res

withGroup :: [Exp] -> Exp
withGroup [x] = x
withGroup xs = EGrouped xs

getAttachmentStyle :: Monad m => M.Map Identifier Val -> P m (Maybe AttachmentStyle)
getAttachmentStyle fields = do
  (base :: Seq Content) <- getField "base" fields
  case base of
    [Elt "limits" _ _] -> pure $ Just Limits
    [Elt "scripts" _ _] -> pure $ Just Scripts
    _ -> pure Nothing

pMath :: Monad m => P m Exp
pMath = pTok (const True) >>= handleMath

handleMath :: Monad m => Content -> P m Exp
handleMath tok =
  case tok of
    Lab t -> do
      warn ("skipping label " <> t)
      pure (EGrouped [])
    Txt t
      | T.any isDigit t -> pure $ ENumber t
      | T.length t == 1 ->
         case T.unpack t of
           [c] | not (isAlphaNum c) -> pure $ ESymbol (getSymbolType c) t
           _ -> pure $ EIdentifier t
      | otherwise -> pure $ EText TextNormal t
    Elt "math.dif" _ _ -> pure $ EIdentifier "d"
    Elt "math.Dif" _ _ -> pure $ EIdentifier "D"
    Elt "math.equation" _ fields -> getField "body" fields >>= pMathGrouped
    Elt "text" _ fields -> do
      body <- getField "body" fields
      (mbweight :: Maybe Text) <- getField "weight" fields
      case mbweight of
        Just "bold" -> EStyled TextBold <$> pMathMany body
        _ -> pMathGrouped body
    Elt "math.op" _ fields -> EMathOperator <$> getField "text" fields
    Elt "math.frac" _ fields -> do
      num <- getField "num" fields >>= pMathGrouped
      denom <- getField "denom" fields >>= pMathGrouped
      pure $ EFraction NormalFrac num denom
    Elt "math.accent" _ fields -> do
      base <- getField "base" fields >>= pMathGrouped
      acc <- getField "accent" fields >>= pMathGrouped
      let acc' = case acc of
                       ESymbol _ t -> ESymbol Accent t
                       _ -> acc
      pure $ EOver False base acc'
    Elt "math.attach" _ fields -> do
      attachmentStyle <- getAttachmentStyle fields
      base <- getField "base" fields >>= pMathGrouped
      t' <- getField "t" fields
      b' <- getField "b" fields
      tr' <- getField "tr" fields
      tl' <- getField "tl" fields
      br' <- getField "br" fields
      bl' <- getField "bl" fields
      let limits = attachmentStyle == Just Limits
      let (mbt, mbtr) =
            case (t', tr') of
              (Just top, Just topright) -> (Just top, Just topright)
              (Just top, Nothing)
                | limits -> (Just top, Nothing)
                | otherwise -> (Nothing, Just top)
              (Nothing, Just topright) -> (Nothing, Just topright)
              (Nothing, Nothing) -> (Nothing, Nothing)
      let (mbb, mbbr) =
            case (b', br') of
              (Just bot, Just botright) -> (Just bot, Just botright)
              (Just bot, Nothing)
                | limits -> (Just bot, Nothing)
                | otherwise -> (Nothing, Just bot)
              (Nothing, Just topright) -> (Nothing, Just topright)
              (Nothing, Nothing) -> (Nothing, Nothing)
      let dummy = EGrouped []
      let addPrefix x =
            case (tl', bl') of
              (Nothing, Nothing) -> pure x
              (Just top, Nothing) -> do
                res <- ESuper dummy <$> pMathGrouped top
                pure $ EGrouped [res, x]
              (Nothing, Just bot) -> do
                res <- ESub dummy <$> pMathGrouped bot
                pure $ EGrouped [res, x]
              (Just top, Just bot) -> do
                res <- ESubsup dummy <$> pMathGrouped bot <*> pMathGrouped top
                pure $ EGrouped [res, x]

      base' <- case (mbtr, mbbr) of
        (Nothing, Nothing) -> pure base
        (Nothing, Just br) -> ESub base <$> pMathGrouped br
        (Just tr, Nothing) -> ESuper base <$> pMathGrouped tr
        (Just tr, Just br) -> ESubsup base <$> pMathGrouped br <*> pMathGrouped tr

      suffix <- case (mbt, mbb) of
        (Nothing, Nothing) -> pure base'
        (Nothing, Just bot) -> EUnder False base' <$> pMathGrouped bot
        (Just top, Nothing) -> EOver False base' <$> pMathGrouped top
        (Just top, Just bot) -> EUnderover False base' <$> pMathGrouped bot <*> pMathGrouped top

      addPrefix suffix

    Elt "math.serif" _ fields ->
      EStyled TextNormal <$> (getField "body" fields >>= pMathMany)
    Elt "math.sans" _ fields ->
      EStyled TextSansSerif <$> (getField "body" fields >>= pMathMany)
    Elt "math.frak" _ fields ->
      EStyled TextFraktur <$> (getField "body" fields >>= pMathMany)
    Elt "math.mono" _ fields ->
      EStyled TextMonospace <$> (getField "body" fields >>= pMathMany)
    Elt "math.cal" _ fields ->
      EStyled TextScript <$> (getField "body" fields >>= pMathMany)
    Elt "math.bb" _ fields ->
      EStyled TextDoubleStruck <$> (getField "body" fields >>= pMathMany)
    Elt "math.upright" _ fields ->
      EStyled TextNormal <$> (getField "body" fields >>= pMathMany)
    Elt "math.bold" _ fields ->
      EStyled TextBold <$> (getField "body" fields >>= pMathMany)
    Elt "math.italic" _ fields ->
      EStyled TextItalic <$> (getField "body" fields >>= pMathMany)
    Elt "math.underline" _ fields ->
      EUnder False <$> (getField "body" fields >>= pMathGrouped)
                   <*>  pure (ESymbol TUnder "_")
    Elt "math.overline" _ fields ->
      EUnder False <$> (getField "body" fields >>= pMathGrouped)
                   <*>  pure (ESymbol TOver "\175")
    Elt "math.underbrace" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EUnder False body (ESymbol TUnder "\9183")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EUnder False x <$> pMathGrouped ann
    Elt "math.overbrace" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EOver False body (ESymbol TOver "\9182")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EOver False x <$> pMathGrouped ann
    Elt "math.underbracket" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EUnder False body (ESymbol TUnder "\9141")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EUnder False x <$> pMathGrouped ann
    Elt "math.overbracket" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EOver False body (ESymbol TOver "\9140")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EOver False x <$> pMathGrouped ann
    Elt "math.scripts" _ fields -> getField "body" fields >>= pMathGrouped
    Elt "math.limits" _ fields ->  getField "body" fields >>= pMathGrouped
    Elt "math.root" _ fields -> do
      mbindex <- getField "index" fields
      radicand <- getField "radicand" fields >>= pMathGrouped
      case mbindex of
        Nothing -> pure $ ESqrt radicand
        Just index -> do
          index' <- pMathGrouped index
          pure $ ERoot index' radicand
    Elt "math.sqrt" _ fields ->
      ESqrt <$> (getField "radicand" fields >>= pMathGrouped)
    Elt "math.abs" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "|" "|" [ Right body ]
    Elt "math.floor" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8970" "\8971" [ Right body ]
    Elt "math.ceil" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8968" "\8969" [ Right body ]
    Elt "math.norm" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8741" "\8741" [ Right body ]
    Elt "math.round" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8970" "\8969" [ Right body ]
    Elt "math.lr" _ fields -> do
      bodyparts <- getField "body" fields >>= mapM pMathMany . V.toList
      let rawbody = intercalate [ESymbol Pun ","] bodyparts
      let (op, rest) =
            case rawbody of
              (ESymbol _ t:xs) -> (t, xs)
              _ -> ("", rawbody)
      let (body, cl) =
            case reverse rest of
              (ESymbol _ t:_) -> (map Right (init rest), t)
              _ -> (map Right rest, "")
      pure $ EDelimited op cl body
    Elt "math.binom" _ fields -> do
      up <- getField "upper" fields >>= pMathGrouped
      low <- getField "lower" fields >>= pMathGrouped
      pure $ EDelimited "(" ")" [ Right (EFraction NoLineFrac up low) ]
    Elt "math.cases" _ fields -> do
      (delim :: Maybe Text) <- getField "delim" fields
      (children :: [Seq Content]) <-
        map valToContent . V.toList <$> getField "children" fields
      let isAlignPoint (Elt "math.alignpoint" _ _) = True
          isAlignPoint _ = False
      let formatRow vs = case Seq.breakl isAlignPoint vs of
                            (xs, ys) -> do
                              case Seq.viewl ys of
                                _ Seq.:< rest -> do
                                  xs' <- pMathMany xs
                                  ys' <- pMathMany rest
                                  pure [xs', ys']
                                _ -> (:[]) <$> pMathMany vs
      rows <- mapM formatRow children
      pure $ EDelimited (fromMaybe "{" delim) ""
              [ Right (EArray [AlignLeft, AlignLeft] rows) ]
    Elt "math.vec" _ fields -> do
      (op, cl) <- arrayDelims fields
      rows <- getField  "children" fields >>=
                 mapM (fmap (:[]) . pMathMany) . V.toList
      pure $ EDelimited op cl
              [ Right (EArray [AlignCenter] rows) ]
    Elt "math.mat" _ fields -> do
      (op, cl) <- arrayDelims fields
      let formatCell x = do
            let content = valToContent x
            let align = case Seq.viewl content of
                          Elt "math.alignpoint" _ _ Seq.:< _ -> AlignLeft
                          _ -> case Seq.viewr content of
                                 _ Seq.:> Elt "math.alignpoint" _ _ -> AlignRight
                                 _ -> AlignCenter
            exp' <- pMathMany content
            pure (align, exp')
      let formatRow (VArray vs) = mapM formatCell (V.toList vs)
          formatRow _ = fail "mat expected array"
      (rawrows :: V.Vector Val) <- getField  "rows" fields
      rows <- mapM formatRow (V.toList rawrows)
      let aligns =
             case rows of
                 [] -> []
                 (r:_) -> map fst r
      pure $ EDelimited op cl
              [ Right (EArray aligns (map (map snd) rows)) ]
    Elt "hide" _ fields -> do
      EPhantom <$> (getField "body" fields >>= pMathGrouped)
    Elt "h" _ fields -> do
      amount <- getField "amount" fields
      let em = case amount of
                 LExact x LEm -> toRational x
                 _ -> case amount <> LExact 0 LPt of -- force to Pt
                        LExact x LPt -> toRational x / 12
                        _ -> 1/3 -- guess!
      pure $ ESpace em
    Elt "grid" _ fields -> do
      children <- getField "children" fields >>= mapM pMathMany . V.toList
      (columns :: Val) <- getField "columns" fields
      numcols <- case columns of
                      VInteger x -> pure $ fromIntegral x
                      VArray x -> pure $ V.length x
                      VNone -> pure 1
                      _ -> fail $ "Could not determine number of columns: " <> show columns
      let rows = chunks numcols children
      pure $ EArray (replicate numcols AlignLeft) rows
    Elt "table" pos fields -> handleMath (Elt "grid" pos fields)
    Elt "link" _ fields -> do
      body <- getField "body" fields
      warn "Hyperlinks not supported in math"
      pMathGrouped body
    Elt (Identifier name) _ fields -> do
      body <- getField "body" fields `mplus` pure mempty
      warn ("Ignoring unsupported " <> name)
      pMathGrouped body

arrayDelims :: Monad m => M.Map Identifier Val -> P m (Text, Text)
arrayDelims fields = do
  (mbdelim :: Maybe Text) <- getField "delim" fields
  pure $ case mbdelim of
    Just "(" -> ( "(", ")" )
    Just "[" -> ( "[", "]" )
    Just "{" -> ( "{", "}" )
    Just "|" -> ( "|", "|" )
    Just "||" -> ( "\8741", "\8741" )
    _ -> ( "(", ")" )

pMathMany :: Monad m => Seq Content -> P m [Exp]
pMathMany cs = do
  -- check for "alignpoint" and "linebreak" elements
  -- and use an array structure for alignment
  let lns = splitOnLinebreaks cs
  case lns of
    [] -> pure []
    [ln] | not (any isAlignpoint ln) -> pWithContents (many pMath) ln
    _ -> do
      rows <- mapM (mapM (pWithContents (many pMath)) . splitOnAlignpoints) lns
      let numcols = maximum $ map length rows
      let cols = take numcols $ AlignRight : cycle [AlignLeft, AlignRight]
      pure [EArray cols rows]

pMathGrouped :: Monad m => Seq Content -> P m Exp
pMathGrouped = fmap withGroup . pMathMany

splitOnLinebreaks :: Seq Content -> [Seq Content]
splitOnLinebreaks xs =
  if Seq.null bs
     then
       if null as
          then []
          else [as]
     else as : splitOnLinebreaks (Seq.drop 1 bs)
 where
  (as, bs) = Seq.breakl isLinebreak xs
  isLinebreak (Elt "linebreak" _ _) = True
  isLinebreak _ = False

splitOnAlignpoints :: Seq Content -> [Seq Content]
splitOnAlignpoints xs =
  if Seq.null bs
     then
       if null as
          then []
          else [as]
     else as : splitOnAlignpoints (Seq.drop 1 bs)
 where
  (as, bs) = Seq.breakl isAlignpoint xs

isAlignpoint :: Content -> Bool
isAlignpoint (Elt "math.alignpoint" _ _) = True
isAlignpoint _ = False

pInlines :: Monad m => P m B.Inlines
pInlines = mconcat <$> many pInline

pInline :: Monad m => P m B.Inlines
pInline = pTok (not . isBlock) >>= handleInline

handleInline :: Monad m => Content -> P m B.Inlines
handleInline tok =
  case tok of
    Txt t -> pure $ B.text t
    Lab name -> pure $ B.spanWith (name, [], []) mempty
    Elt "linebreak" _ _ -> pure B.linebreak
    Elt "text" _ fields -> do
      body <- getField "body" fields
      (mbweight :: Maybe Text) <- getField "weight" fields
      case mbweight of
        Just "bold" -> B.strong <$> pWithContents pInlines body
        _ -> pWithContents pInlines body
    Elt "raw" _ fields -> B.code <$> getField "text" fields
    Elt "footnote" _ fields ->
      B.note <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "cite" _ fields -> do
      keys <- V.toList <$> getField "keys" fields
      let toCitation key = B.Citation{
                B.citationId = key
              , B.citationPrefix = mempty
              , B.citationSuffix = mempty
              , B.citationMode = B.NormalCitation
              , B.citationNoteNum = 0
              , B.citationHash = 0 }
      let citations = map toCitation keys
      pure $ B.cite citations (B.text $ "[" <> T.intercalate "," keys <> "]")
    Elt "lower" _ fields -> do
      body <- getField "text" fields
      walk (modString T.toLower) <$> pWithContents pInlines body
    Elt "upper" _ fields -> do
      body <- getField "text" fields
      walk (modString T.toUpper) <$> pWithContents pInlines body
    Elt "emph" _ fields -> do
      body <- getField "body" fields
      B.emph <$> pWithContents pInlines body
    Elt "strong" _ fields -> do
      body <- getField "body" fields
      B.strong <$> pWithContents pInlines body
    Elt "sub" _ fields -> do
      body <- getField "body" fields
      B.subscript <$> pWithContents pInlines body
    Elt "super" _ fields -> do
      body <- getField "body" fields
      B.superscript <$> pWithContents pInlines body
    Elt "strike" _ fields -> do
      body <- getField "body" fields
      B.strikeout <$> pWithContents pInlines body
    Elt "smallcaps" _ fields -> do
      body <- getField "body" fields
      B.smallcaps <$> pWithContents pInlines body
    Elt "underline" _ fields -> do
      body <- getField "body" fields
      B.underline <$> pWithContents pInlines body
    Elt "math.equation" _ fields -> do
      body <- getField "body" fields
      display <- getField "block" fields
      (if display then B.displayMath else B.math) . writeTeX <$> pMathMany body
    Elt "link" _ fields -> do
      dest <- getField "dest" fields
      src <- case dest of
               VString t -> pure t
               VLabel t -> pure $ "#" <> t
               VDict _ -> do
                 warn "Unable to link to location, linking to #"
                 pure "#"
               _ -> fail $ "Expected string or label for dest"
      body <- getField "body" fields
      description <- if null body
                        then pure $ B.text $
                             if "mailto:" `T.isPrefixOf` src
                                then T.drop 7 src
                                else if "tel:" `T.isPrefixOf` src
                                        then T.drop 4 src
                                        else src
                        else pWithContents pInlines body
      pure $ B.link src "" description
    Elt "image" _ fields -> do
      path <- getField "path" fields
      alt <- (B.text <$> getField "alt" fields) `mplus` pure mempty
      (mbwidth :: Maybe Text) <-
        fmap (renderLength False) <$> getField "width" fields
      (mbheight :: Maybe Text) <-
        fmap (renderLength False) <$> getField "height" fields
      let attr = ("", [],
                  maybe [] (\x -> [("width",x)]) mbwidth ++
                  maybe [] (\x -> [("height",x)]) mbheight)
      pure $ B.imageWith attr path "" alt
    Elt "box" _ fields -> do
      body <- getField "body" fields
      B.spanWith ("",["box"],[]) <$> pWithContents pInlines body
    Elt "h" _ fields -> do
      amount <- getField "amount" fields `mplus` pure (LExact 1 LEm)
      let em = case amount of
                 LExact x LEm -> toRational x
                 _ -> case amount <> LExact 0 LPt of -- force to Pt
                        LExact x LPt -> toRational x / 12
                        _ -> 1/3 -- guess!
      pure $ B.text $ getSpaceChars em
    Elt (Identifier tname) _ _ -> do
      warn ("Skipping unknown inline element " <> tname)
      pure mempty

modString :: (Text -> Text) -> B.Inline -> B.Inline
modString f (B.Str t) = B.Str (f t)
modString _ x = x

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
