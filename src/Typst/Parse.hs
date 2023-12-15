{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Typst.Parse
  ( parseTypst,
  )
where

import Data.List (sortOn)
import Control.Applicative (some)
import Control.Monad (MonadPlus (mzero), guard, void, when)
import Control.Monad.Identity (Identity)
import Data.Char hiding (Space)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (string)
import qualified Text.Parsec as P
import Text.Parsec.Expr
import Text.Read (readMaybe)
import Typst.Syntax
import Typst.Shorthands (mathSymbolShorthands)

-- import Debug.Trace

parseTypst :: FilePath -> Text -> Either ParseError [Markup]
parseTypst fp inp =
  case runParser (spaces *> many pMarkup <* pEndOfContent) initialState fp inp of
    Left e -> Left e
    Right r -> Right r

data PState = PState
  { stIndent :: [Int],
    stLineStartCol :: !Int,
    stAllowNewlines :: !Int, -- allow newlines if > 0
    stBeforeSpace :: Maybe (SourcePos, Text),
    stContentBlockNesting :: Int
  }
  deriving (Show)

initialState :: PState
initialState =
  PState
    { stIndent = [],
      stLineStartCol = 1,
      stAllowNewlines = 0,
      stBeforeSpace = Nothing,
      stContentBlockNesting = 0
    }

type P = Parsec Text PState

string :: String -> P String
string = try . P.string

ws :: P ()
ws = do
  p1 <- getPosition
  inp <- getInput
  allowNewlines <- stAllowNewlines <$> getState
  let isSp c
        | allowNewlines > 0 = c == ' ' || c == '\t' || c == '\n' || c == '\r'
        | otherwise = c == ' ' || c == '\t'
  ( skipMany1 (void (satisfy isSp) <|> void pComment)
      *> updateState (\st -> st {stBeforeSpace = Just (p1, inp)})
    )
    <|> updateState (\st -> st {stBeforeSpace = Nothing})

lexeme :: P a -> P a
lexeme pa = pa <* ws

sym :: String -> P String
sym = lexeme . string

op :: String -> P ()
op s = try $ lexeme $ do
  void $ string s
  when
    ( s == "+"
        || s == "-"
        || s == "*"
        || s == "/"
        || s == "="
        || s == "<"
        || s == ">"
        || s == "!"
    )
    $ notFollowedBy (char '=')
  when (s == "-") $
    notFollowedBy (char '>') -- arrows
  when (s == "<") $
    notFollowedBy (char '-' <|> char '=') -- arrows
  when (s == "=") $
    notFollowedBy (char '>' <|> char '=')

withNewlines :: P a -> P a
withNewlines pa = do
  updateState $ \st -> st {stAllowNewlines = stAllowNewlines st + 1}
  res <- pa
  updateState $ \st -> st {stAllowNewlines = stAllowNewlines st - 1}
  pure res

inParens :: P a -> P a
inParens pa = withNewlines (between (sym "(") (char ')') pa) <* ws

inBraces :: P a -> P a
inBraces pa = withNewlines (between (sym "{") (char '}') pa) <* ws

pMarkup :: P Markup
pMarkup =
  pSpace
    <|> pHeading
    <|> pComment
    <|> pEol
    <|> pHardbreak
    <|> pStrong
    <|> pEmph
    <|> pEquation
    <|> pListItem
    <|> pUrl
    <|> pText
    <|> pRawBlock
    <|> pRawInline
    <|> pEscaped
    <|> pNbsp
    <|> pDash
    <|> pEllipsis
    <|> pQuote
    <|> pLabelInContent
    <|> pRef
    <|> pHash
    <|> pBracketed
    <|> pSymbol

-- We need to group paired brackets or the closing bracketed may be
-- taken to close a pContent block:
pBracketed :: P Markup
pBracketed =
  Bracketed <$> try (between (char '[') (char ']') (many pMarkup))

pSymbol :: P Markup
pSymbol = do
  blockNesting <- stContentBlockNesting <$> getState
  let isSpecial' c = isSpecial c && (c /= ']' || blockNesting == 0)
  Text . T.singleton <$> satisfy isSpecial'

-- equation ::= ('$' math* '$') | ('$ ' math* ' $')
pEquation :: P Markup
pEquation = do
  void $ char '$'
  withNewlines $ do
    display <- option False $ True <$ lookAhead space
    ws
    maths <- many pMath
    void $ char '$'
    pure $ Equation display maths

mathOperatorTable :: [[Operator Text PState Identity Markup]]
mathOperatorTable =
  [ -- precedence 7 -- attachment with number, e.g. a_1 (see #17)
    [ Infix (attachBottom <$ (try (op "_" *> lookAhead mNumber))) AssocLeft,
      Infix (attachTop <$ (try (op "^" *> lookAhead mNumber))) AssocLeft
    ],
    -- precedence 6
    [ Postfix
        ( try $ do
            mbBeforeSpace <- stBeforeSpace <$> getState
            -- NOTE: can't have space before () or [] arg in a
            -- function call! to prevent bugs with e.g. 'if 2<3 [...]'.
            guard $ isNothing mbBeforeSpace
            args <- mGrouped '(' ')' True
            pure $ \expr -> MGroup Nothing Nothing [expr, args]
        )
    ],
    -- precedence 5 -- attachment with non-number, e.g. a_x
    [ Infix (attachBottom <$ op "_") AssocLeft,
      Infix (attachTop <$ op "^") AssocLeft
    ],
    -- precedence 4  -- factorial needs to take precedence over fraction
    [ Postfix (try $ do
                  mbBeforeSpace <- stBeforeSpace <$> getState
                  guard $ isNothing mbBeforeSpace
                  lexeme $ char '!' *> notFollowedBy (char '=')
                  pure (\expr -> MGroup Nothing Nothing [expr, Text "!"]))
    ],
    -- precedence 3
    [ Infix (makeFrac <$ op "/") AssocLeft
    ]
  ]


 -- MAttach (Maybe bottom) (Maybe top) base

attachBottom :: Markup -> Markup -> Markup
attachBottom (MAttach Nothing y x) z = MAttach (Just (hideOuterParens z)) y x
attachBottom z (MAttach Nothing y x) = MAttach (Just (hideOuterParens x)) y z
attachBottom base x = MAttach (Just (hideOuterParens x)) Nothing base

attachTop :: Markup -> Markup -> Markup
attachTop (MAttach x Nothing y) z = MAttach x (Just (hideOuterParens z)) y
attachTop z (MAttach x Nothing y) = MAttach x (Just (hideOuterParens y)) z
attachTop base x = MAttach Nothing (Just (hideOuterParens x)) base

makeFrac :: Markup -> Markup -> Markup
makeFrac x y = MFrac x (hideOuterParens y)

hideOuterParens :: Markup -> Markup
hideOuterParens (MGroup (Just "(") (Just ")") x) = MGroup Nothing Nothing x
hideOuterParens x = x

mathExpressionTable :: [[Operator Text PState Identity Expr]]
mathExpressionTable = take 16 (cycle [[fieldAccess], [mathFunctionCall]])

mathFunctionCall :: Operator Text PState Identity Expr
mathFunctionCall =
  Postfix
    ( do
        mbBeforeSpace <- stBeforeSpace <$> getState
        -- NOTE: can't have space before () or [] arg in a
        -- function call! to prevent bugs with e.g. 'if 2<3 [...]'.
        guard $ isNothing mbBeforeSpace
        args <- mArgs
        pure $ \expr -> FuncCall expr args
    )

mExpr :: P Markup
mExpr = Code <$> getPosition <*> pMathExpr

pMathExpr :: P Expr
pMathExpr = buildExpressionParser mathExpressionTable
               (pMathIdent <|> pMathLiteral)
 where
   pMathLiteral :: P Expr
   pMathLiteral = Block . Content
                    <$> many1 (mLiteral <|> mEscaped <|> mShorthand)

pMathIdent :: P Expr
pMathIdent =
  (Ident <$> pMathIdentifier)
    <|> ( do
            void $ char '√'
            (Ident (Identifier "root") <$ lookAhead (char '('))
              <|> ( do
                      x <- pMath
                      pure $
                        FuncCall
                          (Ident (Identifier "root"))
                          [NormalArg (Block (Content [x]))]
                  )
        )

pMathIdentifier :: P Identifier
pMathIdentifier = lexeme $ try $ do
  c <- satisfy isIdentStart
  cs <- many1 $ satisfy isMathIdentContinue
  pure $ Identifier $ T.pack (c : cs)

isMathIdentContinue :: Char -> Bool
isMathIdentContinue c = isIdentContinue c && c /= '_' && c /= '-'

pMath :: P Markup
pMath = buildExpressionParser mathOperatorTable pBaseMath

pBaseMath :: P Markup
pBaseMath = mNumber
        <|> mLiteral
        <|> mEscaped
        <|> mShorthand
        <|> mBreak
        <|> mAlignPoint
        <|> mExpr
        <|> mGroup
        <|> mCode
        <|> mMid
        <|> mSymbol

mGroup :: P Markup
mGroup = mGrouped '(' ')' False
     <|> mGrouped '{' '}' False
     <|> mGrouped '[' ']' False

mGrouped :: Char -> Char -> Bool -> P Markup
mGrouped op' cl requireMatch = withNewlines $ try $ do
  void $ sym [op']
  res <- many (notFollowedBy (char cl) *> pMath)
  (MGroup (Just (T.singleton op')) (Just (T.singleton cl)) res <$ void (sym [cl]))
    <|> (MGroup (Just (T.singleton op')) Nothing res <$ guard (not requireMatch))

mNumber :: P Markup
mNumber = lexeme $ do
  ds <- T.pack <$> many1 digit
  opt <-
    option
      mempty
      ( do
          e <- char '.'
          es <- many1 digit
          pure $ T.pack (e : es)
      )
  pure $ Text (ds <> opt)

mLiteral :: P Markup
mLiteral = do
  mbBeforeSpace <- stBeforeSpace <$> getState
  String t <- pStr
  -- ensure space in e.g. x "is natural":
  mbAfterSpace <- stBeforeSpace <$> getState
  pure $
    Text $
      (maybe "" (const " ") mbBeforeSpace)
        <> t
        <> (maybe "" (const " ") mbAfterSpace)

mEscaped :: P Markup
mEscaped = Text . T.singleton <$> lexeme (try pEsc)

mBreak :: P Markup
mBreak = HardBreak <$ lexeme (char '\\' *> skipMany (satisfy (isSpace)))

-- we don't need to check for following whitespace, because
-- anything else would have been parsed by mEsc.
-- but we do skip following whitespace, since \160 wouldn't be gobbled by lexeme...

mAlignPoint :: P Markup
mAlignPoint = MAlignPoint <$ sym "&"

-- Math args can't have a content block; they can use semicolons
-- to separate array args.
mArgs :: P [Arg]
mArgs =
  inParens $
    many (mKeyValArg <|> mArrayArg <|> mNormArg <|> mMathArg)
  where
    sep = void (sym ",") <|> void (lookAhead (char ')'))
    mNormArg = try $ NormalArg <$> (char '#' *> pExpr <* sep)
    mKeyValArg = do
      ident <- try $ pIdentifier <* sym ":"
      KeyValArg ident
        <$> ( (char '#' *> pExpr <* sep)
                <|> Block . Content <$> mathContent
            )
    mathContent = do
      xs <- maths
      if null xs
        then void $ sym ","
        else sep
      pure xs
    mMathArg = BlockArg <$> mathContent
    mArrayArg = try $ do
      let pRow = sepBy' (toGroup <$> maths) (sym ",")
      rows <- many1 $ try (pRow <* sym ";")
      -- parse any regular items and form a last row
      lastrow <- many (toGroup <$> mathContent)
      let rows' =
            if null lastrow
              then rows
              else rows ++ [lastrow]
      pure $ ArrayArg rows'
    maths = many (notFollowedBy (oneOf ",;)") *> notFollowedBy mKeyValArg *> pMath)
    toGroup [m] = m
    toGroup ms = MGroup Nothing Nothing ms
    -- special sepBy' with an added try:
    sepBy' p s = sepBy1' p s <|> pure []
    sepBy1' p s = do
      x <- p
      xs <- many (try (s *> p))
      pure (x : xs)

mCode :: P Markup
mCode = lexeme $ char '#' *> (Code <$> getPosition <*> pBasicExpr)

mMid :: P Markup
mMid = try $ do
  getState >>= guard . isJust . stBeforeSpace
  void $ char '|' *> space *> ws
  pure $ MGroup Nothing Nothing [Nbsp, Text "|", Nbsp]

mShorthand :: P Markup
mShorthand =
  getPosition >>= \pos ->
   lexeme (Code pos <$> choice (map toShorthandParser shorthands))
 where
  shorthands = reverse (sortOn (T.length . fst) mathSymbolShorthands)
  toShorthandParser (short, symname) =
    toSym symname <$ try (string (T.unpack short))
  toSym name =
    case map (Ident . Identifier) $ T.split (== '.') name of
      [] -> Literal None
      [i] -> i
      (i:is) -> foldr FieldAccess i is

mSymbol :: P Markup
mSymbol =
  lexeme ( Text . T.singleton
            <$> satisfy (\c -> not (isSpace c) && c /= '$' && c /= '\\'))

withIndent :: Int -> P a -> P a
withIndent indent pa = do
  oldIndent <- stIndent <$> getState
  updateState $ \st -> st {stIndent = indent : oldIndent}
  ms <- pa
  updateState $ \st -> st {stIndent = oldIndent}
  pure ms

-- list ::= '-' space markup
-- enum ::= (digit+ '.' | '+') space markup
-- desc ::= '/' space markup ':' space markup
pListItem :: P Markup
pListItem = do
  col <- sourceColumn <$> getPosition
  startLine <- stLineStartCol <$> getState
  guard (col == startLine)
  try
    ( do
        void $ char '-'
        void (char ' ') <|> pBlankline
        BulletListItem <$> withIndent col (many pMarkup)
    )
    <|> try
      ( do
          start <- (Nothing <$ char '+') <|> (Just <$> enumListStart)
          void (char ' ') <|> pBlankline
          EnumListItem start <$> withIndent col (many pMarkup)
      )
    <|> try
      ( do
          -- desc list
          void (char '/')
          void (many1 (char ' '))
          term <- manyTill pMarkup (char ':')
          skipMany spaceChar
          optional pBlankline
          DescListItem term <$> withIndent col (many pMarkup)
      )

enumListStart :: P Int
enumListStart = do
  ds <- many1 digit
  void $ char '.'
  case readMaybe ds of
    Nothing -> fail $ "could not read " <> ds <> " as digits"
    Just x -> pure x

-- line-comment = '//' (!unicode(Newline))*
-- block-comment = '/*' (. | block-comment)* '*/'
pComment :: P Markup
pComment = Comment <$ (pLineComment <|> pBlockComment)

pLineComment :: P ()
pLineComment = do
  void $ string "//"
  skipMany (satisfy (\c -> c /= '\n' && c /= '\r'))
  void endOfLine

pBlockComment :: P ()
pBlockComment = do
  void $ string "/*"
  void $
    manyTill
      ( pBlockComment
          <|> pLineComment
          <|> void anyChar
      )
      (string "*/")

pSpace :: P Markup
pSpace = Space <$ some (satisfy (\c -> isSpace c && c /= '\r' && c /= '\n'))

pEol :: P Markup
pEol = do
  pBaseEol
  (ParBreak <$ many1 pBaseEol)
    <|> (ParBreak <$ pEndOfContent)
    <|> pure SoftBreak

pBaseEol :: P ()
pBaseEol = try $ do
  void endOfLine
  -- fail if we can't indent enough
  indents <- stIndent <$> getState
  case indents of
    (i : _) -> void (try (count i (char ' '))) <|> pBlankline
    [] -> pure ()
  eatPrefixSpaces

eatPrefixSpaces :: P ()
eatPrefixSpaces = do
  skipMany spaceChar
  col <- sourceColumn <$> getPosition
  updateState $ \st -> st {stLineStartCol = col}

spaceChar :: P Char
spaceChar = satisfy (\c -> c == ' ' || c == '\t')

pHardbreak :: P Markup
pHardbreak =
  HardBreak <$ try (char '\\' *> (void spaceChar <|> pBaseEol) *> skipMany spaceChar)

pBlankline :: P ()
pBlankline = try $ do
  skipMany spaceChar
  void (lookAhead (endOfLine)) <|> pEndOfContent

pRawInline :: P Markup
pRawInline =
  RawInline . T.pack
    <$> (char '`' *> manyTill anyChar (void (char '`') <|> eof))

pRawBlock :: P Markup
pRawBlock = do
  void $ string "```"
  numticks <- (+ 3) . length <$> many (char '`')
  lang <- T.pack <$> (many alphaNum <* optional (char ' '))
  optional $ try $ skipMany (char ' ') *> pEol
  let nl = newline <* optionalGobbleIndent
  code <-
    T.pack
      <$> manyTill
        (nl <|> anyChar)
        (string (replicate numticks '`'))
  skipMany (char '`')
  pure $ RawBlock lang code

optionalGobbleIndent :: P ()
optionalGobbleIndent = do
  indents <- stIndent <$> getState
  case indents of
    (i : _) -> gobble i
    [] -> pure ()
  where
    gobble :: Int -> P ()
    gobble 0 = pure ()
    gobble n = (char ' ' *> gobble (n - 1)) <|> pure ()

pStrong :: P Markup
pStrong = Strong <$> (char '*' *> manyTill pMarkup (char '*'))

pEmph :: P Markup
pEmph = Emph <$> (char '_' *> manyTill pMarkup (char '_'))

pHeading :: P Markup
pHeading = try $ do
  col <- sourceColumn <$> getPosition
  lineStartCol <- stLineStartCol <$> getState
  guard (col == lineStartCol)
  lev <- length <$> many1 (char '=')
  void (many1 (char ' ')) <|> void (lookAhead endOfLine)
  -- Note: == hi _foo
  -- bar_ is parsed as a heading with "hi emph(foobar)"
  ms <- manyTill pMarkup (    void pEol
                          <|> pEndOfContent
                          <|> void (lookAhead (try (spaces *> pLabel)))
                          <|> void (lookAhead (char ']')))
  skipMany spaceChar
  pure $ Heading lev ms

pUrl :: P Markup
pUrl = try $ do
  prot <- T.pack <$> (string "http://" <|> string "https://")
  rest <- T.pack <$> pNonspaceWithBalancedBrackets 0 0 0
  pure $ Url $ prot <> rest

pNonspaceWithBalancedBrackets :: Int -> Int -> Int -> P [Char]
pNonspaceWithBalancedBrackets parens brackets braces =
  ((:) <$> char '(' <*> pNonspaceWithBalancedBrackets (parens + 1) brackets braces)
    <|> ((:) <$> (guard (parens > 0) *> char ')') <*> pNonspaceWithBalancedBrackets (parens - 1) brackets braces)
    <|> ((:) <$> char '[' <*> pNonspaceWithBalancedBrackets parens (brackets + 1) braces)
    <|> ((:) <$> (guard (brackets > 0) *> char ']') <*> pNonspaceWithBalancedBrackets parens (brackets - 1) braces)
    <|> ((:) <$> char '{' <*> pNonspaceWithBalancedBrackets parens brackets (braces + 1))
    <|> ((:) <$> (guard (braces > 0) *> char '}') *> pNonspaceWithBalancedBrackets parens brackets (braces - 1))
    <|> (:) <$> noneOf " \t\r\n()[]{}" <*> pNonspaceWithBalancedBrackets parens brackets braces
    <|> pure []

pText :: P Markup
pText = Text . mconcat <$> some
  ((do xs <- some alphaNum
       T.pack . (xs <>) <$>
             try (some (char '*' <|> char '_') <* lookAhead alphaNum)
        <|> pure (T.pack xs))
 <|> (T.pack <$> some (satisfy (\c -> not (isSpace c || isSpecial c))))
  )

pEscaped :: P Markup
pEscaped = Text . T.singleton <$> pEsc

pEsc :: P Char
pEsc =
  char '\\' *> (uniEsc <|> satisfy (not . isSpace))

pStrEsc :: P Char
pStrEsc =
  try $
    char '\\'
      *> ( uniEsc
             <|> ('\\' <$ char '\\')
             <|> ('"' <$ char '"')
             <|> ('\n' <$ char 'n')
             <|> ('\t' <$ char 't')
             <|> ('\r' <$ char 'r')
         )

uniEsc :: P Char
uniEsc = chr <$> (char 'u' *> char '{' *> hexnum <* char '}')
  where
    hexnum :: P Int
    hexnum = do
      ds <- many1 hexDigit
      case readMaybe ("0x" ++ ds) of
        Just i
          | i <= 1114112 -> pure i
          | otherwise -> pure 0xFFFD
        Nothing -> fail $ "Could not read hex number " ++ ds

pNbsp :: P Markup
pNbsp = Nbsp <$ char '~'

pDash :: P Markup
pDash = do
  void $ char '-'
  (Shy <$ char '?')
    <|> (char '-' *> ((EmDash <$ char '-') <|> pure EnDash))
    <|> pure (Text "-")

pEllipsis :: P Markup
pEllipsis = do
  void $ char '.'
  (Ellipsis <$ string "..") <|> pure (Text ".")

pQuote :: P Markup
pQuote = Quote <$> (char '\'' <|> char '"')

pLabelInContent :: P Markup
pLabelInContent = Code <$> getPosition <*> pLabel

pLabel :: P Expr
pLabel =
  Label . T.pack
    <$> try
      ( char '<'
          *> many1 (satisfy isIdentContinue <|> char '_' <|> char '.')
          <* char '>'
      )

pRef :: P Markup
pRef =
  Ref
    <$> (char '@' *> (T.pack <$> many1 (satisfy isIdentContinue <|> char '_')))
    <*> option (Literal Auto) (Block <$> pContent)

-- "If a character would continue the expression but should be interpreted as
-- text, the expression can forcibly be ended with a semicolon (;)."
-- "A few kinds of expressions are not compatible with the hashtag syntax
-- (e.g. binary operator expressions). To embed these into markup, you
-- can use parentheses, as in #(1 + 2)." Hence pBasicExpr not pExpr.
pHash :: P Markup
pHash = do
  void $ char '#'
  res <- Code <$> getPosition <*> pBasicExpr <* optional (sym ";")
  -- rewind if we gobbled space:
  mbBeforeSpace <- stBeforeSpace <$> getState
  case mbBeforeSpace of
    Nothing -> pure ()
    Just (pos, inp) -> do
      setPosition pos
      setInput inp
  pure res

isSpecial :: Char -> Bool
isSpecial '\\' = True
isSpecial '[' = True
isSpecial ']' = True
isSpecial '#' = True
isSpecial '-' = True
isSpecial '.' = True
isSpecial '"' = True
isSpecial '\'' = True
isSpecial '*' = True
isSpecial '_' = True
isSpecial '`' = True
isSpecial '$' = True
isSpecial '<' = True
isSpecial '>' = True
isSpecial '@' = True
isSpecial '/' = True
isSpecial ':' = True
isSpecial '~' = True
isSpecial '=' = True
isSpecial '(' = True -- so we don't gobble ( before URLs
isSpecial _ = False

pIdentifier :: P Identifier
pIdentifier = lexeme $ try $ do
  c <- satisfy isIdentStart
  cs <- many $ satisfy isIdentContinue
  pure $ Identifier $ T.pack (c : cs)

-- ident_start ::= unicode(XID_Start)
-- ID_Start characters are derived from the Unicode General_Category of
-- uppercase letters, lowercase letters, titlecase letters, modifier letters,
-- other letters, letter numbers, plus Other_ID_Start, minus Pattern_Syntax and
-- Pattern_White_Space code points.
isIdentStart :: Char -> Bool
isIdentStart c = c == '_' ||
  case generalCategory c of
    UppercaseLetter -> True
    LowercaseLetter -> True
    TitlecaseLetter -> True
    ModifierLetter -> True
    OtherLetter -> True
    LetterNumber -> True
    _ -> False

-- ident_continue ::= unicode(XID_Continue) | '-'
-- ID_Continue characters include ID_Start characters, plus characters having
-- the Unicode General_Category of nonspacing marks, spacing combining marks,
-- decimal number, connector punctuation, plus Other_ID_Continue, minus
-- Pattern_Syntax and Pattern_White_Space code points.
isIdentContinue :: Char -> Bool
isIdentContinue c =
  isIdentStart c
    || c == '-'
    || c == '_'
    || case generalCategory c of
      NonSpacingMark -> True
      SpacingCombiningMark -> True
      DecimalNumber -> True
      ConnectorPunctuation -> True
      _ -> False

pKeyword :: String -> P ()
pKeyword t = lexeme $ try $ string t *> notFollowedBy (satisfy isIdentContinue)

-- NOTE: there can be field access lookups that require identifiers like
-- 'not'.
-- keywords :: [Text]
-- keywords = ["none", "auto", "true", "false", "not", "and", "or", "let",
--             "set", "show", "wrap", "if", "else", "for", "in", "as", "while",
--             "break", "continue", "return", "import", "include", "from"]

pExpr :: P Expr
pExpr = buildExpressionParser operatorTable pBasicExpr

-- A basic expression excludes the unary and binary operators outside of parens,
-- but includes field access and function application. Needed for pHash.
pBasicExpr :: P Expr
pBasicExpr = buildExpressionParser basicOperatorTable pBaseExpr

pQualifiedIdentifier :: P Expr
pQualifiedIdentifier =
  buildExpressionParser (replicate 4 [fieldAccess]) pIdent

pBaseExpr :: P Expr
pBaseExpr =
  pLiteral
    <|> pKeywordExpr
    <|> pFuncExpr
    <|> pBindExpr
    <|> pIdent
    <|> pArrayExpr
    <|> pDictExpr
    <|> inParens pExpr
    <|> (Block . Content . (: []) <$> pEquation)
    <|> pLabel
    <|> pBlock

pLiteral :: P Expr
pLiteral =
  Literal
    <$> ( pNone
            <|> pAuto
            <|> pBoolean
            <|> pNumeric
            <|> pStr
        )

fieldAccess :: Operator Text PState Identity Expr
fieldAccess = Postfix (FieldAccess <$> try (sym "." *> pIdent))

-- don't allow space after .
restrictedFieldAccess :: Operator Text PState Identity Expr
restrictedFieldAccess = Postfix (FieldAccess <$> try (char '.' *> pIdent))

functionCall :: Operator Text PState Identity Expr
functionCall =
  Postfix
    ( do
        mbBeforeSpace <- stBeforeSpace <$> getState
        -- NOTE: can't have space before () or [] arg in a
        -- function call! to prevent bugs with e.g. 'if 2<3 [...]'.
        guard $ isNothing mbBeforeSpace
        args <- pArgs
        pure $ \expr -> FuncCall expr args
    )

-- The reason we cycle field access and function call
-- is that a postfix operator will not
-- be repeatable at the same precedence level...see docs for
-- buildExpressionParser.
basicOperatorTable :: [[Operator Text PState Identity Expr]]
basicOperatorTable =
  take 16 (cycle [[restrictedFieldAccess], [functionCall]])

operatorTable :: [[Operator Text PState Identity Expr]]
operatorTable =
  -- precedence 8 (real field access, perhaps  with space after .)
  take 12 (cycle [[fieldAccess], [functionCall]])
    ++
    -- precedence 7 (repeated because of parsec's quirks with postfix, prefix)
    replicate 6 [Postfix (ToPower <$> try (char 'e' *> notFollowedBy letter *> pExpr))]
    ++ replicate 6 [Prefix (Negated <$ op "-"), Prefix (id <$ op "+")]
    ++ [
         -- precedence 6
         [ Infix (Times <$ op "*") AssocLeft,
           Infix (Divided <$ op "/") AssocLeft
         ],
         -- precedence 5
         [ Infix (Plus <$ op "+") AssocLeft,
           Infix (Minus <$ op "-") AssocLeft
         ],
         -- precedence 4
         [ Infix (Equals <$ op "==") AssocLeft,
           Infix ((\x y -> Not (Equals x y)) <$ op "!=") AssocLeft,
           Infix (LessThan <$ op "<") AssocLeft,
           Infix (LessThanOrEqual <$ op "<=") AssocLeft,
           Infix (GreaterThan <$ op ">") AssocLeft,
           Infix (GreaterThanOrEqual <$ op ">=") AssocLeft,
           Infix (InCollection <$ pKeyword "in") AssocLeft,
           Infix
             ( (\x y -> Not (InCollection x y))
                 <$ try (pKeyword "not" *> pKeyword "in")
             )
             AssocLeft
         ],
         -- precedence 3
         [ Prefix (Not <$ pKeyword "not"),
           Infix (And <$ pKeyword "and") AssocLeft
         ],
         -- precedence 2
         [ Infix (Or <$ pKeyword "or") AssocLeft
         ],
         -- precedence 1
         [ Infix (Assign <$ op "=") AssocRight,
           Infix ((\x y -> Assign x (Plus x y)) <$ op "+=") AssocRight,
           Infix ((\x y -> Assign x (Minus x y)) <$ op "-=") AssocRight,
           Infix ((\x y -> Assign x (Times x y)) <$ op "*=") AssocRight,
           Infix ((\x y -> Assign x (Divided x y)) <$ op "/=") AssocRight
         ]
       ]

pNone :: P Literal
pNone = None <$ pKeyword "none"

pAuto :: P Literal
pAuto = Auto <$ pKeyword "auto"

pBoolean :: P Literal
pBoolean =
  (Boolean True <$ pKeyword "true") <|> (Boolean False <$ pKeyword "false")

pNumber :: P (Either Integer Double)
pNumber = try $ do
  pref <- string "0b" <|> string "0x" <|> string "0o" <|> pure ""
  case pref of
    "0b" -> do
      nums <- many1 ((1 <$ char '1') <|> (0 <$ char '0'))
      pure $ Left $ sum $ zipWith (*) (reverse nums) (map (2 ^) [(0 :: Integer) ..])
    "0x" -> do
      num <- many1 hexDigit
      case readMaybe ("0x" ++ num) of
        Just (i :: Integer) -> pure $ Left i
        _ -> fail $ "could not read " <> num <> " as hex digits"
    "0o" -> do
      num <- many1 octDigit
      case readMaybe ("0o" ++ num) of
        Just (i :: Integer) -> pure $ Left i
        _ -> fail $ "could not read " <> num <> " as octal digits"
    _ -> do
      as <- many1 digit <|> ("0" <$ lookAhead (try (char '.' *> digit)))
      pe <- option [] $ string "."
      bs <- many digit
      es <-
        option
          ""
          ( do
              void $ try $ char 'e' *> lookAhead (digit <|> char '-')
              minus <- option [] $ count 1 (char '-')
              ds <- many1 digit
              pure ("e" ++ minus ++ ds)
          )
      let num = pref ++ as ++ pe ++ bs ++ es
      case readMaybe num of
        Just (i :: Integer) -> pure $ Left i
        Nothing ->
          case readMaybe num of
            Just (d :: Double) -> pure $ Right d
            Nothing -> fail $ "could not read " <> num <> " as integer"

pNumeric :: P Literal
pNumeric = lexeme $ do
  result <- pNumber
  ( do
      unit <- pUnit
      case result of
        Left i -> pure $ Numeric (fromIntegral i) unit
        Right d -> pure $ Numeric d unit
    )
    <|> case result of
      Left i -> pure $ Int i
      Right d -> pure $ Float d

pStr :: P Literal
pStr = lexeme $ do
  void $ char '"'
  String . T.pack <$> manyTill (pStrEsc <|> satisfy (/= '"')) (char '"')

pUnit :: P Unit
pUnit =
  (Percent <$ sym "%")
    <|> (Pt <$ pKeyword "pt")
    <|> (Mm <$ pKeyword "mm")
    <|> (Cm <$ pKeyword "cm")
    <|> (In <$ pKeyword "in")
    <|> (Deg <$ pKeyword "deg")
    <|> (Rad <$ pKeyword "rad")
    <|> (Em <$ pKeyword "em")
    <|> (Fr <$ pKeyword "fr")

pIdent :: P Expr
pIdent = Ident <$> pIdentifier

pBlock :: P Expr
pBlock = Block <$> (pCodeBlock <|> pContent)

pCodeBlock :: P Block
pCodeBlock = CodeBlock <$> inBraces pCode

pCode :: P [Expr]
pCode = sepEndBy pExpr (void (sym ";") <|> ws)

-- content-block ::= '[' markup ']'
pContent :: P Block
pContent = do
  void $ char '['
  col <- sourceColumn <$> getPosition
  oldLineStartCol <- stLineStartCol <$> getState
  updateState $ \st ->
    st
      { stLineStartCol = col,
        stContentBlockNesting =
          stContentBlockNesting st + 1
      }
  ms <- manyTill pMarkup (char ']')
  ws
  updateState $ \st ->
    st
      { stLineStartCol = oldLineStartCol,
        stContentBlockNesting =
          stContentBlockNesting st - 1
      }
  pure $ Content ms

pEndOfContent :: P ()
pEndOfContent =
  eof <|> do
    blockNesting <- stContentBlockNesting <$> getState
    if blockNesting > 0
      then void (lookAhead (char ']'))
      else mzero

-- array-expr ::= '(' ((expr ',') | (expr (',' expr)+ ','?))? ')'
pArrayExpr :: P Expr
pArrayExpr =
  try $
    inParens $
      ( do
          v <- pSpread <|> (Reg <$> pExpr)
          vs <- many $ try $ sym "," *> (pSpread <|> (Reg <$> pExpr))
          if null vs
            then void $ sym ","
            else optional $ void $ sym ","
          pure $ Array (v : vs)
      )
        <|> (Array [] <$ optional (void $ sym ","))

-- dict-expr ::= '(' (':' | (pair (',' pair)* ','?)) ')'
-- pair ::= (ident | str) ':' expr
pDictExpr :: P Expr
pDictExpr = try $ inParens (pEmptyDict <|> pNonemptyDict)
  where
    pEmptyDict = Dict mempty <$ sym ":"
    pNonemptyDict = Dict <$> sepEndBy1 (pSpread <|> pPair) (sym ",")
    pPair = Reg <$> ((,) <$> pExpr <*> try (sym ":" *> pExpr))

pSpread :: P (Spreadable a)
pSpread = try $ string ".." *> (Spr <$> pExpr)

-- func-expr ::= (params | ident) '=>' expr
pFuncExpr :: P Expr
pFuncExpr = try $ FuncExpr <$> pParamsOrIdent <*> (sym "=>" *> pExpr)
  where
    pParamsOrIdent =
      pParams
        <|> (do i <- pIdentifier
                if i == "_"
                   then pure [SkipParam]
                   else pure [NormalParam i])

pKeywordExpr :: P Expr
pKeywordExpr =
  pLetExpr
    <|> pSetExpr
    <|> pShowExpr
    <|> pIfExpr
    <|> pWhileExpr
    <|> pForExpr
    <|> pImportExpr
    <|> pIncludeExpr
    <|> pBreakExpr
    <|> pContinueExpr
    <|> pReturnExpr

-- args ::= ('(' (arg (',' arg)* ','?)? ')' content-block*) | content-block+
pArgs :: P [Arg]
pArgs = do
  void $ lookAhead (char '(' <|> char '[')
  args <- option [] $ inParens $ sepEndBy pArg (sym ",")
  blocks <- many $ do
    -- make sure we haven't had a space
    skippedSpaces <- isJust . stBeforeSpace <$> getState
    if skippedSpaces
      then mzero
      else do
        Content ms <- pContent
        pure ms
  pure $ args ++ map BlockArg blocks

-- arg ::= (ident ':')? expr
pArg :: P Arg
pArg = pKeyValArg <|> pSpreadArg <|> pNormalArg
  where
    pKeyValArg = KeyValArg <$> try (pIdentifier <* sym ":") <*> pExpr
    pNormalArg =
      NormalArg
        <$> ((Block . Content . (: []) <$> lexeme (pRawBlock <|> pRawInline)) <|> pExpr)
    pSpreadArg = SpreadArg <$> try (string ".." *> pExpr)

-- params ::= '(' (param (',' param)* ','?)? ')'
pParams :: P [Param]
pParams = inParens $ sepEndBy pParam (sym ",")

-- param ::= ident (':' expr)?
pParam :: P Param
pParam =
  pSinkParam <|> pDestructuringParam <|> pNormalOrDefaultParam <|> pSkipParam
  where
    pSinkParam =
      SinkParam
        <$> try
          ( sym ".."
              *> option Nothing (Just <$> pIdentifier)
          )
    pSkipParam = SkipParam <$ sym "_"
    pNormalOrDefaultParam = do
      i <- pIdentifier
      (DefaultParam i <$> (sym ":" *> pExpr)) <|> pure (NormalParam i)
    pDestructuringParam = do
      DestructuringBind parts <- pDestructuringBind
      pure $ DestructuringParam parts

pBind :: P Bind
pBind = pBasicBind <|> pDestructuringBind

pBasicBind :: P Bind
pBasicBind = BasicBind <$> try (pBindIdentifier <|> inParens pBindIdentifier)

pBindIdentifier :: P (Maybe Identifier)
pBindIdentifier = do
  ident <- pIdentifier
  if ident == "_"
     then pure Nothing
     else pure $ Just ident

pDestructuringBind :: P Bind
pDestructuringBind =
  inParens $
    DestructuringBind <$> (pBindPart `sepEndBy` (sym ","))
  where
    pBindPart = do
      sink <- option False $ True <$ string ".."
      if sink
        then do
          ident <- option Nothing pBindIdentifier -- ..
          pure $ Sink ident
        else do
          ident <- pBindIdentifier
          case ident of
            Nothing -> pure (Simple ident)
            Just key ->
              (WithKey key <$> (sym ":" *> pBindIdentifier))
                <|> pure (Simple ident)

-- let-expr ::= 'let' ident params? '=' expr
pLetExpr :: P Expr
pLetExpr = do
  pKeyword "let"
  bind <- pBind
  case bind of
    BasicBind mbname -> do
      mbparams <- option Nothing $ Just <$> pParams
      mbexpr <- option Nothing $ Just <$> (sym "=" *> pExpr)
      case (mbparams, mbexpr, mbname) of
        (Nothing, Nothing, _) -> pure $ Let bind (Literal None)
        (Nothing, Just expr, _) -> pure $ Let bind expr
        (Just params, Just expr, Just name) -> pure $ LetFunc name params expr
        (Just _, Just _, Nothing) -> fail "expected name for function"
        (Just _, Nothing, _) -> fail "expected expression for let binding"
    _ -> Let bind <$> (sym "=" *> pExpr)

-- set-expr ::= 'set' expr args
pSetExpr :: P Expr
pSetExpr = do
  oldAllowNewlines <- stAllowNewlines <$> getState
  -- see #23 -- 'set' doesn't go with 'if' unless it's on the same line
  updateState $ \st -> st {stAllowNewlines = 0}
  set <- pKeyword "set" *> (Set <$> pQualifiedIdentifier <*> pArgs)
  updateState $ \st -> st {stAllowNewlines = oldAllowNewlines}
  addCondition <- option id $ pKeyword "if" *> ((\c x -> If [(c, x)]) <$> pExpr)
  pure $ addCondition set

pShowExpr :: P Expr
pShowExpr = do
  pKeyword "show"
  from <- (Nothing <$ sym ":") <|> Just <$> (pBasicExpr <* sym ":")
  to <- pBasicExpr
  pure $ Show from to

-- if-expr ::= 'if' expr block ('else' 'if' expr block)* ('else' block)?
pIfExpr :: P Expr
pIfExpr = do
  a <- pIf
  as <- many $ try (pKeyword "else" *> pIf)
  finalElse <-
    option [] $
      -- we represent the final "else" as a conditional with expr True:
      (: []) . (Literal (Boolean True),) <$> (pKeyword "else" *> pBlock)
  return $ If (a : as ++ finalElse)
  where
    pIf = pKeyword "if" *> ((,) <$> pExpr <*> pBlock)

-- while-expr ::= 'while' expr block
pWhileExpr :: P Expr
pWhileExpr = pKeyword "while" *> (While <$> pExpr <*> pBlock)

-- for-expr ::= 'for' bind 'in' expr block
pForExpr :: P Expr
pForExpr =
  pKeyword "for" *> (For <$> pBind <*> (pKeyword "in" *> pExpr) <*> pBlock)

pImportExpr :: P Expr
pImportExpr = pKeyword "import" *> (Import <$> pExpr <*> pImportItems)
  where
    pImportItems =
      option NoIdentifiers $
        sym ":"
          *> ( (AllIdentifiers <$ sym "*")
                 <|> (SomeIdentifiers <$> sepEndBy pIdentifier (sym ","))
             )

pBreakExpr :: P Expr
pBreakExpr = Break <$ pKeyword "break"

pContinueExpr :: P Expr
pContinueExpr = Continue <$ pKeyword "continue"

pReturnExpr :: P Expr
pReturnExpr = do
  pos <- getPosition
  pKeyword "return"
  pos' <- getPosition
  if sourceLine pos' > sourceLine pos
    then pure $ Return Nothing
    else Return <$> (option Nothing (Just <$> pExpr))

pIncludeExpr :: P Expr
pIncludeExpr = Include <$> (pKeyword "include" *> pExpr)

pBindExpr :: P Expr
pBindExpr =
  Binding <$> try (pBind <* lookAhead (op "="))
