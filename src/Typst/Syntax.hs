{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Typst.Syntax
  ( Markup (..),
    Identifier (..),
    Imports (..),
    Arg (..),
    Param (..),
    Bind (..),
    BindPart (..),
    Literal (..),
    Block (..),
    Spreadable (..),
    Expr (..),
    Unit (..),
  )
where

import Data.Data (Data, Typeable)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (SourcePos)

data Markup
  = Space
  | SoftBreak
  | HardBreak
  | ParBreak
  | Text Text
  | Nbsp
  | Shy
  | EmDash
  | EnDash
  | Ellipsis
  | Quote Char
  | Ref Text Expr
  | Equation Bool [Markup] -- Bool is True for displayed format
  | Strong [Markup]
  | Emph [Markup]
  | Bracketed [Markup]
  | RawInline Text
  | RawBlock Text Text
  | Heading Int [Markup]
  | Url Text
  | BulletListItem [Markup]
  | EnumListItem (Maybe Int) [Markup]
  | DescListItem [Markup] [Markup]
  | Code SourcePos Expr
  | Comment
  | MAlignPoint
  | MFrac Markup Markup
  | MAttach (Maybe Markup) (Maybe Markup) Markup -- bottom then top
  | MGroup (Maybe Text) (Maybe Text) [Markup] -- maybes are open/cloes delims
  deriving (Show, Ord, Eq, Data, Typeable)

newtype Identifier = Identifier Text
  deriving (Show, Eq, Ord, Data, Typeable, Semigroup, Monoid)

instance IsString Identifier where
  fromString = Identifier . T.pack

data Arg
  = KeyValArg Identifier Expr
  | NormalArg Expr
  | ArrayArg [[Markup]]
  | SpreadArg Expr
  | BlockArg [Markup]
  deriving (Show, Ord, Eq, Data, Typeable)

data Param
  = DefaultParam Identifier Expr
  | NormalParam Identifier
  | DestructuringParam [BindPart]
  | SinkParam (Maybe Identifier)
  | SkipParam -- _
  deriving (Show, Ord, Eq, Data, Typeable)

data Bind
  = BasicBind (Maybe Identifier)
  | DestructuringBind [BindPart]
  deriving (Show, Ord, Eq, Data, Typeable)

data BindPart
  = Simple (Maybe Identifier)
  | WithKey Identifier (Maybe Identifier)
  | Sink (Maybe Identifier)
  deriving (Show, Ord, Eq, Data, Typeable)

data Unit = Pt | Mm | Cm | In | Deg | Rad | Em | Fr | Percent
  deriving (Show, Ord, Eq, Data, Typeable)

data Literal
  = String Text
  | Boolean Bool
  | Float Double
  | Int Integer
  | Numeric Double Unit
  | None
  | Auto
  deriving (Show, Ord, Eq, Data, Typeable)

data Block
  = Content [Markup]
  | CodeBlock [Expr]
  deriving (Show, Ord, Eq, Data, Typeable)

data Spreadable a =
    Spr Expr
  | Reg a
  deriving (Show, Ord, Eq, Data, Typeable)

-- binary-op ::=
--   '+' | '-' | '*' | '/' | 'and' | 'or' | '==' | '!=' |
--   '<' | '<=' | '>' | '>=' | '=' | 'in' | ('not' 'in') |
--   '+=' | '-=' | '*=' | '/='
data Expr
  = Literal Literal
  | Negated Expr
  | ToPower Expr Expr
  | Times Expr Expr
  | Divided Expr Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Equals Expr Expr
  | LessThan Expr Expr
  | LessThanOrEqual Expr Expr
  | GreaterThan Expr Expr
  | GreaterThanOrEqual Expr Expr
  | InCollection Expr Expr
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Assign Expr Expr
  | Ident Identifier
  | FuncCall Expr [Arg]
  | FuncExpr [Param] Expr
  | FieldAccess Expr Expr
  | Group Expr
  | Array [Spreadable Expr]
  | Dict [Spreadable (Identifier, Expr)]
  | Binding Bind
  | Let Bind Expr
  | LetFunc Identifier [Param] Expr
  | Set Expr [Arg]
  | Show (Maybe Expr) Expr
  | If [(Expr, Expr)]
  | While Expr Expr
  | For Bind Expr Expr
  | Block Block
  | Import Expr Imports
  | Include Expr
  | Return (Maybe Expr)
  | Label Text
  | Break
  | Continue
  deriving (Show, Ord, Eq, Data, Typeable)

data Imports
  = AllIdentifiers
  | SomeIdentifiers [Identifier]
  | NoIdentifiers
  deriving (Show, Ord, Eq, Data, Typeable)
