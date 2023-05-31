{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Typst.Module.Math (
    mathModule
  ) where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Typst.Util
import Typst.Types
import Text.Parsec (getPosition)

mathModule :: M.Map Identifier Val
mathModule =
  M.fromList [
      makeElement (Just "math") "frac" [("num", One TContent),
                          ("denom", One TContent)]
    , makeElement (Just "math") "accent" [("base", One TContent),
                            ("accent", One (TContent :|: TString :|: TSymbol))]
    , makeElement (Just "math") "attach" [("base", One TContent)]
    , makeElement (Just "math") "scripts" [("body", One TContent)]
    , makeElement (Just "math") "limits" [("body", One TContent)]
    , makeElement (Just "math") "binom" [("upper", One TContent),
                           ("lower", One TContent)]
    , makeElement (Just "math") "cancel" [("body", One TContent)]
    , makeElement (Just "math") "equation" [("body", One TContent)]
    , makeElement (Just "math") "root" [("index", One (TNone :|: TContent :|: TInteger :|: TRatio)),
                          ("radicand", One TContent)]
    , makeElement (Just "math") "sqrt" [("radicand", One TContent)]
    , makeElement (Just "math") "cases" [("children", Many TContent)]
    , makeElement (Just "math") "lr" [("body", One TContent)]
    , makeElement (Just "math") "abs" [("body", One TContent)]
    , makeElement (Just "math") "norm" [("body", One TContent)]
    , makeElement (Just "math") "floor" [("body", One TContent)]
    , makeElement (Just "math") "ceil" [("body", One TContent)]
    , ("mat", matrixElement)
    , makeElement (Just "math") "round" [("body", One TContent)]
    , makeElement (Just "math") "serif" [("body", One TContent)]
    , makeElement (Just "math") "sans" [("body", One TContent)]
    , makeElement (Just "math") "frak" [("body", One TContent)]
    , makeElement (Just "math") "mono" [("body", One TContent)]
    , makeElement (Just "math") "bb" [("body", One TContent)]
    , makeElement (Just "math") "cal" [("body", One TContent)]
    , makeElement (Just "math") "cal" [("body", One TContent)]
    , makeElement (Just "math") "upright" [("body", One TContent)]
    , makeElement (Just "math") "italic" [("body", One TContent)]
    , makeElement (Just "math") "bold" [("body", One TContent)]
    , makeElement (Just "math") "op" [("text", One TString)]
    , makeElement (Just "math") "underline" [("body", One TContent)]
    , makeElement (Just "math") "overline" [("body", One TContent)]
    , makeElement (Just "math") "underbrace" [("body", One TContent),
                                ("annotation", One (TNone :|: TContent))]
    , makeElement (Just "math") "overbrace" [("body", One TContent),
                               ("annotation", One (TNone :|: TContent))]
    , makeElement (Just "math") "underbracket" [("body", One TContent),
                                  ("annotation", One (TNone :|: TContent))]
    , makeElement (Just "math") "overbracket" [("body", One TContent),
                                 ("annotation", One (TNone :|: TContent))]
    , makeElement (Just "math") "vec" [("children", Many TContent)]
    , makeElement (Just "math") "alignpoint" [] -- not a real element, but needed internally
    , makeElement (Just "math") "dif" [] -- not a real element
    , makeElement (Just "math") "Dif" [] -- not a real element
    ] <> M.map (VContent . Seq.singleton) predefinedOperators
      <> M.map (VContent . Seq.singleton) spaceConstants

matrixElement :: Val
matrixElement = VFunction (Just "mat") mempty $ Function $ \args -> do
  pos <- getPosition
  -- get array args first
  let isArray (VArray{}) = True
      isArray _ = False
  let (as, bs) = span isArray (positional args)
  let rows = if null bs
                then as
                else as ++ [VArray (V.fromList bs)]
  -- then any leftovers
  let fields = M.fromList [ ("rows", VArray (V.fromList rows)) ]
  pure $ VContent . Seq.singleton $ Elt "math.mat" (Just pos) fields

spaceConstants :: M.Map Identifier Content
spaceConstants =
  [ ("thin", Txt "\8201")
  , ("thick", Txt "\8197")
  , ("med", Txt "\8287")
  , ("quad", Txt "\8195")
  ]

predefinedOperators :: M.Map Identifier Content
predefinedOperators = M.fromList $
  map (\t -> (Identifier t, Elt "math.op" Nothing
               [("text", VString t), ("limits", VBoolean True)]))
        ["limsup", "liminf", "det", "gcd", "inf", "lim", "max", "min",
         "Pr", "sup"]
  ++ map (\t -> (Identifier t, Elt "math.op" Nothing
                  [("text", VString t), ("limits", VBoolean False)]))
        ["arccos", "arcsin", "arctan", "arg", "cos", "cosh",
         "cot", "ctg", "coth", "csc", "deg", "dim", "exp",
         "hom", "mod", "ker", "lg", "ln", "log", "sec", "sin",
         "sinc", "sinh", "tan", "tg", "tanh"]
