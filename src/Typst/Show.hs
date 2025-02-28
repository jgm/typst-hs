{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=140 #-}

module Typst.Show (applyShowRules) where

import Control.Monad (foldM)
import Data.Array ((!))
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (getState, updateState, (<|>))
import qualified Text.Regex.TDFA as TDFA
import Typst.Regex (RE (..), makeLiteralRE)
import Typst.Syntax
import Typst.Types

import Debug.Trace

applyShowRules :: Monad m => Seq Content -> MP m (Seq Content)
applyShowRules cs = do
  rules <- evalShowRules <$> getState
  foldM (tryShowRules rules) mempty cs

withoutShowRule :: Monad m => ShowRule -> MP m a -> MP m a
withoutShowRule rule pa = do
  oldShowRules <- evalShowRules <$> getState
  updateState $ \st ->
    st { evalShowRules = filter (/= rule) (evalShowRules st) }
  res <- pa
  updateState $ \st -> st {evalShowRules = oldShowRules}
  pure res

-- By experiment, it seems that show rules work this way:
-- the first (i.e. most recently defined) one to match a given element
-- are applied first.
tryShowRules ::
  Monad m =>
  [ShowRule] ->
  Seq Content ->
  Content ->
  MP m (Seq Content)
tryShowRules [] cs c = pure $ cs Seq.|> c
tryShowRules (r@(ShowRule ident sel f) : rs) cs c = do
  case (sel, c) of
    (SelectString s, Txt t) ->
      ( do
          re <- makeLiteralRE s
          withoutShowRule r
            ((cs <>) <$> (replaceRegexContent re t f >>= applyShowRules))
      )
        <|> tryShowRules rs cs c
    (SelectRegex re, Txt t) ->
      ( withoutShowRule r
          ((cs <>) <$> (replaceRegexContent re t f >>= applyShowRules))
      )
        <|> tryShowRules rs cs c
    (SelectLabel s, elt@(Elt _ _ fields))
      | Just (VLabel s') <- M.lookup "label" fields,
        s' == s ->
          withoutShowRule r ((cs <>) <$> (f elt >>= applyShowRules))
    (SelectElement name fields, elt@(Elt name' _ fields'))
      | name == name',
        fieldsMatch fields fields' ->
          withoutShowRule r $ (cs <>) <$> (f elt >>= applyShowRules)
    (SelectOr _sel1 _sel2, _elt) ->
      fail "or is not yet implemented for select"
    (SelectAnd _sel1 _sel2, _elt) ->
      fail "and is not yet implemented for select"
    (SelectBefore _sel1 _sel2, _elt) ->
      fail "before is not yet implemented for select"
    (SelectAfter _sel1 _sel2, _elt) ->
      fail "after is not yet implemented for select"
    _ -> tryShowRules rs cs c

fieldsMatch :: [(Identifier, Val)] -> (M.Map Identifier Val) -> Bool
fieldsMatch [] _ = True
fieldsMatch ((k, v) : rest) m =
  ( case M.lookup k m of
      Just v' -> v == v'
      Nothing -> False
  )
    && fieldsMatch rest m

replaceRegexContent ::
  Monad m =>
  RE ->
  Text ->
  (forall m'. Monad m' => Content -> MP m' (Seq Content)) ->
  MP m (Seq Content)
replaceRegexContent (RE _ re) strIn f =
  let matches = map (! 0) (TDFA.matchAll re strIn)
      go _i str [] = pure $ Seq.singleton (Txt str)
      go i str ((off, len) : rest) =
        let i' = off + len
            before = T.take (off - i) str
            matched = T.take len (T.drop (off - i) str)
            after = T.drop (i' - i) str
         in seq i' $
              (\x y -> Seq.singleton (Txt before) <> x <> y)
                <$> f (Txt matched)
                <*> go i' after rest
   in go 0 strIn matches
