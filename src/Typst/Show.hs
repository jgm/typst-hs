{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=140 #-}

module Typst.Show (applyShowRules) where

import Data.Array ((!))
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (getState, updateState)
import qualified Text.Regex.TDFA as TDFA
import Typst.Regex (RE (..), makeLiteralRE)
import Typst.Syntax
import Typst.Types

-- import Debug.Trace

applyShowRules :: Monad m => Seq Content -> MP m (Seq Content)
applyShowRules cs = do
  rules <- evalShowRules <$> getState
  foldMap (tryShowRules rules) cs

-- By experiment, it seems that show rules work this way:
-- the first (i.e. most recently defined) one to match a given element
-- are applied first.
tryShowRules ::
  Monad m =>
  [ShowRule] ->
  Content ->
  MP m (Seq Content)
tryShowRules (r:rest) e@(Elt "text" pos fields) =
  case M.lookup "body" fields of
    Just (VContent cs) -> do
      cs' <- foldMap (tryShowRules (r:rest)) cs
      pure $ Seq.singleton $
        Elt "text" pos (M.insert "body" (VContent cs') fields)
    _ -> applyShowRule r e >>= foldMap (tryShowRules rest)
tryShowRules rs c =
  case rs of
    [] -> pure $ Seq.singleton c
    (r:rest) -> applyShowRule r c >>= foldMap (tryShowRules rest)
-- TODO recursive applyShowRules?

withoutShowRule :: Monad m => ShowRule -> MP m a -> MP m a
withoutShowRule rule pa = do
  oldShowRules <- evalShowRules <$> getState
  updateState $ \st ->
    st { evalShowRules = filter (/= rule) (evalShowRules st) }
  res <- pa
  updateState $ \st -> st {evalShowRules = oldShowRules}
  pure res

applyShowRule :: Monad m => ShowRule -> Content -> MP m (Seq Content)
applyShowRule rule@(ShowRule _ sel f) c = withoutShowRule rule $ do
  case (sel, c) of
    (SelectString s, Txt t) | s `T.isInfixOf` t -> do
      re <- makeLiteralRE s
      replaceRegexContent re t f
    (SelectRegex re@(RE _ re'), Txt t) | not (null (TDFA.matchAll re' t)) -> do
      replaceRegexContent re t f
    (SelectLabel s, elt@(Elt _ _ fields))
      | Just (VLabel s') <- M.lookup "label" fields
      , s' == s
      -> f elt
    (SelectElement name fields, elt@(Elt name' _ fields'))
      | name == name' && fieldsMatch fields fields'
      -> f elt
    (_, cont) -> pure (Seq.singleton cont)
    -- TODO not implemented: SelectOr, SelectAnd, SelectBefore, SelectAfter

fieldsMatch :: [(Identifier, Val)] -> M.Map Identifier Val -> Bool
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
