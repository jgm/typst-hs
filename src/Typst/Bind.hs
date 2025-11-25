{-# LANGUAGE RankNTypes #-}

module Typst.Bind (destructuringBind, doBind) where

import Control.Monad.State
import qualified Data.Map.Ordered as OM
import qualified Data.Vector as V
import Typst.Syntax
import Typst.Types


doBind ::
  Monad m =>
  (forall m'. Monad m' => Identifier -> Val -> MP m' ()) ->
  Bind ->
  Val ->
  MP m ()
doBind setIdentifier (BasicBind (Just ident)) val = setIdentifier ident val
doBind _ (BasicBind Nothing) _ = pure ()
doBind setIdentifier (DestructuringBind parts) val =
  destructuringBind setIdentifier parts val

destructuringBind ::
  Monad m =>
  (forall m'. Monad m' => Identifier -> Val -> MP m' ()) ->
  [BindPart] ->
  Val ->
  MP m ()
destructuringBind setIdentifier parts val = do
  let isSink Sink {} = True
      isSink _ = False
  let (fronts, rest) = break isSink parts
  let (sinks, backs) = span isSink rest
  mbsink <- case sinks of
    [Sink s] -> pure s
    [] -> pure Nothing
    _ -> fail "Bind cannot contain multiple sinks"
  case val of
    VDict m ->
      evalStateT (destructureDict setIdentifier fronts backs mbsink) m
    VArray v ->
      evalStateT (destructureArray setIdentifier fronts backs mbsink) v
    _ -> fail "Only Array or Dictionary values can be destructured"

destructureDict ::
  Monad m =>
  (forall m'. Monad m' => Identifier -> Val -> MP m' ()) ->
  [BindPart] ->
  [BindPart] ->
  Maybe Identifier ->
  StateT (OM.OMap Identifier Val) (MP m) ()
destructureDict setIdentifier fronts backs mbsink = do
  mapM_ handleDictBind (fronts ++ backs)
  case mbsink of
    Just i -> get >>= lift . setIdentifier i . VDict
    Nothing -> pure ()
  where
    handleDictBind :: Monad m => BindPart -> StateT (OM.OMap Identifier Val) (MP m) ()
    handleDictBind (Sink {}) = fail "Bind cannot contain multiple sinks"
    handleDictBind (Simple (BasicBind (Just i))) = do
      m <- get
      case OM.lookup i m of
        Nothing ->
          fail $ "Destructuring key not found in dictionary: " <> show i
        Just v -> do
          put $ OM.delete i m
          lift $ setIdentifier i v
    handleDictBind (Simple _) = fail "cannot destructure unnamed pattern from dictionary"
    handleDictBind (WithKey key bind) = do
      m <- get
      case OM.lookup key m of
        Nothing ->
          fail $ "Destructuring key not found in dictionary: " <> show key
        Just v -> do
          put $ OM.delete key m
          lift $ doBind setIdentifier bind v


destructureArray ::
  Monad m =>
  (forall m'. Monad m' => Identifier -> Val -> MP m' ()) ->
  [BindPart] ->
  [BindPart] ->
  Maybe Identifier ->
  StateT (V.Vector Val) (MP m) ()
destructureArray setIdentifier fronts backs mbsink = do
  mapM_ handleFrontBind fronts
  mapM_ handleBackBind (reverse backs)
  case mbsink of
    Just i -> get >>= lift . setIdentifier i . VArray
    Nothing -> pure ()
  where
    handleFrontBind :: Monad m => BindPart -> StateT (V.Vector Val) (MP m) ()
    handleFrontBind (Sink {}) = fail "Bind cannot contain multiple sinks"
    handleFrontBind (WithKey {}) = fail "Cannot destructure array with key"
    handleFrontBind (Simple bind) = do
      v <- get
      case V.uncons v of
        Nothing -> fail "Array does not contain enough elements to destructure"
        Just (x, v') -> do
          put v'
          lift $ doBind setIdentifier bind x

    handleBackBind :: Monad m => BindPart -> StateT (V.Vector Val) (MP m) ()
    handleBackBind (Sink {}) = fail "Bind cannot contain multiple sinks"
    handleBackBind (WithKey {}) = fail "Cannot destructure array with key"
    handleBackBind (Simple bind) = do
      v <- get
      case V.unsnoc v of
        Nothing -> fail "Array does not contain enough elements to destructure"
        Just (v', x) -> do
          put v'
          lift $ doBind setIdentifier bind x
