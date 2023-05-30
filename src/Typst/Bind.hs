{-# LANGUAGE RankNTypes #-}
module Typst.Bind
  ( destructuringBind )
where
import Typst.Types
import Typst.Syntax
import Control.Monad.State
import qualified Data.Map.Ordered as OM
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

destructuringBind
  :: MonadFail m => (forall m'. MonadFail m' => Identifier -> Val -> MP m' ())
  -> [BindPart] -> Val -> MP m ()
destructuringBind setIdentifier parts val = do
  let isSink Sink{} = True
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

destructureDict
  :: MonadFail m => (forall m'. MonadFail m' => Identifier -> Val -> MP m' ())
  -> [BindPart] -> [BindPart] -> Maybe Identifier
  -> StateT (OM.OMap Identifier Val) (MP m) ()
destructureDict setIdentifier fronts backs mbsink = do
  mapM_ handleDictBind (fronts ++ backs)
  case mbsink of
    Just i -> get >>= lift . setIdentifier i . VDict
    Nothing -> pure ()
 where
   handleDictBind :: MonadFail m => BindPart -> StateT (OM.OMap Identifier Val) (MP m) ()
   handleDictBind (Sink{}) = fail "Bind cannot contain multiple sinks"
   handleDictBind (Simple Nothing) = pure ()
   handleDictBind (Simple (Just i)) = do
     m <- get
     case OM.lookup i m of
       Nothing ->
         fail $ "Destructuring key not found in dictionary: " <> show i
       Just v -> do
         put $ OM.delete i m
         lift $ setIdentifier i v
   handleDictBind (WithKey key mbident) = do
     m <- get
     case OM.lookup key m of
       Nothing ->
         fail $ "Destructuring key not found in dictionary: " <> show key
       Just v -> do
         put $ OM.delete key m
         lift $ setIdentifier (fromMaybe key mbident) v

destructureArray
  :: MonadFail m => (forall m'. MonadFail m' => Identifier -> Val -> MP m' ())
  -> [BindPart] -> [BindPart] -> Maybe Identifier
  -> StateT (V.Vector Val) (MP m) ()
destructureArray setIdentifier fronts backs mbsink = do
  mapM_ handleFrontBind fronts
  mapM_ handleBackBind (reverse backs)
  case mbsink of
    Just i -> get >>= lift . setIdentifier i . VArray
    Nothing -> pure ()
 where
   handleFrontBind :: MonadFail m => BindPart -> StateT (V.Vector Val) (MP m) ()
   handleFrontBind (Sink{}) = fail "Bind cannot contain multiple sinks"
   handleFrontBind (WithKey{}) = fail "Cannot destructure array with key"
   handleFrontBind (Simple mbi) = do
     v <- get
     case V.uncons v of
       Nothing -> fail "Array does not contain enough elements to destructure"
       Just (x, v') -> do
         put v'
         case mbi of
           Nothing -> pure ()
           Just i -> lift $ setIdentifier i x

   handleBackBind :: MonadFail m => BindPart -> StateT (V.Vector Val) (MP m) ()
   handleBackBind (Sink{}) = fail "Bind cannot contain multiple sinks"
   handleBackBind (WithKey{}) = fail "Cannot destructure array with key"
   handleBackBind (Simple mbi) = do
     v <- get
     case V.unsnoc v of
       Nothing -> fail "Array does not contain enough elements to destructure"
       Just (v', x) -> do
         put v'
         case mbi of
           Nothing -> pure ()
           Just i -> lift $ setIdentifier i x
