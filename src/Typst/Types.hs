{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typst.Types
  ( RE,
    Val (..),
    ValType (..),
    valType,
    hasType,
    FromVal (..),
    Negatable (..),
    Summable (..),
    Multipliable (..),
    Selector (..),
    Symbol (..),
    Content (..),
    Function (..),
    Arguments (..),
    getPositionalArg,
    getNamedArg,
    Compare (..),
    MP,
    Scope (..),
    FlowDirective (..),
    Operations (..),
    XdgDirectory (..),
    EvalState (..),
    emptyEvalState,
    ShowRule (..),
    Counter (..),
    LUnit (..),
    Length (..),
    renderLength,
    Horiz (..),
    Vert (..),
    Color (..),
    Direction (..),
    Identifier (..), -- reexported
    lookupIdentifier,
    joinVals,
    prettyVal,
    valToContent,
    repr,
    Attempt (..),
  )
where

import Control.Monad (MonadPlus (..))
import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Data (Typeable)
import qualified Data.Foldable as F
import Data.Functor.Classes (Ord1 (liftCompare))
import qualified Data.Map as M
import qualified Data.Map.Ordered as OM
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Scientific (floatingOrInteger)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec
import qualified Text.PrettyPrint as P
import Text.Read (readMaybe)
import Typst.Regex (RE, makeLiteralRE)
import Typst.Syntax (Identifier (..), Markup)
import Data.Time (UTCTime, Day, DiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (XdgDirectory(..))

data Val
  = VNone
  | VAuto
  | VBoolean !Bool
  | VInteger !Integer
  | VFloat !Double
  | VRatio !Rational
  | VLength !Length
  | VAlignment (Maybe Horiz) (Maybe Vert)
  | VAngle !Double -- degrees
  | VFraction !Double
  | VColor !Color
  | VSymbol !Symbol
  | VString !Text
  | VRegex !RE
  | VDateTime (Maybe Day) (Maybe DiffTime)
  | VContent (Seq Content)
  | VArray (Vector Val)
  | VDict (OM.OMap Identifier Val)
  | VTermItem (Seq Content) (Seq Content)
  | VDirection Direction
  | VFunction (Maybe Identifier) (M.Map Identifier Val) Function
  | -- first param is Just ident if element function
    -- second param is a map of subfunctions in this function's scope
    VArguments Arguments
  | VLabel !Text
  | VCounter !Counter
  | VSelector !Selector
  | VModule Identifier (M.Map Identifier Val)
  | VStyles -- just a placeholder for now
  deriving (Show, Eq, Typeable)

instance FromJSON Val where
  parseJSON v@(Aeson.Object {}) =
    VDict . OM.fromList . M.toList . M.mapKeys Identifier <$> parseJSON v
  parseJSON v@(Aeson.Array {}) = VArray <$> parseJSON v
  parseJSON (Aeson.String t) = pure $ VString t
  parseJSON (Aeson.Number n) =
    pure $ either VFloat VInteger (floatingOrInteger n)
  parseJSON (Aeson.Bool b) = pure $ VBoolean b
  parseJSON Aeson.Null = pure VNone

data ValType
  = TNone
  | TAuto
  | TBoolean
  | TInteger
  | TFloat
  | TRatio
  | TLength
  | TAlignment
  | TAngle
  | TFraction
  | TColor
  | TSymbol
  | TString
  | TRegex
  | TDateTime
  | TContent
  | TArray
  | TDict
  | TTermItem
  | TDirection
  | TFunction
  | TArguments
  | TModule
  | TSelector
  | TStyles
  | TLabel
  | TCounter
  | TLocation
  | TAny
  | ValType :|: ValType
  deriving (Show, Eq, Typeable)

valType :: Val -> ValType
valType v =
  case v of
    VNone {} -> TNone
    VAuto {} -> TAuto
    VBoolean {} -> TBoolean
    VInteger {} -> TInteger
    VFloat {} -> TFloat
    VRatio {} -> TRatio
    VLength {} -> TLength
    VAlignment {} -> TAlignment
    VAngle {} -> TAngle
    VFraction {} -> TFraction
    VColor {} -> TColor
    VSymbol {} -> TSymbol
    VString {} -> TString
    VRegex {} -> TRegex
    VDateTime {} -> TDateTime
    VContent {} -> TContent
    VArray {} -> TArray
    VDict {} -> TDict
    VTermItem {} -> TTermItem
    VDirection {} -> TDirection
    VLabel {} -> TLabel
    VCounter {} -> TCounter
    VFunction {} -> TFunction
    VArguments {} -> TArguments
    VModule {} -> TModule
    VSelector {} -> TSelector
    VStyles {} -> TStyles

hasType :: ValType -> Val -> Bool
hasType TAny _ = True
hasType TLocation (VDict m) =
  isJust (OM.lookup "page" m >> OM.lookup "x" m >> OM.lookup "y" m)
hasType (t1 :|: t2) v = hasType t1 v || hasType t2 v
hasType t v = t == valType v

class FromVal a where
  fromVal :: (MonadPlus m, MonadFail m) => Val -> m a

instance FromVal Val where
  fromVal = pure

instance FromVal (Seq Content) where
  fromVal = pure . valToContent

instance FromVal Text where
  fromVal (VContent cs) = mconcat <$> mapM go (F.toList cs)
    where
      go (Txt t) = pure t
      go (Elt "text" _ fs) =
        maybe
          (fail "text element has no body")
          fromVal
          (M.lookup "body" fs)
      go _ = fail "not a text element"
  fromVal (VString t) = pure t
  fromVal _ = fail "not a string or content value"

instance FromVal String where
  fromVal = fmap T.unpack . fromVal

instance FromVal RE where
  fromVal (VString t) = makeLiteralRE t
  fromVal (VRegex re) = pure re
  fromVal _ = fail "not a string or regex"

instance FromVal Integer where
  fromVal val =
    case val of
      VInteger x -> pure $ fromIntegral x
      VFloat x -> pure $ floor x
      VRatio x -> pure $ floor x
      VBoolean x -> pure $ if x then 1 else 0
      VString x | Just (xint :: Integer) <- readMaybe (T.unpack x) -> pure xint
      _ -> fail $ "Cannot convert " <> show val <> " to integer"

instance FromVal Int where
  fromVal val = (fromIntegral :: Integer -> Int) <$> fromVal val

instance FromVal Rational where
  fromVal val =
    case val of
      VRatio x -> pure x
      VInteger x -> pure $ fromIntegral x
      VString x | Just (xrat :: Rational) <- readMaybe (T.unpack x) -> pure xrat
      _ -> fail $ "Cannot convert " <> show val <> " to rational"

instance FromVal Double where
  fromVal val =
    case val of
      VInteger x -> pure $ fromIntegral x
      VFloat x -> pure x
      VRatio x -> pure $ fromRational x
      VString x | Just (xdb :: Double) <- readMaybe (T.unpack x) -> pure xdb
      _ -> fail $ "Cannot convert " <> show val <> " to double"

instance FromVal Bool where
  fromVal (VBoolean b) = pure b
  fromVal val = fail $ "Cannot convert " <> show val <> " to boolean"

instance FromVal Length where
  fromVal (VLength x) = pure x
  fromVal (VRatio x) = pure $ LRatio x
  fromVal val = fail $ "Cannot convert " <> show val <> " to length"

instance FromVal Function where
  fromVal (VFunction _ _ f) = pure f
  fromVal val = fail $ show val <> " is not a function"

instance FromVal Direction where
  fromVal (VDirection d) = pure d
  fromVal val = fail $ show val <> " is not a direction"

instance FromVal Counter where
  fromVal (VString t) = pure $ CounterCustom t
  fromVal (VLabel t) = pure $ CounterLabel t
  fromVal (VFunction (Just "page") _ _) = pure $ CounterPage
  fromVal (VFunction (Just name) _ _) = pure $ CounterSelector $ SelectElement name []
  fromVal (VSelector s) = pure $ CounterSelector s
  fromVal val = fail $ show val <> " is not a counter"

instance FromVal Selector where
  fromVal (VSelector s) = pure s
  fromVal val = fail $ show val <> " is not a selector"

instance FromVal a => FromVal (Maybe a) where
  fromVal VNone = pure Nothing
  fromVal x = (Just <$> fromVal x) `mplus` pure Nothing

instance FromVal a => FromVal (Vector a) where
  fromVal (VArray v) = V.mapM fromVal v
  fromVal val = fail $ "Could not convert " <> show val <> " to array"

data Selector
  = SelectElement Identifier [(Identifier, Val)]
  | SelectString !Text
  | SelectRegex !RE
  | SelectLabel !Text
  | SelectOr Selector Selector
  | SelectAnd Selector Selector
  | SelectBefore Selector Selector
  | SelectAfter Selector Selector
  deriving (Show, Eq, Ord, Typeable)

data Symbol = Symbol
  { symDefault :: !Text,
    symAccent :: !Bool,
    symVariants :: [(Set.Set Text, Text)]
  }
  deriving (Show, Eq, Typeable)

joinVals :: MonadFail m => Val -> Val -> m Val
joinVals = go
  where
    go VNone v = pure v
    go v VNone = pure v
    go v (VSymbol (Symbol s _ _)) = go v (VString s)
    go (VString t) (VString t') = pure $ VString (t <> t')
    go (VString t) (VContent cs) = pure $ VContent (Txt t Seq.<| cs)
    go (VContent cs) (VString t) = pure $ VContent (cs Seq.|> Txt t)
    go (VContent cs) (VContent cs') = pure $ VContent (cs <> cs')
    go (VArray vec) (VArray vec') = pure $ VArray (vec <> vec')
    go accum v = fail $ "Can't combine " <> show accum <> " and " <> show v

class Compare a where
  comp :: a -> a -> Maybe Ordering

instance Compare Val where
  comp VNone VNone = Just EQ
  comp VAuto VAuto = Just EQ
  comp (VBoolean b1) (VBoolean b2) = Just $ compare b1 b2
  comp (VInteger i1) (VInteger i2) = Just $ compare i1 i2
  comp (VFloat f1) (VFloat f2) = Just $ compare f1 f2
  comp (VInteger i1) (VFloat f2) = Just $ compare (fromIntegral i1) f2
  comp (VFloat f1) (VInteger i2) = Just $ compare f1 (fromIntegral i2)
  comp (VRatio r1) (VRatio r2) = Just $ compare r1 r2
  comp (VRatio r1) (VLength (LRatio r2)) = Just $ compare r1 r2
  comp (VLength (LRatio r1)) (VRatio r2) = Just $ compare r1 r2
  comp (VRatio r1) x = comp (VFloat (fromRational r1)) x
  comp x (VRatio r1) = comp x (VFloat (fromRational r1))
  comp (VLength x1) (VLength x2) = compareLength x1 x2
  comp (VAlignment {}) (VAlignment {}) = Nothing
  comp (VAngle x1) (VAngle x2) = Just $ compare x1 x2
  comp (VFraction x1) (VFraction x2) = Just $ compare x1 x2
  comp (VColor c1) (VColor c2) = Just $ compare c1 c2
  comp (VSymbol (Symbol s1 _ _)) (VSymbol (Symbol s2 _ _)) = Just $ compare s1 s2
  comp (VString s1) (VString s2) = Just $ compare s1 s2
  comp (VContent c1) (VContent c2) = Just $ compare c1 c2
  comp (VArray v1) (VArray v2) =
    Just $ liftCompare (\x y -> fromMaybe LT (comp x y)) v1 v2
  comp (VDict m1) (VDict m2) =
    Just $ liftCompare (\x y -> fromMaybe LT (comp x y)) (OM.toMap m1) (OM.toMap m2)
  comp (VFunction (Just i1) _ _) (VFunction (Just i2) _ _) = Just $ compare i1 i2
  comp _ _ = Nothing

instance Ord Val where
  compare v1 v2 = fromMaybe EQ $ comp v1 v2

class Negatable a where
  maybeNegate :: a -> Maybe a

instance Negatable Val where
  maybeNegate (VInteger i) = pure $ VInteger (-i)
  maybeNegate (VFloat f) = pure $ VFloat (-f)
  maybeNegate (VLength x) = pure $ VLength $ negateLength x
  maybeNegate (VAngle x) = pure $ VAngle (-x)
  maybeNegate (VFraction x) = pure $ VFraction (-x)
  maybeNegate (VRatio x) = pure $ VRatio (-x)
  maybeNegate v = fail $ "could not negate " <> show v

class Negatable a => Summable a where
  maybePlus :: a -> a -> Maybe a
  maybeMinus :: a -> a -> Maybe a
  maybeMinus x y = maybeNegate y >>= maybePlus x

instance Summable Val where
  maybePlus VNone x = pure x
  maybePlus x VNone = pure x
  maybePlus (VInteger i1) (VInteger i2) = pure $ VInteger (i1 + i2)
  maybePlus (VRatio r1) (VRatio r2) = pure $ VRatio (r1 + r2)
  maybePlus (VFloat f1) (VFloat f2) = pure $ VFloat (f1 + f2)
  maybePlus (VInteger i1) (VFloat f2) = pure $ VFloat (fromIntegral i1 + f2)
  maybePlus (VFloat f1) (VInteger i2) = pure $ VFloat (f1 + fromIntegral i2)
  maybePlus (VInteger i1) (VRatio r2) = pure $ VRatio (fromIntegral i1 + r2)
  maybePlus (VRatio r1) (VInteger i2) = pure $ VRatio (r1 + fromIntegral i2)
  maybePlus (VFloat f1) (VRatio r2) = pure $ VFloat (f1 + fromRational r2)
  maybePlus (VRatio r1) (VFloat f2) = pure $ VFloat (fromRational r1 + f2)
  maybePlus (VString s1) (VString s2) = pure $ VString (s1 <> s2)
  maybePlus (VContent c1) (VContent c2) = pure $ VContent (c1 <> c2)
  maybePlus (VString s1) (VContent c2) = pure $ VContent (Txt s1 Seq.<| c2)
  maybePlus (VContent c1) (VString s2) = pure $ VContent (c1 Seq.|> Txt s2)
  maybePlus (VLength l1) (VLength l2) = pure $ VLength (l1 <> l2)
  maybePlus (VLength l1) (VRatio r1) = pure $ VLength (l1 <> LRatio r1)
  maybePlus (VRatio r1) (VLength l1) = pure $ VLength (l1 <> LRatio r1)
  maybePlus (VAngle a1) (VAngle a2) = pure $ VAngle (a1 + a2)
  maybePlus (VFraction f1) (VFraction f2) = pure $ VFraction (f1 + f2)
  maybePlus (VArray v1) (VArray v2) = pure $ VArray (v1 <> v2)
  maybePlus (VDict m1) (VDict m2) = pure $ VDict (m1 OM.<>| m2)
  maybePlus (VColor c) (VLength l) =
    -- Stroke '1pt + red'
    pure $ VDict $ OM.fromList [("thickness", VLength l), ("color", VColor c)]
  maybePlus (VLength l) (VColor c) = maybePlus (VColor c) (VLength l)
  maybePlus v1 v2 = fail $ "could not add " <> show v1 <> " and " <> show v2

class Multipliable a where
  maybeTimes :: a -> a -> Maybe a
  maybeDividedBy :: a -> a -> Maybe a

instance Multipliable Val where
  maybeTimes (VInteger i1) (VInteger i2) = pure $ VInteger (i1 * i2)
  maybeTimes (VFloat x1) (VFloat x2) = pure $ VFloat (x1 * x2)
  maybeTimes (VInteger i1) (VFloat f2) = pure $ VFloat (fromIntegral i1 * f2)
  maybeTimes (VFloat f1) (VInteger i2) = pure $ VFloat (f1 * fromIntegral i2)
  maybeTimes (VInteger i) (VArray v) =
    pure $ VArray (mconcat $ replicate (fromIntegral i) v)
  maybeTimes (VArray v) (VInteger i) =
    pure $ VArray (mconcat $ replicate (fromIntegral i) v)
  maybeTimes (VInteger i) (VString s)
    | i >= 0 = pure $ VString (T.replicate (fromIntegral i) s)
  maybeTimes (VString s) (VInteger i)
    | i >= 0 = pure $ VString (T.replicate (fromIntegral i) s)
  maybeTimes (VInteger i) (VContent c)
    | i >= 0 = pure $ VContent (mconcat $ replicate (fromIntegral i) c)
  maybeTimes (VContent c) (VInteger i)
    | i >= 0 = pure $ VContent (mconcat $ replicate (fromIntegral i) c)
  maybeTimes (VInteger i) (VLength l) = pure $ VLength $ timesLength (fromIntegral i) l
  maybeTimes (VLength l) (VInteger i) = pure $ VLength $ timesLength (fromIntegral i) l
  maybeTimes (VFloat f) (VLength l) = pure $ VLength $ timesLength f l
  maybeTimes (VLength l) (VFloat f) = pure $ VLength $ timesLength f l
  maybeTimes (VInteger i) (VAngle a) = pure $ VAngle (fromIntegral i * a)
  maybeTimes (VAngle a) (VInteger i) = pure $ VAngle (fromIntegral i * a)
  maybeTimes (VFloat f) (VAngle a) = pure $ VAngle (f * a)
  maybeTimes (VAngle a) (VFloat f) = pure $ VAngle (f * a)
  maybeTimes (VInteger i) (VFraction f) = pure $ VFraction (fromIntegral i * f)
  maybeTimes (VFraction f) (VInteger i) = pure $ VFraction (fromIntegral i * f)
  maybeTimes (VFloat x) (VFraction f) = pure $ VFraction (x * f)
  maybeTimes (VFraction f) (VFloat x) = pure $ VFraction (x * f)
  maybeTimes (VFraction f1) (VFraction f2) = pure $ VFraction (f1 * f2)
  maybeTimes (VRatio r1) (VRatio r2) = pure $ VRatio (r1 * r2)
  maybeTimes (VInteger i) (VRatio r) = pure $ VRatio (fromIntegral i * r)
  maybeTimes (VRatio r) (VInteger i) = pure $ VRatio (fromIntegral i * r)
  maybeTimes (VFloat x) (VRatio r) = pure $ VRatio (realToFrac x * r)
  maybeTimes (VRatio r) (VFloat x) = pure $ VRatio (realToFrac x * r)
  maybeTimes v1 v2 = fail $ "could not multiply " <> show v1 <> " and " <> show v2

  maybeDividedBy (VInteger i1) (VInteger i2) =
    if i1 `mod` i2 == 0
      then pure $ VInteger (i1 `div` i2)
      else pure $ VFloat (fromIntegral i1 / fromIntegral i2)
  maybeDividedBy (VFloat x1) (VFloat x2) = maybeTimes (VFloat x1) (VFloat (1 / x2))
  maybeDividedBy (VInteger i1) (VFloat f2) = pure $ VFloat (fromIntegral i1 / f2)
  maybeDividedBy (VFloat f1) (VInteger i2) = pure $ VFloat (f1 / fromIntegral i2)
  maybeDividedBy (VLength l) (VInteger i)
    | i >= 0 = pure $ VLength (mconcat $ replicate (fromIntegral i) l)
  maybeDividedBy (VLength l) (VFloat f) = pure $ VLength $ timesLength (1 / f) l
  maybeDividedBy (VAngle a) (VInteger i) = pure $ VAngle (fromIntegral i / a)
  maybeDividedBy (VInteger i) (VFraction f) = pure $ VFraction (fromIntegral i / f)
  maybeDividedBy (VFraction f) (VInteger i) = pure $ VFraction (fromIntegral i / f)
  maybeDividedBy (VFraction f1) (VFraction f2) = pure $ VFraction (f1 / f2)
  maybeDividedBy (VLength l1) (VLength l2)
    | l1 == l2 = pure $ VInteger 1
  maybeDividedBy (VLength (LExact l1 u1)) (VLength (LExact l2 u2))
    | u1 == u2 = pure $ VFloat (l1 / l2)
    | Just pts1 <- toPts u1 l1,
      Just pts2 <- toPts u2 l2 =
        pure $ VFloat (pts1 / pts2)
  maybeDividedBy (VLength (LRatio r)) x
    | Just (VRatio r') <- maybeDividedBy (VRatio r) x =
        pure $ VLength (LRatio r')
  maybeDividedBy (VRatio r1) (VLength (LRatio r2)) = pure $ VRatio (r1 / r2)
  maybeDividedBy (VAngle a1) (VAngle a2) = pure $ VFloat (a1 / a2)
  maybeDividedBy (VRatio a1) (VRatio a2) = pure $ VRatio (a1 / a2)
  maybeDividedBy (VRatio r) (VInteger i) = pure $ VRatio (r / fromIntegral i)
  maybeDividedBy (VRatio r) (VFloat x) =
    pure $ VRatio (r / realToFrac x)
  maybeDividedBy v1 v2 = fail $ "could not divide " <> show v1 <> " by " <> show v2

data Content
  = Txt !Text
  | Lab !Text
  | Elt
      { eltName :: Identifier,
        eltPos :: Maybe SourcePos,
        eltFields :: M.Map Identifier Val
      }
  deriving (Show, Typeable)

instance Eq Content where
  Txt t1 == Txt t2 = t1 == t2
  Lab t1 == Lab t2 = t1 == t2
  Elt n1 _ f1 == Elt n2 _ f2 = n1 == n2 && f1 == f2
  _ == _ = False

instance Ord Content where
  compare Txt {} Lab {} = LT
  compare Lab {} Elt {} = LT
  compare Txt {} Elt {} = LT
  compare Lab {} Txt {} = GT
  compare Elt {} Lab {} = GT
  compare Elt {} Txt {} = GT
  compare (Txt t1) (Txt t2) = compare t1 t2
  compare (Lab t1) (Lab t2) = compare t1 t2
  compare (Elt n1 _ f1) (Elt n2 _ f2) = compare (n1, f1) (n2, f2)

instance IsString Content where
  fromString x = Txt (T.pack x)

newtype Function = Function (forall m. Monad m => Arguments -> MP m Val)
  deriving (Typeable)

instance Show Function where
  show _ = "<function>"

instance Eq Function where
  _ == _ = False

data Scope
  = FunctionScope
  | BlockScope
  deriving (Show, Ord, Eq)

data FlowDirective
  = FlowNormal
  | FlowBreak
  | FlowContinue
  | FlowReturn Bool
  deriving (Show, Ord, Eq)

data Operations m =
  Operations
  { loadBytes :: FilePath -> m BS.ByteString
  , currentUTCTime :: m UTCTime
  , getXdgDir :: XdgDirectory -> FilePath -> m FilePath
  , checkExistence :: FilePath -> m Bool
  }

data EvalState m = EvalState
  { evalIdentifiers :: [(Scope, M.Map Identifier Val)],
    -- first item is current block, then superordinate block, etc.
    evalCounters :: M.Map Counter Integer,
    evalMath :: Bool,
    evalShowRules :: [ShowRule],
    evalStyles :: M.Map Identifier Arguments,
    evalFlowDirective :: FlowDirective,
    evalOperations :: Operations m
  }

emptyEvalState :: EvalState m
emptyEvalState = EvalState
    { evalIdentifiers = [],
      evalCounters = mempty,
      evalMath = False,
      evalShowRules = [],
      evalStyles = mempty,
      evalFlowDirective = FlowNormal,
      evalOperations = undefined
    }

data Attempt a
  = Success a
  | Failure String
  deriving (Show, Eq, Ord, Typeable)

instance Functor Attempt where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure s) = Failure s

instance Applicative Attempt where
  pure = Success
  (Success f) <*> (Success a) = Success (f a)
  Failure s <*> _ = Failure s
  _ <*> Failure s = Failure s

instance Monad Attempt where
  return = pure
  Failure s >>= _ = Failure s
  Success x >>= f = f x

instance MonadFail Attempt where
  fail = Failure

data ShowRule
  = ShowRule Selector (forall m. Monad m => Content -> MP m (Seq Content))

instance Show ShowRule where
  show (ShowRule sel _) = "ShowRule " <> show sel <> " <function>"

type MP m = ParsecT [Markup] (EvalState m) m

data Arguments = Arguments
  { positional :: [Val],
    named :: OM.OMap Identifier Val
  }
  deriving (Show, Eq, Typeable)

instance Semigroup Arguments where
  Arguments ps1 ns1 <> Arguments ps2 ns2 =
    Arguments (combinePositional ps1 ps2) (OM.unionWithR (\_ _ v -> v) ns1 ns2)

-- we want to let a later alignment, color, or length supersede rather than
-- adding to an earlier one. For #set.
combinePositional :: [Val] -> [Val] -> [Val]
combinePositional [] ys = ys
combinePositional xs (y : ys) =
  case (valType y, valType (last xs)) of
    (TAlignment, TAlignment) -> init xs ++ y : ys
    (TLength, TLength) -> init xs ++ y : ys
    (TAngle, TAngle) -> init xs ++ y : ys
    (TColor, TColor) -> init xs ++ y : ys
    _ -> xs ++ y : ys
combinePositional xs ys = xs ++ ys

instance Monoid Arguments where
  mappend = (<>)
  mempty :: Arguments
  mempty = Arguments mempty OM.empty

getPositionalArg :: (MonadFail m, MonadPlus m, FromVal a) => Int -> Arguments -> m a
getPositionalArg idx args =
  if length (positional args) < idx
    then fail "Not enough arguments"
    else fromVal (positional args !! (idx - 1))

getNamedArg :: (MonadFail m, MonadPlus m, FromVal a) => Identifier -> Arguments -> m a
getNamedArg ident@(Identifier name) args =
  case OM.lookup ident (named args) of
    Nothing -> fail $ "No argument named " <> T.unpack name
    Just v -> fromVal v

data Counter
  = CounterCustom !Text
  | CounterLabel !Text
  | CounterSelector !Selector
  | CounterPage
  deriving (Eq, Ord, Show, Typeable)

data LUnit = LEm | LPt | LIn | LCm | LMm
  deriving (Show, Eq, Typeable)

data Length
  = LExact Double LUnit
  | LRatio !Rational
  | LSum Length Length
  deriving (Show, Eq, Typeable)

instance Semigroup Length where
  (LExact x xu) <> (LExact y yu)
    | Just (z, zu) <- addLengths (x, xu) (y, yu) =
        LExact z zu
  LRatio x <> LRatio y = LRatio (x + y)
  LRatio x <> LExact 0 _ = LRatio x
  LExact 0 _ <> LRatio x = LRatio x
  LRatio 0 <> LExact x u = LExact x u
  LExact x u <> LRatio 0 = LExact x u
  x <> y = LSum x y

instance Monoid Length where
  mappend = (<>)
  mempty = LExact 0.0 LPt

addLengths :: (Double, LUnit) -> (Double, LUnit) -> Maybe (Double, LUnit)
addLengths (0, _xu) (y, yu) = pure (y, yu)
addLengths (x, xu) (0, _yu) = pure (x, xu)
addLengths (x, xu) (y, yu) =
  if xu == yu
    then pure (x + y, xu)
    else do
      x' <- toPts xu x
      y' <- toPts yu y
      pure (x' + y', LPt)

timesLength :: Double -> Length -> Length
timesLength f (LExact l u) = LExact (f * l) u
timesLength f (LRatio r) = LRatio (toRational f * r)
timesLength f (LSum l1 l2) = LSum (timesLength f l1) (timesLength f l2)

toPts :: LUnit -> Double -> Maybe Double
toPts LPt x = Just x
toPts LEm _ = Nothing
toPts LIn x = Just $ x * 72.0
toPts LCm x = Just $ x * 28.35
toPts LMm x = Just $ x * 283.5

-- boolean is true if we need to include parens for LSum
renderLength :: Bool -> Length -> Text
renderLength parens (LSum l1 l2) =
  (if parens then (\x -> "(" <> x <> ")") else id)
    (renderLength True l1 <> " + " <> renderLength True l2)
renderLength _ (LExact x u) =
  T.pack (show x) <> renderUnit u
renderLength _ (LRatio x) = toPercent x

renderUnit :: LUnit -> Text
renderUnit LEm = "em"
renderUnit LPt = "pt"
renderUnit LIn = "in"
renderUnit LCm = "cm"
renderUnit LMm = "mm"

compareLength :: Length -> Length -> Maybe Ordering
compareLength (LExact x xu) (LExact y yu)
  | xu == yu = pure $ compare x y
  | otherwise = do
      x' <- toPts xu x
      y' <- toPts yu y
      pure $ compare x' y'
compareLength (LRatio x) (LRatio y) = pure (compare x y)
compareLength (LSum x1 y1) (LSum x2 y2) = do
  z <- compareLength x1 x2
  if z == EQ
    then compareLength y1 y2
    else mzero
compareLength _ _ = mzero

negateLength :: Length -> Length
negateLength (LExact x u) = LExact (negate x) u
negateLength (LRatio x) = LRatio (negate x)
negateLength (LSum x y) = LSum (negateLength x) (negateLength y)

data Horiz = HorizStart | HorizEnd | HorizLeft | HorizCenter | HorizRight
  deriving (Show, Eq, Ord, Typeable)

data Vert = VertTop | VertHorizon | VertBottom
  deriving (Show, Eq, Ord, Typeable)

data Color
  = RGB Rational Rational Rational Rational
  | CMYK Rational Rational Rational Rational
  | Luma Rational
  deriving (Show, Eq, Ord, Typeable)

data Direction = Ltr | Rtl | Ttb | Btt
  deriving (Show, Eq, Ord, Typeable)

prettyVal :: Val -> P.Doc
prettyVal expr =
  case expr of
    VContent cs -> prettyContent cs
    VString t -> "\"" <> escString t <> "\""
    VRegex re -> P.text (show re)
    VDateTime d t -> P.text (unwords (catMaybes
       [show <$> d, formatTime defaultTimeLocale "%0H:%0M:%0S" <$> t]))
    VAuto -> "auto"
    VNone -> "none"
    VBoolean True -> "true"
    VBoolean False -> "false"
    VFloat x -> P.text $ show x
    VRatio x -> text $ toPercent x
    VInteger x -> P.text $ show x
    VAngle x -> P.text (show x <> "deg")
    VLength len -> text $ renderLength False len
    VAlignment x y -> text $
      case (x, y) of
        (Nothing, Nothing) -> mempty
        (Just x', Nothing) -> renderHoriz x'
        (Nothing, Just y') -> renderVert y'
        (Just x', Just y') ->
          "Axes(" <> renderHoriz x' <> ", " <> renderVert y' <> ")"
      where
        renderHoriz = T.toLower . T.drop 5 . T.pack . show
        renderVert = T.toLower . T.drop 4 . T.pack . show
    VFraction x -> P.text (show x <> "fr")
    VArray xs ->
      P.parens
        ( P.cat $
            P.punctuate ", " $
              map prettyVal (V.toList xs)
        )
    VTermItem t d -> prettyVal (VArray [VContent t, VContent d])
    VDict m ->
      P.parens
        ( P.sep $
            P.punctuate "," $
              ( map
                  ( \(Identifier k, v) ->
                      text k <> ": " <> prettyVal v
                  )
                  (OM.assocs m)
              )
        )
    VDirection d -> text $ T.toLower $ T.pack $ show d
    VFunction _ _ _ -> mempty
    VLabel _ -> mempty
    VCounter _ -> mempty
    VColor (RGB r g b o) ->
      "rgb("
        <> text (toPercent r)
        <> ","
        <> text (toPercent g)
        <> ","
        <> text (toPercent b)
        <> ","
        <> text (toPercent o)
        <> ")"
    VColor (CMYK c m y k) ->
      "cmyk("
        <> text (toPercent c)
        <> ","
        <> text (toPercent m)
        <> ","
        <> text (toPercent y)
        <> ","
        <> text (toPercent k)
        <> ")"
    VColor (Luma g) -> "luma(" <> text (toPercent g) <> ")"
    VModule (Identifier modid) _ -> "<module " <> text modid <> ">"
    VArguments args ->
      P.parens
        ( P.sep
            ( P.punctuate
                ","
                ( [ P.sep
                      ( P.punctuate
                          ","
                          ( map
                              ( \(Identifier k, v) ->
                                  text k <> ": " <> prettyVal v
                              )
                              (OM.assocs (named args))
                          )
                      )
                    | not (OM.null (named args))
                  ]
                    ++ [ P.cat (P.punctuate ", " $ map prettyVal (positional args))
                         | not (null (positional args))
                       ]
                )
            )
        )
    VSymbol (Symbol t _ _) -> text t
    VSelector _ -> mempty
    VStyles -> mempty

escString :: Text -> P.Doc
escString =
  P.text . concatMap go . T.unpack
  where
    go :: Char -> String
    go '"' = "\\\""
    go '\\' = "\\\\"
    go '\n' = "\\n"
    go '\r' = "\\r"
    go '\t' = "\\t"
    go x = [x]

prettyContent :: Seq Content -> P.Doc
prettyContent cs
  | Seq.length cs == 1 = foldMap go cs
  | otherwise =
      P.braces
        ( P.space
            <> P.cat (P.punctuate ", " (map go (F.toList cs)))
            <> P.space
        )
  where
    go (Txt t) = "[" <> text t <> "]"
    go (Lab l) = "<" <> text l <> ">"
    go (Elt (Identifier name) _ fields) =
      text name
        <> P.parens
          ( P.cat $
              P.punctuate
                ", "
                ( map
                    ( \(Identifier k, v) ->
                        text k <> ": " <> prettyVal v
                    )
                    (M.toList fields)
                )
          )

valToContent :: Val -> Seq Content
valToContent (VContent x) = x
valToContent VNone = mempty
valToContent (VString t) = Seq.singleton $ Txt t
valToContent (VLabel t) = Seq.singleton $ Lab t
valToContent x = Seq.singleton $ Txt $ repr x

renderStyle :: P.Style
renderStyle = P.Style P.PageMode 60 2.0

repr :: Val -> Text
repr = T.pack . P.renderStyle renderStyle . prettyVal

toPercent :: Rational -> Text
toPercent n =
  T.pack (show (floor (100 * n) :: Integer)) <> "%"

text :: Text -> P.Doc
text t = P.text $ T.unpack t

lookupIdentifier :: Monad m => Identifier -> MP m Val
lookupIdentifier ident = do
  let go [] = fail $ show ident <> " not found"
      go ((_, i) : is) = case M.lookup ident i of
        Just v -> pure v
        Nothing -> go is
  getState >>= go . evalIdentifiers
