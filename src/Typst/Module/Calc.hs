{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typst.Module.Calc
  ( calcModule,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Typst.Types
import Typst.Util

calcModule :: M.Map Identifier Val
calcModule =
  M.fromList
    [ ( "abs",
        makeFunction $ do
          (v :: Val) <- nthArg 1
          (n :: Double) <- fromVal v
          pure $
            if n < 0
              then fromMaybe v $ maybeNegate v
              else v
      ),
      ( "binom",
        makeFunction $ do
          (n :: Integer) <- nthArg 1
          (k :: Integer) <- nthArg 2
          pure $ VInteger $ product [(1 + n - k) .. n] `div` product [1 .. k]
      ),
      ( "ceil",
        makeFunction $ do
          (x :: Double) <- nthArg 1
          pure $ VInteger (ceiling x)
      ),
      ( "clamp",
        makeFunction $ do
          value <- nthArg 1
          minval <- nthArg 2
          maxval <- nthArg 3
          pure $
            if value < minval
              then minval
              else
                if value > maxval
                  then maxval
                  else value
      ),
      ( "even",
        makeFunction $ do
          v <- nthArg 1
          case v of
            VInteger i -> pure $ VBoolean $ even i
            _ -> fail "even requires an integer argument"
      ),
      ( "fact",
        makeFunction $ do
          v <- nthArg 1
          case v of
            VInteger i
              | i == 0 -> pure $ VInteger 1
              | i > 0 -> pure $ VInteger $ product [1 .. i]
            _ -> fail "odd requires a non-negative integer argument"
      ),
      ( "floor",
        makeFunction $ do
          (x :: Double) <- nthArg 1
          pure $ VInteger (floor x)
      ),
      ( "fract",
        makeFunction $ do
          (v :: Val) <- nthArg 1
          case v of
            VInteger _ -> pure $ VInteger 0
            VFloat x -> pure $ VFloat (x - fromIntegral (truncate x :: Integer))
            _ -> fail "fract requires integer or float argument"
      ),
      ( "gcd",
        makeFunction $ do
          x <- nthArg 1
          y <- nthArg 2
          pure $ VInteger $ gcd x y
      ),
      ( "lcm",
        makeFunction $ do
          x <- nthArg 1
          y <- nthArg 2
          pure $ VInteger $ lcm x y
      ),
      ( "log",
        makeFunction $ do
          b <- namedArg "base" <|> pure 10
          n <- nthArg 1
          if n <= 0
            then fail "value must be strictly positive"
            else
              if b == 0
                then fail "base may not be 0"
                else pure $ VFloat $ logBase b n
      ),
      ( "max",
        makeFunction $ do
          vs <- allArgs
          case vs of
            [] -> fail "max requires one or more argument"
            _ : _ -> pure $ maximum vs
      ),
      ( "min",
        makeFunction $ do
          vs <- allArgs
          case vs of
            [] -> fail "min requires one or more argument"
            _ : _ -> pure $ minimum vs
      ),
      ( "odd",
        makeFunction $ do
          v <- nthArg 1
          case v of
            VInteger i -> pure $ VBoolean $ odd i
            _ -> fail "odd requires an integer argument"
      ),
      ( "perm",
        makeFunction $ do
          b <- nthArg 1
          n <- nthArg 2
          pure $
            if n > b
              then VInteger 0
              else VInteger $ div (product [1 .. b]) (product [1 .. (b - n)])
      ),
      ( "pow",
        makeFunction $ do
          base <- nthArg 1
          ex <- nthArg 2
          case (base, ex) of
            (VInteger x, VInteger y) -> pure $ VInteger $ x ^ y
            _ -> do
              (base' :: Double) <- fromVal base
              (ex' :: Integer) <- fromVal ex
              pure $ VFloat $ (base') ^ (ex')
      ),
      ( "quo",
        makeFunction $ do
          (a :: Integer) <- nthArg 1
          (b :: Integer) <- nthArg 2
          pure $ VInteger $ a `quot` b
      ),
      ( "rem",
        makeFunction $ do
          (a :: Integer, f :: Double) <- properFraction <$> nthArg 1
          (b :: Integer) <- nthArg 2
          pure $
            if f == 0
              then VInteger $ rem a b
              else VFloat $ fromIntegral (rem a b) + f
      ),
      ( "round",
        makeFunction $ do
          (x :: Double) <- nthArg 1
          (digits :: Integer) <- namedArg "digits" <|> pure 0
          pure $
            if digits > 0
              then
                VFloat $
                  fromIntegral (round (x * 10 ^ digits) :: Integer)
                    / 10 ^ digits
              else VInteger (round x)
      ),
      ( "trunc",
        makeFunction $ do
          (x :: Double) <- nthArg 1
          pure $ VInteger $ truncate x
      ),
      ( "sqrt",
        makeFunction $ do
          n <- nthArg 1
          if n < 0
            then fail "can't take square root of negative number"
            else pure $ VFloat $ sqrt n
      ),
      ("cos", makeFunction $ VFloat . cos <$> nthArg 1),
      ("cosh", makeFunction $ VFloat . cosh <$> nthArg 1),
      ("sin", makeFunction $ VFloat . sin <$> nthArg 1),
      ("sinh", makeFunction $ VFloat . sinh <$> nthArg 1),
      ("tan", makeFunction $ VFloat . tan <$> nthArg 1),
      ("tanh", makeFunction $ VFloat . tanh <$> nthArg 1),
      ("acos", makeFunction $ VAngle . acos <$> nthArg 1),
      ("asin", makeFunction $ VAngle . asin <$> nthArg 1),
      ("atan", makeFunction $ VAngle . atan <$> nthArg 1),
      ("atan2", makeFunction $ VAngle <$> (atan2 <$> nthArg 1 <*> nthArg 2)),
      ("e", VFloat (exp 1)),
      ("pi", VFloat pi)
    ]
