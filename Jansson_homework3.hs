{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

-- import Test.Hspec
import RPNAST
import Data.List

-- Function prob1
-- @type         String -> Exp
-- @param        String
-- @output       PExp
-- @description: Parses a String and returns a PExp
-- Used example code found in Learn You a Haskell (http://learnyouahaskell.com/functionally-solving-problems#reverse-polish-notation-calculator)
prob1 :: String -> PExp
prob1 expression = map mapFxn (words expression)
  where
    mapFxn "*" = Mul
    mapFxn "+" = Plus
    mapFxn "-" = Minus
    mapFxn "/" = IntDiv
    mapFxn x   = Val (read x::Int)

prob2 :: a
prob2 = undefined

prob3 :: a
prob3 = undefined

prob4 :: a
prob4 = undefined

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction xs numberString = read numberString:xs

-- Write your Hspec Tests below
