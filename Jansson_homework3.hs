{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

-- import Test.Hspec
import RPNAST
import Data.List

-- Function prob1
-- @type
-- @param  String
-- @output PExp
-- @description:
prob1 :: String -> PExp
prob1 expression = foldl foldingFunction [] (words expression)
  where
    foldingFunction (x:xs) "*" = Mul:xs
    foldingFunction (x:xs) "+" = Plus:xs
    foldingFunction (x:xs) "-" = Minus:xs
    foldingFunction (x:xs) "/" = IntDiv:xs
    foldingFunction xs numberString = Val num:xs
      where
        num = read numberString::Int

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
