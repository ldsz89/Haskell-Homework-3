{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST
import Data.List
import Control.Exception (evaluate)

-- Function prob1
-- @type         String -> Exp
-- @param        String
-- @output       PExp
-- @description: Parses a String and returns a PExp
-- Used example code found in Learn You a Haskell (http://learnyouahaskell.com/functionally-solving-problems#reverse-polish-notation-calculator)
prob1 :: String -> PExp
-- prob1 expression = map mapFxn (words expression)
prob1 = map mapFxn . words
  where
    mapFxn "*" = Mul
    mapFxn "+" = Plus
    mapFxn "-" = Minus
    mapFxn "/" = IntDiv
    mapFxn x   = Val (read x::Int)

-- Function prob2
-- @type        PExp -> Int
-- @param       PExp
-- @output      Int
-- @description Evaluates an RPN expression
prob2 :: PExp -> Int
prob2 expression = prob2' expression []
  where
    prob2'(Plus:xs) (x:y:ys) = prob2' xs ((y+x):ys)
    prob2'(Minus:xs) (x:y:ys) = prob2' xs ((y-x):ys)
    prob2'(Mul:xs) (x:y:ys) = undefined
    prob2'(IntDiv:xs) (0:y:ys) = error "Div by zero"
    prob2'(IntDiv:xs) (x:y:ys) = undefined
    prob2'(Val i:xs) ans = prob2' xs (i:ans)
    prob2' [] [x] = x
    prob2' _ _ = error "Bad Syntax"


prob3 :: PExp -> RPNResult
prob3 = undefined

prob4 :: a
prob4 = undefined

-- Write your Hspec Tests below
test_prob2::IO ()
test_prob2 = hspec $ do
  describe "" $ do
    context "" $ do
      it "" $ do
        prob2 [Val 1, Val 5, Plus] `shouldBe` 6
    context "" $ do
      it "" $ do
        prob2 [Val 1, Val 5, Minus] `shouldBe` -4
    context "" $ do
      it "" $ do
        evaluate (prob2 [Val 0, Val 5, IntDiv]) `shouldThrow` anyException
