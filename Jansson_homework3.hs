{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Homework3 where

import Test.Hspec
import RPNAST
import Control.Exception (evaluate)

-- Function prob1
-- @type         String -> PExp
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
    prob2'(Plus:xs)   (x:y:ys)  = prob2' xs ((y+x):ys)
    prob2'(Minus:xs)  (x:y:ys)  = prob2' xs ((y-x):ys)
    prob2'(Mul:xs)    (x:y:ys)  = prob2' xs ((y*x):ys)
    prob2'(IntDiv:xs) (0:y:ys)  = error "Div by zero"
    prob2'(IntDiv:xs) (x:y:ys)  = prob2' xs ((y `div` x):ys)
    prob2'(Val i:xs)  ans       = prob2' xs (i:ans)
    prob2'[]          [x]       = x
    prob2' _          _         = error "Bad Syntax"

-- Function prob3
-- @type        PExp -> RPNResult
-- @param       PExp
-- @output      RPNResult
-- @description Refactor the evaluator by returning an RPNResult
prob3 :: PExp -> RPNResult
prob3 expression = prob3' expression []
  where
    prob3'(Plus:xs)   (x:y:ys)  = prob3' xs ((y+x):ys)
    prob3'(Minus:xs)  (x:y:ys)  = prob3' xs ((y-x):ys)
    prob3'(Mul:xs)    (x:y:ys)  = prob3' xs ((y*x):ys)
    prob3'(IntDiv:xs) (0:y:ys)  = Failure DivByZero
    prob3'(IntDiv:xs) (x:y:ys)  = prob3' xs ((y `div` x):ys)
    prob3'(Val i:xs)  ans       = prob3' xs (i:ans)
    prob3' []         [x]       = Success x
    prob3' _          _         = Failure BadSyntax

-- Function prob4
-- @type        PExp -> Result String String
-- @param       PExp
-- @type        Result String String
-- @description Translate RPN to infix notation
prob4 :: PExp -> Result String String
prob4 expression = prob4' expression [] []
  where
    prob4'(Plus:xs)   (x:y:ys) ans = prob4' xs ys ("(" ++ ans ++ (show y) ++ " + " ++ (show x) ++ ")")
    prob4'(Plus:xs)   (n:ns)   ans = prob4' xs ns ("(" ++ ans ++ " + " ++ (show n) ++ ")" )
    prob4'(Minus:xs)  (x:y:ys) ans = prob4' xs ys ("(" ++ ans ++ (show y) ++ " - " ++ (show x) ++ ")")
    prob4'(Minus:xs)  (n:ns)   ans = prob4' xs ns ("(" ++ ans ++ " - " ++ (show n) ++ ")" )
    prob4'(Mul:xs)    (x:y:ys) ans = prob4' xs ys ("(" ++ ans ++ (show y) ++ " * " ++ (show x) ++ ")")
    prob4'(Mul:xs)    (n:ns)   ans = prob4' xs ns ("(" ++ ans ++ " * " ++ (show n) ++ ")" )
    prob4'(IntDiv:xs) (0:y:ys) ans = Failure "Div by Zero"
    prob4'(IntDiv:xs) (0:ns)   ans = Failure "Div by Zero"
    prob4'(IntDiv:xs) (x:y:ys) ans = prob4' xs ys ("(" ++ ans ++ (show y) ++ " / " ++ (show x) ++ ")")
    prob4'(IntDiv:xs) (n:ns)   ans = prob4' xs ns ("(" ++ ans ++ " / " ++ (show n) ++ ")" )
    prob4'(Val i:xs)  acc      ans = prob4' xs (i:acc) ans
    prob4' []         (x:xs)   ans = Success (show x)
    prob4' []         []       ans = Success ans
    prob4' _          _        _   = Failure "Bad Syntax"

-- Write your Hspec Tests below
test_prob1::IO ()
test_prob1 = hspec $ do
  describe "" $ do
    context "1" $ do
      it "Tests Val Int Case" $ do
        prob1 "1" `shouldBe` [Val 1]
    context "1, 5, Plus" $ do
      it "Tests Plus Case" $ do
        prob1 "1 5 +" `shouldBe` [Val 1, Val 5, Plus]
    context "1, 5, Minus" $ do
      it "Tests Minus Case" $ do
        prob1 "1 5 -" `shouldBe` [Val 1, Val 5, Minus]
    context "1, 5, Mul" $ do
      it "Tests Mul Case" $ do
        prob1 "1 5 *" `shouldBe` [Val 1, Val 5, Mul]
    context "1, 5, IntDiv" $ do
      it "Tests IntDiv Case" $ do
        prob1 "1 5 /" `shouldBe` [Val 1, Val 5, IntDiv]

test_prob2::IO ()
test_prob2 = hspec $ do
  describe "" $ do
    context "1, 5, Plus" $ do
      it "Tests Plus Case" $ do
        prob2 [Val 1, Val 5, Plus] `shouldBe` 6
    context "1, 5, Minus" $ do
      it "Tests Minus Case" $ do
        prob2 [Val 1, Val 5, Minus] `shouldBe` -4
    context "1, 5, Mul" $ do
      it "Tests Mult Case" $ do
        prob2 [Val 1, Val 5, Mul] `shouldBe` 5
    context "5, 20, IntDiv" $ do
      it "Tests Div Case" $ do
        prob2 [Val 20, Val 5, IntDiv] `shouldBe` 4
    context "5, 0, IntDiv" $ do
      it "Tests Div by Zero" $ do
        evaluate (prob2 [Val 5, Val 0, IntDiv]) `shouldThrow` anyException
    context "5, Mult, IntDiv" $ do
      it "Tests Bad Syntax" $ do
        evaluate (prob2 [Val 5, Mul, IntDiv]) `shouldThrow` anyException

test_prob3::IO ()
test_prob3 = hspec $ do
  describe "" $ do
    context "1, 5, Plus" $ do
      it "Tests Plus Case" $ do
        prob3 [Val 1, Val 5, Plus] `shouldBe` (Success 6)
    context "1, 5, Minus" $ do
      it "Tests Minus Case" $ do
        prob3 [Val 1, Val 5, Minus] `shouldBe` (Success (-4))
    context "1, 5, Mul" $ do
      it "Tests Mult Case" $ do
        prob3 [Val 1, Val 5, Mul] `shouldBe` (Success 5)
    context "5, 20, IntDiv" $ do
      it "Tests Div Case" $ do
        prob3 [Val 20, Val 5, IntDiv] `shouldBe` (Success 4)
    context "5, 0, IntDiv" $ do
      it "Tests Div by Zero" $ do
        prob3 [Val 5, Val 0, IntDiv] `shouldBe` (Failure DivByZero)
    context "5, Mult, IntDiv" $ do
      it "Tests Bad Syntax" $ do
        prob3 [Val 5, Mul, IntDiv] `shouldBe` (Failure BadSyntax)

test_prob4::IO ()
test_prob4 = hspec $ do
  describe "" $ do
    context "1, 5, Plus" $ do
      it "Tests Plus Case" $ do
        prob4 [Val 1, Val 5, Plus] `shouldBe` (Success "(1 + 5)")
    context "5, 4, Minus" $ do
      it "Tests Minus Case" $ do
        prob4 [Val 5, Val 4, Minus] `shouldBe` (Success "(5 - 4)")
    context "5, 4, Mul" $ do
      it "Tests Mul Case" $ do
        prob4 [Val 5, Val 4, Mul] `shouldBe` (Success "(5 * 4)")
    context "10, 5, IntDiv" $ do
      it "Tests IntDiv Case" $ do
        prob4 [Val 5, Val 4, IntDiv] `shouldBe` (Success "(5 / 4)")
    context "1, 0, IntDiv" $ do
      it "Tests Div By Zero Case" $ do
        prob4 [Val 1, Val 0, IntDiv] `shouldBe` (Failure "Div by Zero")
    context "Plus" $ do
      it "Tests Bad Syntax Case" $ do
        prob4 [Plus] `shouldBe` (Failure "Bad Syntax")
