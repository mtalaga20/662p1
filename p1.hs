{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- Evaluation Functions

evalM :: ABE -> (Maybe ABE)
evalM (Num x) = if x < 0 then Nothing else Just (Num x)
--evalM (Plus l r) = 
--evalM (Minus l r) = 
--evalM (Mult l r) = 
--evalM (Div l r) =
--evalM (Boolean b) = 
--evalM (And l r) = 
--evalM (Leq l r) = 
--evalM (Is0 l) = 
--evalM (If a b c) = 
evalM _ = Nothing -- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeof(Num n) = if n >= 0 then return TNUM else Nothing
typeof(BOOLEAN b) = return TBOOL

typeof(Plus l r) = do { TNUM <- typeof(l);
			TNUM <- typeof(r);
			return TNUM;}
typeof(And l r) = do {TBOOL <- typeof(l);
			TBOOL <- typeof(r);
			return TBOOL}
--typeof(Minus l r) = do {
--typeof (Leq l r) = do {TNUM <- typeof(l);
			TNUM <- typeof(r);
			return TBOOL}
typeof(Is0 l) = do {TNUM <- typeof(l);
			return TBOOL}
typeof(If a b c) = do {TBOOL <- typeof(a);
			b' <- typeof(b);
			c' <- typeof(c);
			if b' == c' return b' else Nothing} --checking types of b' and c'

typeofM _ = Nothing

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM _ = Nothing

-- Optimizer

optimize :: ABE -> ABE
optimize e = e

evalOptM :: ABE -> Maybe ABE
evalOptM _ = Nothing
