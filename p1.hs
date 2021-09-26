{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Michael Talaga, EECS 662, Project 1

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
evalM (Num x) = if x < 0 then Nothing else return (Num x)
evalM (Plus l r) = do {(Num l') <- (evalM l);
                        (Num r') <- (evalM r);
                        return (Num ( l' + r'))}
evalM (Minus l r) = do {(Num l') <- (evalM l);
                        (Num r') <- (evalM r);
                        if l' - r' < 0 then Nothing else return (Num (l' - r'))}
evalM (Mult l r) = do {(Num l') <- (evalM l);
                        (Num r') <- (evalM r);
                        return (Num (l' * r'))}
evalM (Div l r) = do {(Num l') <- (evalM l);
                        (Num r') <- (evalM r);
                        if r' == 0 then Nothing else return (Num (l' `div` r'))}
evalM (Boolean b) = return (Boolean b)
evalM (And l r) = do {(Boolean l') <- (evalM l);
                      (Boolean r') <- (evalM r);
                      if l' == r' then return (Boolean True) else return (Boolean False)}
evalM (Leq l r) = do {(Num l') <- (evalM l);
                      (Num r') <- (evalM r);
                      if l' <= r' then return (Boolean True) else return (Boolean False)}
evalM (IsZero l) = do {(Num l') <- (evalM l);
                    if l' == 0 then return (Boolean True) else return ( Boolean False)}
evalM (If a b c) = do { (Boolean a') <- (evalM a);
                        if a' then (evalM b) else (evalM c)}
evalM _ = Nothing -- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num n) = if n >= 0 then return TNum else Nothing
typeofM (Boolean b) = return TBool

typeofM (Plus l r) = do { TNum <- (typeofM l);
                        TNum <- (typeofM r);
                        return TNum}
typeofM (And l r) = do {(TBool) <- (typeofM l);
		  	              (TBool) <- (typeofM r);
			                return TBool}
typeofM (Minus l r) = do { TNum <- (typeofM l);
                        TNum <- (typeofM r);
                        return TNum}
typeofM (Mult l r) = do { TNum <- (typeofM l);
                        TNum <- (typeofM r);
                        return TNum}
typeofM (Div l r) = do { TNum <- (typeofM l);
                        TNum <- (typeofM r);
                        return TNum}
typeofM (Leq l r) = do {TNum <- typeofM(l);
			                TNum <- typeofM(r);
			                return TBool}
typeofM (IsZero l) = do {TNum <- (typeofM l);
			                return TBool}
typeofM (If a b c) = do {TBool <- (typeofM a);
                      b' <- (typeofM b);
                      c' <- (typeofM c);
                      if b' == c' then return b' else Nothing} --checking types of b' and c'
typeofM _ = Nothing

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM (Num n) = if typeofM(Num n) /= (Nothing) then evalM(Num n) else Nothing
evalTypeM (Plus l r) = do {((TNum)) <- typeofM(l);
                         ((TNum)) <- typeofM(r);
                        a <- (evalM (Plus l r));
                        return a}           
evalTypeM (Minus l r) = do {((TNum)) <- typeofM(l);
                         ((TNum)) <- typeofM(r);
                        a <- (evalM (Minus l r));
                        return a}  
evalTypeM (Mult l r) = do {((TNum)) <- typeofM(l);
                         ((TNum)) <- typeofM(r);
                        a <- (evalM (Mult l r));
                        return a} 
evalTypeM (Div l r) = do {((TNum)) <- typeofM(l);
                         ((TNum)) <- typeofM(r);
                        a <- (evalM (Div l r));
                        return a}      
evalTypeM (Boolean b) = if typeofM(Boolean b) /= (Nothing) then evalM(Boolean b) else Nothing
evalTypeM (And l r) = do {((TBool)) <- typeofM(l);
                         ((TBool)) <- typeofM(r);
                        a <- (evalM (And l r));
                        return a}  
evalTypeM (Leq l r) = do {((TNum)) <- typeofM(l);
                         ((TNum)) <- typeofM(r);
                        a <- (evalM (Leq l r));
                        return a}  
evalTypeM (IsZero l) = do {((TNum)) <- typeofM(l);
                        a <- (evalM (IsZero l));
                        return a}  
evalTypeM (If a b c) = do {TBool <- (typeofM a);
                          (Boolean a') <- (evalM a);
                          if a' then (evalM b) else (evalM c)}
evalTypeM _ = Nothing

-- Optimizer

optimize :: ABE -> ABE
optimize e = e
optimize (Num n) = (Num n)
optimize (Boolean b) = (Boolean b)
optimize (Plus l (Num 0)) = (optimize l)  
optimize (Plus l r) = (Plus (optimize l)(optimize r))
optimize (Minus l r) = (Minus (optimize l)(optimize r))
optimize (Mult l r) = (Mult (optimize l)(optimize r))
optimize (Div l r) = (Div (optimize l)(optimize r))
optimize (Leq l r) = (Leq (optimize l)(optimize r))
optimize (IsZero l) = (IsZero (optimize l))
optimize (If (Boolean True) b c) = (optimize b) 
optimize (If (Boolean False) b c) = (optimize c) 
optimize (And l r) = (And (optimize l) (optimize r))



evalOptM :: ABE -> Maybe ABE
evalOptM x = evalM (optimize (x)) --evaluates optimized x (can also be done with the combined interpreter)
evalOptM _ = Nothing



