{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- AST and Type Definitions
data TYPELANG where 
	TNum :: TYPELANG
	TBool :: TYPELANG
	(:->:) :: TYPELANG->TYPELANG->TYPELANG
	deriving (Show,Eq)
data VALUELANG where
  NumV :: Int -> VALUELANG
  ClosureV :: String -> TERMLANG -> ValueEnv -> VALUELANG
  deriving (Show,Eq)


type TermEnv = [(String,TERMLANG)]
type Cont = [(String,TYPELANG)]
type ValueEnv = [(String,VALUELANG)]

data TERMLANG where
  			 Num :: Int -> TERMLANG
  			 Id :: String -> TERMLANG
  			 Plus :: TERMLANG -> TERMLANG -> TERMLANG
 			 Minus :: TERMLANG -> TERMLANG -> TERMLANG
  			 Mult :: TERMLANG -> TERMLANG -> TERMLANG
  			 Div :: TERMLANG -> TERMLANG -> TERMLANG
  			 Boolean :: Bool -> TERMLANG
  			 And :: TERMLANG -> TERMLANG -> TERMLANG
  			 Or :: TERMLANG -> TERMLANG -> TERMLANG
  			 Leq :: TERMLANG -> TERMLANG -> TERMLANG
  			 IsZero :: TERMLANG->TERMLANG
  			 If :: 	TERMLANG-> TERMLANG-> TERMLANG-> TERMLANG
  			 Lambda :: String -> TERMLANG -> TERMLANG
  			 App :: TERMLANG -> TERMLANG -> TERMLANG
  			 deriving (Show,Eq)


typeofM :: Cont -> TERMLANG -> (Maybe TYPELANG)
typeofM g (Num n) = if n>=0 then return TNum else Nothing
typeofM g (Id i) = (lookup i g)
typeofM g (Plus l r) = do {TNum <- typeofM g l;
                         TNum <- typeofM g r;
                            return TNum}
typeofM g (Minus l r) = do {TNum <- typeofM g l;
                         TNum <- typeofM g r;
                            return TNum}
typeofM g (Mult l r) = do {TNum <- typeofM g l;
                         TNum <- typeofM g r;
                            return TNum}
typeofM g (Div l r) = do {TNum <- typeofM g l;
                         TNum <- typeofM g r;
                            return TNum}
typeofM g (Boolean b) = return TBool
typeofM g (And l r) = do {TBool <- typeofM g l;
                       TBool <- typeofM g r;
                            return TBool}
typeofM g (Or l r) = do {TBool <- typeofM g l;
                            TBool <- typeofM g r;
                            return TBool}
typeofM g (Leq l r) = do {TNum<- typeofM g l;
                            TNum <- typeofM g r;
                            return TBool}
typeofM g(IsZero x) = do {TNum<- typeofM g x;
                            return TBool}
typeofM g (If c t e) = do {TBool <- typeofM g c;
                             t' <- typeofM g t;
                             e' <- typeofM g e;
                             if t' == e' then return t' else Nothing}

typeof g (Lambda i d b) = do {r <- typeof (i,d):g b;
                              return d :->: r}
typeof g (App f a) = do { a' <- typeof g a;
                          d :->: r <- typeof g f;
                          if a'==d then return r else Nothing}
