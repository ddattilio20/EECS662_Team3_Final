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
  BoolV :: Bool -> VALUELANG
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
  If :: TERMLANG-> TERMLANG-> TERMLANG-> TERMLANG
  Lambda :: String -> TYPELANG -> TERMLANG ->TERMLANG
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

typeofM g (Lambda i d b) = do {r <- typeofM ((i,d):g) b;
                              return (d:->:r)}
typeofM g (App f a) = do { a' <- typeofM g a;
                          (d:->:r) <- typeofM g f;
                          if a'==d then return r else Nothing}


evalM :: ValueEnv -> TERMLANG -> (Maybe VALUELANG)
evalM e (Num x) = Just (NumV x)
evalM e (Id id) = lookup id e
evalM e (Plus l r) = do{ (NumV l') <- (evalM e l);
                         (NumV r') <- (evalM e r);
                         return (NumV (l' + r'))}
evalM e (Minus l r) = do{ (NumV l') <- (evalM e l);
                         (NumV r') <- (evalM e r);
                         return (NumV (l' - r'))}
evalM e (Mult l r) = do{ (NumV l') <- (evalM e l);
                         (NumV r') <- (evalM e r);
                         return (NumV (l' * r'))}
evalM e (Div l r) = do{ (NumV l') <- (evalM e l);
                         (NumV r') <- (evalM e r);
                         if r'==0 then Nothing
                         else return (NumV (l' `div` r'))}
evalM e (Boolean b) = Just (BoolV b)
evalM e (And l r) = do{ (BoolV l') <- (evalM e l);
                        (BoolV r') <- (evalM e r);
                        return (BoolV (l' && r'))}
evalM e (Or l r) = do{ (BoolV l') <- (evalM e l);
                        (BoolV r') <- (evalM e r);
                        return (BoolV (l' || r'))}
evalM e (Leq l r) = do{ (NumV l') <- (evalM e l);
                        (NumV r') <- (evalM e r);
                        return (BoolV (l' <= r'))}
evalM e (IsZero v) = do{ (NumV v') <- (evalM e v);
                          return (BoolV (v'==0))}
evalM e (If c t e') = do{ (BoolV c') <- (evalM e c);
                         (if c' then (evalM e t) else (evalM e e'))}
evalM e (Lambda i d b) = Just (ClosureV i b e)
evalM e (App f a) = do{ (ClosureV i b e) <- (evalM e f);
                        (a') <- (evalM e a);
                        (evalM ((i,a'):e) b)}

main :: IO()
main = do
        print (typeofM[("n",(TNum))](App(Lambda "x" TNum (Plus(Id "x")(Id "n")))(Num 4)))
        print (evalM [("n",(NumV 3))] (App(Lambda "x" TNum (Plus(Id "x")(Id "n")))(Num 4)))
        print (evalM [("x",(NumV 4)),("y", NumV 4),("b", (BoolV False))] (If (Id "b") (Mult(Id "x")(Id "y")) (Minus(Id "x")(Id "y"))))