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
  ClosureV :: String -> TYPELANG ->TERMLANG -> ValueEnv -> VALUELANG
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
  Fix :: TERMLANG -> TERMLANG
  deriving (Show,Eq)

 
data EXTLANG where
  NumX :: Int -> EXTLANG
  IdX :: String -> EXTLANG
  PlusX :: EXTLANG -> EXTLANG -> EXTLANG
  MinusX :: EXTLANG -> EXTLANG -> EXTLANG
  MultX :: EXTLANG -> EXTLANG -> EXTLANG
  DivX :: EXTLANG -> EXTLANG -> EXTLANG
  BooleanX :: Bool -> EXTLANG
  AndX :: EXTLANG -> EXTLANG -> EXTLANG
  OrX :: EXTLANG -> EXTLANG -> EXTLANG
  LeqX :: EXTLANG -> EXTLANG -> EXTLANG
  IsZeroX :: EXTLANG->EXTLANG
  IfX :: EXTLANG -> EXTLANG -> EXTLANG -> EXTLANG
  LambdaX :: String -> TYPELANG->EXTLANG -> EXTLANG
  AppX :: EXTLANG -> EXTLANG -> EXTLANG
  BindX :: String  -> TYPELANG -> EXTLANG -> EXTLANG -> EXTLANG
  FixX :: EXTLANG->EXTLANG
  WhileX :: String -> TYPELANG -> EXTLANG-> EXTLANG -> EXTLANG ->EXTLANG
  FactX :: EXTLANG->EXTLANG
  deriving (Show,Eq)


-- substitution
subst :: String -> TERMLANG -> TERMLANG -> TERMLANG
subst i v (Num x) = (Num x)
subst i v (Id id) = if i==id then v else (Id id)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Boolean b) = (Boolean b)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero v') = (IsZero (subst i v v'))
subst i v (If c t e) = (If (subst i v c) (subst i v t) ( subst i v e))
subst i v (Lambda i' t b) = (Lambda i' t (subst i v b))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (Fix f) = (Fix (subst i v f))

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
typeofM g (Fix t) = do { (d :->: r) <- (typeofM g t);
                         return r}


evalM :: ValueEnv -> TERMLANG -> (Maybe VALUELANG)
evalM e (Num n) = if n<0 then Nothing else Just (NumV n)
evalM e (Id i) = (lookup i e)
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
evalM e (Lambda i d b) = Just (ClosureV i d b e)
evalM e (App f a) = do{ (ClosureV i d b e) <- (evalM e f);
                        (a') <- (evalM e a);
                        (evalM ((i,a'):e) b)}
evalM e (Fix t) = do{ (ClosureV i d b e') <- (evalM e t);
                      (evalM e' (subst i (Fix (Lambda i d b)) b))}




elabTerm :: EXTLANG -> TERMLANG
elabTerm (NumX n) = (Num n)
elabTerm(IdX i)= (Id i)
elabTerm (PlusX l r) = (Plus (elabTerm l) (elabTerm r))
elabTerm (MinusX l r) = (Minus (elabTerm l) (elabTerm r))
elabTerm (MultX l r) = (Mult (elabTerm l) (elabTerm r))
elabTerm (DivX l r) = (Div (elabTerm l) (elabTerm r))
elabTerm (BooleanX b)= (Boolean b)
elabTerm (AndX l r) = (And (elabTerm l) (elabTerm r))
elabTerm (OrX l r) = (Or (elabTerm l) (elabTerm r))
elabTerm (LeqX l r) = (Leq (elabTerm l) (elabTerm r))
elabTerm (IsZeroX v) = (IsZero(elabTerm v))
elabTerm (IfX c t n) = (If (elabTerm c)(elabTerm t) (elabTerm n))
elabTerm (LambdaX i d b) = (Lambda i d (elabTerm b))
elabTerm (AppX l r) = (App(elabTerm l) (elabTerm r))
elabTerm (FixX t) = (Fix (elabTerm t))
elabTerm(BindX i d v b)=(App (Lambda i d (elabTerm b)) (elabTerm v))
elabTerm(WhileX i d v c b)=(elabTerm( BindX "f" ((:->:) TNum TNum) (LambdaX "g" ((:->:) TNum TNum) (LambdaX i TNum (IfX  (c) (AppX(IdX "g")(b)) (IdX i)))) (AppX (FixX (IdX "f"))  (v))))
elabTerm(FactX m) =(elabTerm(BindX "f" ((:->:) TNum TNum) (LambdaX "g" ((:->:) TNum TNum) (LambdaX "x" TNum (IfX (IsZeroX (IdX "x")) (NumX 1) (MultX (IdX "x") (AppX (IdX "g") (MinusX (IdX "x") (NumX 1))))))) (AppX (FixX (IdX "f")) (m))))














evalTerm :: ValueEnv -> EXTLANG -> (Maybe VALUELANG)
evalTerm e f = evalM e (elabTerm f)


main :: IO()
main = do
        print (typeofM[("n",(TNum))](App(Lambda "x" TNum (Plus(Id "x")(Id "n")))(Num 4)))
        print (evalM [("n",(NumV 3))] (App(Lambda "x" TNum (Plus(Id "x")(Id "n")))(Num 4)))
        print (evalM [("x",(NumV 4)),("y", NumV 4),("b", (BoolV False))] (If (Id "b") (Mult(Id "x")(Id "y")) (Minus(Id "x")(Id "y"))))
        print ( evalTerm [] (BindX "f" ((:->:) TNum TNum) (LambdaX "g" ((:->:) TNum TNum)
                                    (LambdaX "x" TNum (IfX (IsZeroX (IdX "x")) (NumX 1)
                                                        (MultX (IdX "x")
                                                              (AppX (IdX "g")
                                                                    (MinusX (IdX "x")
                                                                          (NumX 1)))))))
                         (AppX (FixX (IdX "f")) (NumX 5))))




       