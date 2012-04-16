--G52AFP Coursework 2 - Monadic Compiler
   
--Your full name(s)
--Your full email address(es)


--Imperative language
-------------------

data Prog             =  Assign Name Expr
                      |  If Expr Prog Prog
                      |  While Expr Prog
                      |  Sequence [Prog]
                         deriving Show

data Expr             =  Val Int | Var Name | App Op Expr Expr
                         deriving Show

type Name             =  Char

data Op               =  Add | Sub | Mul | Div
                         deriving Show


--Virtual machine
---------------

type Stack            =  [Int]

type Mem              =  [(Name,Int)]

type Code             =  [Inst]

data Inst             =  PUSH Int
                      |  PUSHV Name
                      |  POP Name
                      |  DO Op
                      |  JUMP Label
                      |  JUMPZ Label
                      |  LABEL Label
                         deriving Show

type Label            =  Int


--Factorial example
-----------------

fac                   :: Int -> Prog
fac n                 =  Sequence [Assign 'A' (Val 1),
                                   Assign 'B' (Val n),
                                   While (Var 'B') (Sequence
                                      [Assign 'A' (App Mul (Var 'A') (Var 'B')),
                                       Assign 'B' (App Sub (Var 'B') (Val 1))])]


--Compiler code
-----------------

comp                  :: Prog -> Code

comp (Seqn [])        =  return []
comp (Seqn (p:ps)     =  do 
                             return (comp p ++ comp ps)
comp (Assign n e)     =
comp (While e p)      =
comp (If e t f)       =
