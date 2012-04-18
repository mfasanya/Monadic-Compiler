--G52AFP Coursework 2 - Monadic Compiler
   
--Tom Bowden, Charlie Morgan
--psytb@nottingham.ac.uk, psycm@nottingham.ac.uk


--Imperative language
-------------------

data Prog                    =  Assign Name Expr
                             |  If Expr Prog Prog
                             |  While Expr Prog
                             |  Sequence [Prog]
                                deriving Show
       
data Expr                    =  Val Int | Var Name | App Op Expr Expr
                                deriving Show
       
type Name                    =  Char
       
data Op                      =  Add | Sub | Mul | Div
                                deriving Show


--Virtual machine
---------------

type Stack                   =  [Int]
       
type Mem                     =  [(Name,Int)]
       
type Code                    =  [Inst]
       
data Inst                    =  PUSH Int
                             |  PUSHV Name
                             |  POP Name
                             |  DO Op
                             |  JUMP Label
                             |  JUMPZ Label
                             |  LABEL Label
                                deriving Show
       
type Label                   =  Int

--Factorial example
-----------------

fac                          :: Int -> Prog
fac n                        =  Sequence [Assign 'A' (Val 1),
                                          Assign 'B' (Val n),
                                          While (Var 'B') (Sequence
                                             [Assign 'A' (App Mul (Var 'A') (Var 'B')),
                                              Assign 'B' (App Sub (Var 'B') (Val 1))])]


--State transformer monad
-----------------

-- needs to use data mechanism to make ST into an instance of a class
data ST a                    =  S ( State -> (a, State))

-- removes the dummy constructor
apply                        :: ST a -> State -> (a, State)
apply (S f) x                =  f x


instance Monad ST where
  -- return                  :: a -> ST a
  return x                   =  S (\s -> (x, s))
  --(>>=)                    :: ST a -> (a -> ST b) -> ST b
  st >>= f                   =  S (\s -> let (x,s') = apply st s in apply (f x) s')
       

type State                   =  Label
       
fresh                        :: ST Int
fresh                        =  app (+1)
       
app                          :: (State -> State) -> ST State
app f                        =  S (\n -> (n, f n))
       
run                          :: ST a -> State -> a
run p s                      =  fst (apply p s) 


--Compiler code
-----------------

--compiles program and expressions, assigining fresh labels when needed
comp                         :: Prog -> Code
comp p                       =  run (compProg p) 0

--compile program
compProg                     :: Prog ->  ST Code

compProg (Sequence [])       =  return []

compProg (Sequence (x:xs))   =  do cx   <- compProg x
                                   cSeq <- compProg (Sequence xs)
                                   return (cx ++ cSeq) 

compProg (Assign n xpr)      =  return (compExp xpr ++ [POP n])

compProg (While xpr s)       =  do l    <- fresh
                                   l'   <- fresh
                                   cSeq <- compProg s
                                   return ([LABEL l] ++ compExp xpr ++ [JUMPZ l'] ++ cSeq ++ [JUMP 0, LABEL l'])

compProg (If xpr p1 p2)      =  do l     <- fresh
                                   l'    <- fresh
                                   cTSeq <- compProg p1 -- compiled true sequence
                                   cFSeq <- compProg p2 -- compiled false sequence
                                   return (compExp xpr ++ [JUMPZ l] ++ cTSeq ++ [JUMP l', LABEL l] ++ cFSeq ++ [LABEL l'])





-- compile expressions
compExp                      :: Expr -> Code

compExp (Val n)              =  [PUSH n]

compExp (Var v)              =  [PUSHV v]

compExp (App op xp1 xp2)     =  compExp xp1 ++ compExp xp2 ++ [DO op]