{-
  TEAM MEMBERS:
    - Shane Barrantes
    - Ty Skelton
-}

module MiniLogo where

-- 1. (a) Define the abstract syntax for Mini Logo as a Haskell data type

data Cmd  = Pen Mode              -- Pen state
          | MoveTo Pos Pos        -- Move Pen
          | Def String Pars Cmd   -- Define a macro
          | Calls String Vals     -- Call a macro
          | Exp Cmd Cmd           -- Expand command space
          | Nop                   -- Empty
     deriving (Show, Eq)

data Mode = Up | Down
     deriving (Show, Eq)

data Pos  = Num Int | Name String
     deriving (Show, Eq)

data Pars = Params String Pars | Param String
     deriving (Show, Eq)

data Vals = Values Int Vals | Value Int
     deriving (Show, Eq)

--  1. (b) Write a Mini Logo macro vector that draws a line from a given position (x1,y1) to a given position (x2,y2)
--  and represent the macro in abstract syntax, that is, as a Haskell data type value.

-- Concrete:
-- def vector (x1, y1, x2, y2) Pen Up; MoveTo x1 y1; Pen Down; MoveTo x2 y2; Pen Up;

-- Abstract:
vector :: Cmd
vector = Def "vector" (Params "x1" (Params "y1" (Params "x2" (Param "y2"))))
          (Exp
            (Pen Up)
            (Exp
              (MoveTo (Name "x1") (Name "y1"))
              (Exp
                (Pen Down)
                (MoveTo (Name "x2") (Name "y2"))
              )
            )
          )

-- 1. (c) Define a Haskell function steps :: Int -> Cmd that constructs a Mini Logo program which draws a stair of n
-- steps.

steps :: Int -> Cmd
steps 0 = Nop
steps 1 = Exp
            (Calls "vector" (Values 1 (Values 1 (Values 0 (Value 1)))))
            (Calls "vector" (Values 0 (Values 1 (Values 0 (Value 0)))))
steps n = Exp
            (Calls "vector" (Values n (Values n (Values (n-1) (Value n)))))
            (Exp
                (Calls "vector" (Values (n-1) (Values n (Values (n-1) (Value (n-1))))))
                (steps (n-1)))

-- (a) Define the abstract syntax for the above language as a Haskell data type.
data Link     =  S (Int, Int) (Int, Int)
              deriving (Show, Eq)

data Links    = Ls Link Links | L Link
              deriving (Show, Eq)

data GateFn   = And
              | Or
              | Xor
              | Not
              deriving (Show, Eq)

data Gates    = G (Int, GateFn) Gates
              | Noop
              deriving (Show, Eq)

data Circuit  = C Gates Links
              deriving (Show, Eq)

-- (b) Represent the half adder circuit in abstract syntax, that is, as a Haskell data type value.
circuit :: Circuit
circuit = C(G(1, Xor)(G(2, And)Noop))(Ls(S(1,1)(2,1))(L(S(1,2)(2,2))))
-- circuit = C
--             (G
--               (1, Xor)
--               (G
--                 (2, And)
--                 Noop)
--             )
--             (Ls
--               (S
--                 (1,1)
--                 (2,1)
--               )
--               (L
--                 (S
--                   (1,2)
--                   (2,2)
--                 )
--               )
--             )

-- (c) Define a Haskell function that implements a pretty printer for the abstract syntax.

ppGateFn :: GateFn -> String
ppGateFn And = "And"
ppGateFn Or = "Or"
ppGateFn Xor = "Xor"
ppGateFn Not =  "Not"

ppGates :: Gates -> String
ppGates Noop = ""
ppGates (G (x, fn) gates) = "(" ++ show x ++ ", " ++ (ppGateFn fn) ++ ")" ++ ppGates gates

ppLink :: Link -> String
ppLink (S (p1, g1) (p2, g2)) = "From ("++ show p1 ++ ", " ++ show g1 ++") to (" ++ show p2 ++", "++ show g2 ++ ")"

ppLinks :: Links -> String
ppLinks (Ls link links) = (ppLink link) ++ (ppLinks links)
ppLinks (L link) = ppLink link

ppCircuit :: Circuit -> String
ppCircuit (C gs ls) = (ppGates gs) ++ (ppLinks ls)

data Expr = Number Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
          deriving (Show, Eq)

data Op  = Add | Multiply | Negate
         deriving (Show, Eq)
data Exp = N Int | Apply Op [Exp]
         deriving (Show, Eq)

-- 3) a. represent the expression -(3+4)*7 in the alternative abstract syntax.
equation :: Exp
equation = Apply Multiply [N 7, Apply Negate [Apply Add [N 3, N 4]]]

-- 3) b. What are the advantages or disadvantages of either representation?
-- One advantage of using the second abstract syntax is that by seperating operations and expressions into two data types so it is easier to interpret
-- what's occuring at each step. One advantage of using the first abstract syntax is by containing in one data type we're creating a more succinct
-- definition.

-- 3) c. Define a function translate :: Expr -> Exp that translates expressions given in the first abstract syntax into
--       equivalent expressions in the second abstract syntax.
translate :: Expr -> Exp
translate (Number n)        = (N n)
translate (Plus exp1 exp2)  = Apply Add [(translate exp1), (translate exp2)]
translate (Times exp1 exp2) = Apply Multiply [(translate exp1), (translate exp2)]
translate (Neg exp1)        = Apply Negate [(translate exp1)]
