{-
  TEAM MEMBERS:
    - Shane Barrantes
    - Ty Skelton
-}

module MiniLogo where

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
