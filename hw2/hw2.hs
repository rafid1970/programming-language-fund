{-
  TEAM MEMBERS:
    - Shane Barrantes
    - Ty Skelton
    - Griffin Gonsalves
-}

module HW2 where


-- 1. Stack Language
-- Uses S language to create a stack of commands C
-- generates a new stack before each op is applied
-- final stack created is result

type Prog = [Cmd]

data Cmd = LD Int
          | ADD
          | MULT
          | DUP
type Stack = [Int]

--sem :: Prog -> D
--semCmd :: Cmd -> D

--test with [LD 3,DUP,ADD,DUP,MULT] and [LD 3,ADD] and the empty stack []
--cases and maybes?
