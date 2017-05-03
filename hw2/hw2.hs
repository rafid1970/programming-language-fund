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

type Prog   = [Cmd]
type Stack  = [Int]
type D      = Maybe Stack -> Maybe Stack

data Cmd    = LD Int
            | ADD
            | MULT
            | DUP
            deriving (Eq, Show)

run :: Prog -> Maybe Stack
run ps = sem ps (Just [])

sem :: Prog -> D
sem [] xs = xs
sem (p:ps) xs = sem ps (semCmd p xs)

semCmd :: Cmd -> D
semCmd (LD n) xs = case xs of Just xs -> Just (n:xs)
                              _       -> Nothing
semCmd (ADD) xs  = case xs of Just (x:x':xs) -> Just ((x+x'):xs)
                              _              -> Nothing
semCmd (MULT) xs = case xs of Just (x:x':xs) -> Just ((x*x'):xs)
                              _              -> Nothing
semCmd (DUP)  xs = case xs of Just (x:xs)    -> Just (x:x:xs)
                              _              -> Nothing

{-
  test output:
  -- run [LD 3,DUP,ADD,DUP,MULT] == [36]
  -- run [LD 3, ADD] == Nothing
  -- run [] == Just []
-}
