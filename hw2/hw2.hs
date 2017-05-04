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

-- 2. Extending the Stack Language by Macros

-- Extend the abstract syntax to represent macro definitions and calls, that is, give a correspondingly changed data
-- definition for Cmd.

type Prog   = [Cmd]
type Prog2  = (Prog, State)
type Stack  = [Int]
type D      = Maybe Stack -> Maybe Stack
type State  = (Macros, Maybe Stack)
type Macros = [(String, Prog)]

data Cmd    = LD Int
            | ADD
            | MULT
            | DUP
            | DEF String Prog
            | CALL String
            deriving (Eq, Show)

run :: Prog -> Maybe Stack
run ps = sem ps (Just [])

sem :: Prog -> D
sem [] xs = xs
sem (p:ps) xs = sem ps (semCmd p xs)

semCmd :: Cmd -> D
semCmd (LD n) xs = case xs of Just xs        -> Just (n:xs)
                              _              -> Nothing
semCmd (ADD) xs  = case xs of Just (x:x':xs) -> Just ((x+x'):xs)
                              _              -> Nothing
semCmd (MULT) xs = case xs of Just (x:x':xs) -> Just ((x*x'):xs)
                              _              -> Nothing
semCmd (DUP)  xs = case xs of Just (x:xs)    -> Just (x:x:xs)
                              _              -> Nothing

{-
  test output:
  -- run [LD 3,DUP,ADD,DUP,MULT] == Just [36]
  -- run [LD 3, ADD] == Nothing
  -- run [] == Just []
-}

run2 :: Prog -> Maybe Stack
run2 ps = sem2Wrapper (ps, ([],(Just [])))

sem2Wrapper :: Prog2 -> Maybe Stack
sem2Wrapper ([], (_, xs)) = xs
sem2Wrapper (p:ps, s) = sem2Wrapper (ps, (semCmd2 p s))

sem2 :: Prog2 -> State
sem2 ([], s) = s
sem2 (p:ps, s) = sem2 (ps, (semCmd2 p s))

semCmd2 :: Cmd -> State -> State
semCmd2 (LD n) (ms, xs)      = case xs of Just xs          -> (ms, (Just (n:xs)))
                                          _                -> (ms, Nothing)
semCmd2 (ADD) (ms, xs)      = case xs of Just (x:x':xs)    -> (ms, (Just ((x+x'):xs)))
                                         _                 -> (ms, Nothing)
semCmd2 (MULT) (ms, xs)     = case xs of Just (x:x':xs)    -> (ms, (Just ((x*x'):xs)))
                                         _                 -> (ms, Nothing)
semCmd2 (DUP) (ms, xs)      = case xs of Just (x:xs)       -> (ms, (Just (x:x:xs)))
                                         _                 -> (ms, Nothing)
semCmd2 (DEF n ps) (ms, xs) = ((n, ps):ms, xs)
semCmd2 (CALL n) (ms, xs)   = case (isMacro n ms) of True  -> (sem2((getMacro n ms),(ms,xs)))
                                                     False -> (ms, Nothing)

isMacro :: String -> Macros -> Bool
isMacro _ [] = False
isMacro a ((a', p):ms) | a == a' = True
                       | otherwise = isMacro a ms

getMacro :: String -> Macros -> Prog
getMacro _ [] = []
getMacro n ((m,p):ms) | n == m = p
                      | otherwise = (getMacro n ms)

data Cmd1 = Pen Mode
          | MoveTo Int Int
          | Seq Cmd1 Cmd1
          deriving Show

data Mode = Up | Down
          deriving Show

type State1 = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)

type Lines = [Line]

semS :: Cmd1 -> State1 -> (State1, Lines)
semS (Pen m) (_, x, y)        = ((m, x, y), [])

semS (MoveTo x' y') (m, x, y) = case m of Up   -> ((m, x', y'), [])
                                          Down -> ((m, x', y'), [(x, y, x', y')])
semS (Seq c c') (m, x, y)     = ((m'', x'', y''), (l' ++ l'')) where
                                ((m', x', y'), l') = semS c (m, x, y)
                                ((m'', x'', y''), l'') = semS c' (m', x', y')

sem' :: Cmd1 -> Lines
sem' c = l where
         (s, l) = semS c (Up, 0, 0)

{-
  test output:
  -- run2 [DEF "test" [LD 2, DUP, ADD], CALL "test"] == Just [4]
  -- run2 [DEF "test" [LD 2, DUP, DEF "nest_test" [LD 4, MULT]], CALL "test", CALL "nest_test"] == Just [8,2]
-}
