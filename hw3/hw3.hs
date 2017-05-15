{-
  TEAM MEMBERS:
    - Shane Barrantes
    - Ty Skelton
    - Griffin Gonsalves
-}

{-
    1. A rank-based type system for the stack language
    goals:
      - extend language (INC, SWAP, POP)
      - add ranking types (Rank, CmdRank)
      - add ranking functions (rankP, rankC, rank)
      - add type checking semantics (semStatTC)

    1. a) done below.
       b) sem's type and definition can now be simplified from using Maybe Stack
          and case switches because we do all of our type checking in semStatTC.
          This allows cleaner definitions over a stack we have proven has a valid
          rank so no statement results in an underflow.
-}

module HW3 where

type Prog     = [Cmd]
type Stack    = [Int]
type D        = Stack -> Stack
type Rank     = Int
type CmdRank  = (Int, Int)

data Cmd    = LD Int
            | ADD
            | MULT
            | DUP
            | INC
            | SWAP
            | POP Int
            deriving (Eq, Show)

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP p  = rank p 0

rank :: Prog -> Rank -> Maybe Rank
rank [] r    = Just r
rank (c:p) r = let (n,m) = rankC c
               in if n <= r then rank p (r - n + m)
                            else Nothing

rankC :: Cmd -> CmdRank
rankC (LD _)  = (0, 1)
rankC (ADD)   = (2, 1)
rankC (MULT)  = (2, 1)
rankC (DUP)   = (1, 2)
rankC (INC)   = (1, 1)
rankC (SWAP)  = (2, 2)
rankC (POP n) = (n, 0)

semStatTC :: Prog -> Maybe Stack
semStatTC p | (rankP p) >= Just 0 = Just (sem p [])
            | otherwise           = Nothing

sem :: Prog -> D
sem [] xs = xs
sem (c:cs) xs = sem cs (semCmd c xs)

semCmd :: Cmd -> D
semCmd (LD n)  xs        = n:xs
semCmd (ADD)   (x:x':xs) = (x+x'):xs
semCmd (MULT)  (x:x':xs) = (x*x'):xs
semCmd (DUP)   (x:xs)    = x:x:xs
semCmd (INC)   (x:xs)    = (x+1):xs
semCmd (SWAP)  (x:x':xs) = x':x:xs
semCmd (POP n) (xs)      | n > 0     = semCmd (POP (n-1)) (tail xs)
                         | otherwise = xs
