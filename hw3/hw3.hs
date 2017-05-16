{-
  TEAM MEMBERS:
    - Shane Barrantes
    - Ty Skelton
    - Griffin Gonsalves
-}

{-
    #1. A rank-based type system for the stack language
    goals:
      - extend language (INC, SWAP, POP)
      - add ranking types (Rank, CmdRank)
      - add ranking functions (rankP, rankC, rank)
      - add type checking semantics (semStatTC)

    a) done below.
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
{-
    testing #1:
    - semStatTC [LD 1, DUP, INC, SWAP] == Just [1,2]
    - semStatTC [LD 3, LD 4, MULT, INC, LD 4, DUP, DUP, POP 2, SWAP] == Just [13,4]
-}
-- ----------------------------------------------------------------
{-
   #2. Shape Language
   goals:
     - define a type checker of shape as it's resulting width/height.
     - define a type checker of shape as only rectangles.

   a) shown below (bbox definition).
   b) shown below (rect definition).
-}

data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show
type BBox = (Int, Int)


bbox :: Shape -> BBox
bbox (X) = (1,1)
bbox (TD b b') = (maximum [w,w'], h+h')
                 where (w, h)   = bbox b
                       (w', h') = bbox b'
bbox (LR b b') = (w+w', maximum[h,h'])
                 where (w, h)   = bbox b
                       (w', h') = bbox b'

rect :: Shape -> Maybe BBox
rect (X) = Just (1,1)
rect (TD b b') | w == w'   = Just (bbox (TD b b'))
               | otherwise =  Nothing
               where (w,_)  = bbox b
                     (w',_) = bbox b'
rect (LR b b') | h == h'   = Just (bbox (LR b b'))
               | otherwise =  Nothing
               where (_,h)  = bbox b
                     (_,h') = bbox b'

{-
    testing #2:
    - rect (TD X X) == Just (1,2)
    - rect (LR (TD X X) (TD X X))  == Just (2,2)
    - rect (LR (LR (TD X X) (TD X X)) X) == Nothing
-}
-- ----------------------------------------------------------------
{-
  #3. Parametric Polymorphism
  goals:

  a) 1) The type definitions for f and g are as follows:
        f :: [a] -> a -> [a]
        g :: [a] -> b -> [b]
     2) f returns either x or a list containing y. For the return type to match
        x must be defined as a list of y-type elements. Alternatively, g only
        returns either a null list or a list of y-type elements. This means x can
        hold any type while the function returns a list of y-type elements.
     3) g is more general, because x and [y] don't need to be equivalent in terms
        of type definition.
     4) f and g have different types because f has a more rigid definition whereas
        g allows some freedom in terms of parameter types.
  b) shown below (h).
  c) shown below (k)
  d) Yes it is possible to define a function that takes type a and returns something
     of type b. However, this compromises type safety by turning type a into type b
     using unsafe manipulations over the binary value of input a, transforming it
     into something of type b. A function of this definition already exists and
     is called unsafeCoerce.

     A wrapper function could use the existing implemenation like so:
     e.g.:
     unsafeCoerce' = unsafeCoerce

     Without using unsafeCoerce and maintaining typesafety I'd say no, because
     type b would be out of scope. One could suggest returning a
     constant b, but that would return the type of the constant, not b.
     Implementing a function with definition a -> b wouldn't be possible in a
     typesafe language.
-}

h bs ((a,b):ps) = b:bs

k x y = x $ y x

{-
    testing #3:
    - :t h == h :: [a] -> [(t, a)] -> [a]
    - :t k == k :: (t1 -> t) -> ((t1 -> t) -> t1) -> t
-}
