module Rose where

data Rose a = Node a (Forest a)
    deriving Show

data Forest a = Empty | Cons (Rose a) (Forest a)
    deriving Show

data RoseAlt a = NodeAlt a [RoseAlt a]

size :: Rose a -> Int
size(Node x f) = 1 + sizeF f
sizeF :: Forest a -> Int
sizeF Empty = 0
sizeF (Cons r f) = size r + sizeF f

height :: Rose a -> Int
height (Node x f) = 1 + heightF f
heightF :: Forest a -> Int
heightF Empty = 0
heightF (Cons r f) = max (height r) (heightF f)

data Expr = Num Int
          | Add Expr Expr
          | Mul Expr Expr
          | Var String deriving Show

constantFolder :: Expr -> Expr
constantFolder(Num n) = Num n
constantFolder(Var x) = Var x
constantFolder(Add e1 e2) =
    let e1' = constantFolder e1
        e2' = constantFolder e2
    in
        case (e1',e2') of
            (Num n, Num m) -> Num(n + m)
constantFolder(Mul e1 e2) = 
    let e1' = constantFolder e1
        e2' = constantFolder e2
    in
        case (e1', e2') of
            (Num n, Num m) -> Num(n * m)
            _ -> Mul e1' e2'
