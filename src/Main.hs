import Data.Char

-- ok, this is a comment, and I'm going to type some Haskell

-- BTW, you can load this file in the "ghci" interpreter by typing
-- ":l lexer" or ":l lexer.hs", and reload it with ":r".

-- GHCi can be installed as part of the "GHC" Haskell toolchain. It should
-- be possible to install GHC on your hardware. It's also available on the
-- CSE machines, which should be sufficient for experiments prior to
-- assignment 1.

-- the goal is to lex a string like this
example_string :: String
example_string = "int f (int x) {\n return 1;\n }"

data Token
  = Ident String
  | Lit Int
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semi
  | GreaterThan
  | LessThan
  | Equals
  | Minus
  | Plus
  | Return
  deriving (Show)

data My_List a = Nil | Cons a (My_List a)
  deriving (Show)

add_my_list :: My_List Int -> Int
add_my_list xs = case xs of
  Nil -> 0
  Cons x xs -> x + add_my_list xs

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

-- almost working. exercise to the reader: treat reserved word tokens like
-- "return", "if", "else" specially
lexer :: String -> [Token]
lexer ('{' : cs) = LBrace : lexer cs
lexer ('}' : cs) = RBrace : lexer cs
lexer ('(' : cs) = LParen : lexer cs
lexer (')' : cs) = RParen : lexer cs
lexer (';' : cs) = Semi : lexer cs
lexer ('>' : cs) = GreaterThan : lexer cs
lexer ('<' : cs) = LessThan : lexer cs
lexer ('=' : cs) = Equals : lexer cs
lexer ('-' : cs) = Minus : lexer cs
lexer ('+' : cs) = Plus : lexer cs
lexer [] = []
lexer (c : cs) =
  if isSpace c
    then lexer cs
    else
      if isDigit c
        then
          let (int_string, rest) = break (comp not isDigit) (c : cs)
           in (Lit (read int_string) : lexer rest)
        else
          if isAlpha c
            then
              let (ident_string, rest) = break (\c -> not (isAlpha c || isDigit c)) (c : cs)
               in (Ident ident_string : lexer rest)
            else error ("couldn't lex this: " ++ show (c : cs))

-- this might help explain the "let" syntax a little
example_let = let x = 1 in x + x

example_let2 = (\x -> x + x) 1
