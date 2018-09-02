
-- what's missing from the current parser?
-- - numeric types: 0, succ t, pred t, iszero t
-- - parens and paren-matching
-- - error on numeric mismatch
-- - overflow info.. i.e. "true false" should error
-- - underflow info .. i.e. "if true then false" should error
-- - providing file, line and column info for each error

module Main where

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn $ "words are: " ++ testexpr
  putStr "result is: "
  print (utpartparse $ words testexpr)
  putStrLn $ "words are: " ++ testexpr2
  putStr "result is: "
  print (utpartparse $ words testexpr2)

testexpr :: String
testexpr = "if true then false else true"
testexpr2 = "if if false then false else false then false else if true then true else false"

utpartparse :: [String] -> TermParsable a
utpartparse ("true":xs) = TP True' xs
utpartparse ("false":xs) = TP False' xs
utpartparse ("then":xs) = utpartparse xs
utpartparse ("else":xs) = utpartparse xs
utpartparse ("if":xs) = let (TP t2 xs2) = utpartparse xs
                            (TP t3 xs3) = utpartparse xs2
                            (TP t4 xs4) = utpartparse xs3
                        in TP (Cond t2 t3 t4) xs4
utpartparse (x:xs) = error $ "term not found: " ++ x

data TermParsable a = TP (Term a) [String] deriving Show

getTerm :: TermParsable a -> Term a
getTerm (TP t s) = t

-- adding 0 term
data Term a = Cond (Term a) (Term a) (Term a)
            | True'
            | False'
            deriving (Eq, Show)

-- evaluation
uteval :: Term a -> Term a
uteval True' = True'
uteval False' = False'
uteval (Cond True'  b _) = uteval b
uteval (Cond False' _ c) = uteval c
uteval (Cond a b c)      = uteval $ Cond (uteval a) b c

