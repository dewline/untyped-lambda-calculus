
-- what's missing from the current parser?
-- - numeric types: 0, succ t, pred t, iszero t
-- - parens and paren-matching
-- parse time errors:
--   DONE (kinda) overflow info.. i.e. "true false" should error
--     - responsibility of caller, not parser
--   DONE underflow info .. i.e. "if true then false" should error
--   DONE (kinda) providing file, line and column for each error
--     - i've set up the framework for this, but not actual code
-- eval time errors:
--  - error on numeric mismatch
--  - underflowing Zero should also error; "prev 0" is not valid

module Main where

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn $ "words are: " ++ testexpr
  putStr "result is: "
  print (utparse (words testexpr) Nothing)
  putStrLn $ "words are: " ++ testexpr2
  putStr "result is: "
  print (utparse (words testexpr2) Nothing)

testexpr :: String
testexpr = "if true then false else true"
testexpr2 = "if if false then false else false \
             \ then false else if true then true else false"

-------------------
-- PARSING --------
-------------------
-- 1. we don't have to worry about lexing just yet since 'words'
--    just does the right thing for us. (splits into tokens)
-- 2. at parse time, i'd want to directly call something like: 
--        Term t = parse $ words input
--    and get a Term tree from this. but we cant directly do this 
--    since we need to keep track of the next tokens to read.
--    this means that we need to hold both a Term and the string
--    array that is yet to be parsed.
-- 3. We'd also want to report parse-time errors to the end user
--      this is doable by holding the next expected token
--      (like "else", "then") as part of the datatype.
--      Then if we find that a completion is not available we can
--      error out where appropriate
-- 4. The problem with doing this is that it is somewhat inelegant
--      Also we wouldn't be able to point to the exact location
--      where the issue is occurring.
--      Instead, we can just store the Term tree parsed, and check
--      on return of the parse function is the returned term
--      matches the type of the expected term.
-- 5. What about error handling?
--      - We can throw exceptions at parse-time...
--      - Alternatively we can return a Maybe ParsedTerm
--        the caller can check this and decide whether to proceed.
--      - But if we return Nothing that wouldn't help
--      - We can parse "Maybe (Term *)".. then we'd return a PT
--        that contains Nothing :: Term and nontrivial remainder.
--        callers can then throw their own error pointing to the
--        location of the error.
--        - but this loses the cause of the error right?
--          we need an ParseError type I guess
--      - I have a ParseError type that now encodes all parse-time
--        errors...not using String indexes since that feels hacky.
--        it feels like a SoC violation...the ParseError should
--        have all the context needed to report the problem, and
--        should not rely being an index to an external [] String.

data ParseError = MissingThenExpr ([] String)
                | MissingElseExpr ([] String)
                | UnexpectedToken ([] String)
                | UnmatchableToken ([] String) String
                | UnexpectedEndOfInput
                deriving (Eq, Show)

data ParsedTerm a = PT { term :: Term a
                       , remainder :: [] String
                       } deriving Show

type ParseResult a = Either ParseError (ParsedTerm a)

-- utparse takes a string array as input, along with an 'expected'
-- parse token.
utparse :: [] String -> Maybe String -> ParseResult a
utparse ("true" :xs) Nothing       = Right $ PT True' xs
utparse ("false":xs) Nothing       = Right $ PT False' xs
utparse ("if"   :xs) Nothing       = utparseif xs
utparse ("then" :xs) (Just "then") = utparse xs Nothing
utparse ("else" :xs) (Just "else") = utparse xs Nothing
utparse x  (Just "then") = Left $ MissingThenExpr x
utparse x  (Just "else") = Left $ MissingElseExpr x
utparse [] _             = Left $ UnexpectedEndOfInput
utparse x  (Just y)      = Left $ UnmatchableToken x y
utparse x  Nothing       = Left $ UnexpectedToken x

-- factoring out the "if" statement matching since it is getting
-- quite complicated.
-- we need to pass errors through, while also matching and 
-- accumulating "then" and "else" into a Cond.
utparseif :: [] String -> ParseResult a
utparseif x = do
  (PT ifTerm   ifStr)   <- utparse x       Nothing
  (PT thenTerm thenStr) <- utparse ifStr   (Just "then")
  (PT elseTerm elseStr) <- utparse thenStr (Just "else")
  pure $ PT (Cond ifTerm thenTerm elseTerm) elseStr

-- 'Term' is basically ASTNode
-- We parse into a tree of Term s, but we don't check semantic
-- correctness until evaluation. Therefore we could end up with
-- nonsensical Term subtrees like (Cond (Succ 0) True False) or
-- (Succ True).
-- adding 0 value and succ, pred, iszero to represent arithexprs
-- what's a good way really to express termination states?
--  - i wanted to have a datatype for values that are the
--    leaves of the Term tree, but not sure how to do this really.
data Term a = Cond (Term a) (Term a) (Term a)
            | Succ (Term a)
            | Pred (Term a)
            | IsZero (Term a)
            | True'
            | False'
            | Zero'
            deriving (Eq, Show)

-- evaluation
uteval :: Term a -> Term a
uteval True' = True'
uteval False' = False'
uteval (Cond True'  b _) = uteval b
uteval (Cond False' _ c) = uteval c
uteval (Cond a b c)      = uteval $ Cond (uteval a) b c

