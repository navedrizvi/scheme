-- Haskell implementation of R5RS Scheme https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_8.html
-- source: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
module Main where
import System.Environment
{-
There are two ways to create an IO action (either directly or by calling a function that performs them):

Lift an ordinary value into the IO monad, using the return function.
Combine two existing IO actions.
Since we want to do two things, we'll take the second approach. The built-in action getArgs reads the command-line arguments and passes them along as a list of strings. The built-in function putStrLn takes a string and creates an action that writes this string to the console.

To combine these actions, we use a do-block. A do-block consists of a series of lines, all lined up with the first non-whitespace character after the do. Each line can have one of two forms:

name <- action1
action2
The first form binds the result of action1 to name, to be available in next actions. For example, if the type of action1 is IO [String] (an IO action returning a list of strings, as with getArgs), then name will be bound in all the subsequent actions to the list of strings thus passed along, through the use of "bind" operator >>=. The second form just executes action2, sequencing it with the next line (should there be one) through the >> operator. The bind operator has different semantics for each monad: in the case of the IO monad, it executes the actions sequentially, performing whatever external side-effects that result from actions. Because the semantics of this composition depend upon the particular monad used, you cannot mix actions of different monad types in the same do-block - only IO monad can be used (it's all in the same "pipe").

A new action thus created, which is a combined sequence of actions, is stored in the identifier main of type IO (). The Haskell compiler notices this monadic composition, and executes the action in it.
-}

import Text.ParserCombinators.Parsec hiding ( spaces )
import Control.Monad ( liftM )

-- value attached with (hidden/runtime) information is called "Monadic value".
--  >> (bind) operator is used behind the scenes to combine the lines of a do-block.
-- each line of do-block must have same type
-- $ operator is infix fn application. same as if we'd written "return (String x)". Same as 'apply' in lisp. Associates to right
main' :: IO ()
main' = do
    (expr:_) <- getArgs
    -- putStrLn (readExpr'' (args !! 0))
    putStrLn $ readExpr' expr

-- >>= is sequencing operator
-- we take result of getArgs action and pass it into composition of:
-- 1. take 1st val. (!! 0) notation is called operator section: tells compiler to partially-apply list indexing operator to 0, giving back a fn that takes 1st elem of whatever list it's passed 2. parse it 3. evaluate it 3. convert to string and 4. print it
main :: IO ()
-- main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
main = getArgs >>= (print . eval . readExpr . (!! 0))

-- define a parser that recognizes one of the symbols allowed in Scheme identifiers:
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
-- This is another example of a monad: the "extra info" that is being hidden is the info about position in the input stream, backtracking record, first and follow sets, etc. Parsec takes care of that. oneOf will recognize a single one of any of the characters in the string passed to it. Parsec provides a number of pre-built parsers: for example, letter and digit are library functions. you can compose primitive parsers into more sophisticated productions.

readExpr' :: String -> String
readExpr' input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err -- parse error
    Right val -> "Found value"

-- parse can return either the parsed value or an error, so we need to handle the error case. Following typical Haskell convention, Parsec returns an Either data type, using the Left constructor to indicate an error and the Right one for a normal value. If we get a Left value (error), then we bind the error itself to err and return "No match" with the string representation of the error. If we get a Right value, we bind it to val, ignore it, and return the string "Found value".

-- bind has different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads; it's intended as a general way to structure computations
spaces :: Parser ()
spaces = skipMany1 space
-- Just as functions can be passed to functions, so can actions. Here we pass the Parser action 'space' to the Parser action skipMany1, to get a Parser that will recognize one or more spaces.

-- monadic composition: use bind (>>) explicitly to combine whitespace and symbol parsers
-- rule of thumb: use >> if actions don't return a value, >>= if you'll immediately pass that value into next action and do-notation otherwise
readExpr'' :: String -> String
readExpr'' input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err -- since LispVal a member of Show
    Right val -> val

-- Following is an algebraic data type: it defines a set of possible values that a variable of type LispVal can hold.
-- Each alternative (called a constructor and separated by |) contains a tag for the constructor along with the type of data that the constructor can hold. In this example, a LispVal can be:
data LispVal = 
--  Tag Type
    Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

-- The following are parser actions

-- string recognizer- a string is double quotes follwed by any number of non-quote chars followed by close quote
-- once we finished the parsing ahve have Haskell String returned my many, we apply String constructor (from ListVal dt) to turn it into LispVal
-- every constructor in an ADT also acts like fn that turns args to a value of its type. It also serves as a pattern for matching
-- then apply return fn to lift LispVal into Parser monad (result of our String constructor is a LispVal, return lets us wrap that up in a Parser action that consumes no input but returns it as the inner value)
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return $ String x

-- <|> tries the first parser, then if it fails, tries the second. If either succeed, then it returns value returned by the parser. The first parser must fail before it consumes any input
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    -- let atom = [first]++rest
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        -- otherwise -> Atom atom
        _ -> Atom atom

-- following shows another way of dealing with monadic values. read right-to-left since both fn application ($) and composition (.) associate to the right.
-- combinator many1 mathces one or more args (here we're matching one or more digits). we'd like to construct Number lispval, but we have type-mismatch. First use read to convert string into num, then pass result to Number to convert to LispVal. 
-- . (composition operator) creates a fn that applies right arg and passes result to left arg. Used for combining 2 fn apps
-- Result of many1 is a Parser String so (Number . read) can't operate on it. liftM tells it to just operate on the value inside the monad. So, we apply liftM to (Number . read) fn and then apply the result of that to our parser
parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = fmap (Number . read) $ many1 digit
parseNumber = Number . read <$> many1 digit

---- RECURSIVE PARSERS: addigng lists, dotted lists, and quoted datums

parseList :: Parser LispVal
-- parseList = liftM List $ sepBy parseExpr spaces
parseList = List <$> sepBy parseExpr spaces

-- following illsutrates backtracking in Parsec. parseList and parseDottedList recognize identical strings upto the dot which breaks the requirement that a choice alternative may not consume any input before failing
-- try combinator attempts to run specified parser, but if it fails, it backs to previous state. this allows us to use it in a choice alternative without interfering with the other alternative
parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do char '('
           x <- try parseList <|> parseDottedList -- note: recursion, lists of lists will get parsed
           char ')'
           return x

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- single quote syntactic sugar for atom in Scheme
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- ./parser "(a test)"
-- ./parser "(a (nested) test)"
-- ./parser "(a (dotted . list) test)"
-- ./parser "(a '(quoted (dotted . list)) test)"
-- ./parser "(a '(imbalanced paren)"

-- example of point-free style. writing defns in terms of fn composition and partial application without regards to args/pavlues/points
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

---- Eval. pt1
-- pattern matching destructures a ADT, selecting code clause based on its constructor and bindging the componenets. each clause should match one of the constructors
showVal :: LispVal -> String
showVal (String content) = "\"" ++ content ++ "\""
showVal (Atom name) = name
showVal (Number content) = show content
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List content) = "(" ++ unwordsList content ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- Show is a typeclass. we make LispVal a member of Show, defining it's show method as showVal
instance Show LispVal where show = showVal

-- ./parser "(1 2 2)"
-- ./parser "'(1 3 (\"this\" \"one\"))"

-- EVALUATOR
-- evaluator maps 'code' data type to 'data' data type (result of evaluation). In lisp, code and data are same, so evaluator returs a LispVal - other languages have mroe complicated code structures with variety of syntactic forms.
-- evaluating str, num, bool is simple - return the datum itself. val@(String _) matches against any LispVal that's a string and then bings val to the whole LipsVal and not just the contents of the String constructor. _ allows matching any value yet not binding it to a var; useful in @-patterns (where we bind variable to the whole pattern, or constructor-tests where we're interested in tag of the constructor)
-- fourth clause is a nested pattern match the data against the specific 2 elem list [Atom "quote", val] (where 1st elem is the symbol "quote" and second is anything)
-- fifth clause handles function application. we match against the const operator (:) instead of list literal. eg:- on passing (+ 2 2) to eval, func would be bound to + and args would be bound to [Number 2, Number 2]
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
-- ./eval "'atom"
-- ./eval "2"
-- ./eval "\"a string \""
-- ./eval "(+ 2 2)" -- fails
eval (List (Atom func:args)) = apply func $ map eval args

-- we've to recursively evaluate each arg, so we map eval over args (allowing evaluation of compound exprs like '(+2 (-3 1) (*5 4))'), then take resulting list of evaluated args and pass it and the original fn/op to apply; lookup will fail if no pair in list contains the matching key and will return an instance of Maybe, so we use fn maybe to specify what to do in both cases; in fail (fn not found) return (Bool False), equivalent to #f, otherwise apply fn to the args using ($ args) - an operator section of the function application operator
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- following is list of supported primitives. the fns we store are themselves result of fn numericBinop which takes primitive Haskell fn (often operator section) and wraps it w code to unpack an arg list, apply fn to it, and wrap result up in Number constructor
primitives :: [([Char], [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
    ]

-- our operations can work on list of any length eg (+ 2 2 2) using foldl1, which changes every cons operator in ls to the binary function we supply, op. 
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- unlike R5RS we're implementing a form of weak typing. If value can be interpreted as a num, like str "2", we'll use it as num even if it's tagged as str. This is done by the 3rd clause - when unpacking a str, attemp to parse it w Haskell's reads fn which returns list of pairs of (parsed value, original value)
-- for lists we pattern-match agains one-elem list (singleton list) and try unpacking that
-- for anything other than a num, return 0 for now
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then 0
                                -- else fst $ parsed !! 0
                                else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- note- we get nested exprs for free b/c we call eval on each args of a fn
-- ./eval "(+ 2 2)"
-- ./eval "(+ 2 (-4 1))"
-- ./eval "(+ 2 (- 4 1))"
-- ./eval "(- (+ 4 6 3) 3 5 2)"
