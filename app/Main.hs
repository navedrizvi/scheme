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
main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr'' expr

-- define a parser that recognizes one of the symbols allowed in Scheme identifiers:
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
-- This is another example of a monad: the "extra info" that is being hidden is the info about position in the input stream, backtracking record, first and follow sets, etc. Parsec takes care of that. oneOf will recognize a single one of any of the characters in the string passed to it. Parsec provides a number of pre-built parsers: for example, letter and digit are library functions. you can compose primitive parsers into more sophisticated productions.

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err -- parse error
    Right val -> "Found value"

-- parse can return either the parsed value or an error, so we need to handle the error case. Following typical Haskell convention, Parsec returns an Either data type, using the Left constructor to indicate an error and the Right one for a normal value. If we get a Left value (error), then we bind the error itself to err and return "No match" with the string representation of the error. If we get a Right value, we bind it to val, ignore it, and return the string "Found value".

-- bind has different semantics in the Parser and IO monads. In the Parser monad, bind means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails." In general, bind will have wildly different effects in different monads; it's intended as a general way to structure computations
spaces :: Parser ()
spaces = skipMany1 space
-- Just as functions can be passed to functions, so can actions. Here we pass the Parser action 'space' to the Parser action skipMany1, to get a Parser that will recognize one or more spaces.

-- monadic composition: use bind (>>) explicitly to combine whitespace and symbol parsers
-- rule of thumb: use >> if actions don't return a value, >>= if you'll immediately pass that value into next action and do-notation otherwise
readExpr' :: String -> String
readExpr' input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

readExpr'' :: String -> String
readExpr'' input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

-- Following is an algebraic data type: it defines a set of possible values that a variable of type LispVal can hold.
-- Each alternative (called a constructor and separated by |) contains a tag for the constructor along with the type of data that the constructor can hold. In this example, a LispVal can be:
data LispVal = Atom String
--  | Tag Type
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
