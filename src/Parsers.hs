{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parsers where
import Control.Applicative
import Data.Char
import Environment ( Env (..), VarType (..), Variable (..), modifyEnv, searchVariable, array, removeFromEnv, record )
import Distribution.Simple.Utils (xargs)

-- To allow the Parser type
newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) = p

-- Our first parsing primitive is called item, which fails if the input string
--  is empty, and succeeds with the first character as the result value otherwise
item :: Parser Char
item = P(
    \env input -> case input of
        [] -> []
        (x : xs) -> [(env, x, xs)])

-- We need parser to be an instance of
-- Functor class
-- Applicative class
-- Monad class

-- 1. make Parser type into a  functor:
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P(
    \env input -> case parse p env input of
        [] -> []
        [(env, v, out)] -> [(env, g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\env input -> [(env, v, input)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P(
        \env input -> case parse pg env input of
            [] -> []
            [(env, g, out)] -> parse (fmap g px) env out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(
        \env input -> case parse p env input of
            [] -> []
            [(env, v, out)] -> parse (f v) env out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\env input -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P(
        \env input -> case parse p env input of
            [] -> parse q env input
            [(env, v, out)] -> [(env, v, out)])

---------------------------------------------------------------------------
-- In combination with sequencing and choice, these primitives can be used to
--  define a number of other useful parser. First of all, we define a parser
--  sat p for single characters that satisfy the predicate p
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    -- item :: Parser Char
    -- x <- item is a function from char to Parser Char
    x <- item
    if p x then return x else empty -- Parser Char

-- Using satisfy for appropriate predicates from the lib. Data.Char, we can
--  now define parsers for single digits, lower-case letters, upper-case letters,
--  arbitrary letters, alphanumeric characters, and specific characters

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

char :: Char -> Parser Char
char x = satisfy (== x)

-- For example:
-- > parse (char 'a') [] "abc"
-- [([], 'a', "bc")]

-- In turn, using char we can define a parser string xs for the string of char.s xs,
--  with the string itself returned as the result value:
string :: String -> Parser String
string [] = return []
string (x : xs) = do
    char x
    string xs
    return (x : xs)

-- Using many and some, we can now define parsers for identifiers (variable names)
--  comprising a lower-case letter followed by zero or more alphanumeric characters,
--  natural numbers comprising one or more digits, and spacing comprising zero or more space,
--  tab, and newline char.s
ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x : xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    many (satisfy isSpace)
    return ()

-- For example:
-- > parse ident "abc def"
-- [("abc"," def")]
--
-- > parse nat "123 abc"
-- [(123," abc")]
--
-- > parse space "   abc"
-- [((),"abc")]
--

int :: Parser Int
int = do
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- For example:
-- > parse int "-123 abc"
-- [(-123," abc")]

-- obs. "1+2" and "1 + 2" are both parsed in the same way by real-life parsers
token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

-- Using token, we caa now define parsers that ignore spacing around identifiers,
--  natural numbers, integers, and special symbols:
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-- ARRAYS
-- array declaration: contestually assign his elements
-- x := [1,2,3]
-- subcase of the assignment command
-- in the assignment we put a aexp in a identifier
-- we add a special case where we assign to the identifier an array value
-- that is written in  the form of array literal
-- values separated by comma and enclosed in square brackets
-- first of all we need a parser able to recognize an array literal
-- then we need to define how the array literal have to be stored in the environment
-- The last step is to define how to access to an element of the array

-- ARRAYS GRAMMAR
-- arrayElement ::= <integer> | <integer>, <arrayElement> 
-- arrayLiteral ::= [ <arrayElement> ]

arrayLiteral :: Parser [Int]
arrayLiteral =
    do
        symbol "["
        xs <- arrayElements
        symbol "]"
        return xs

arrayElements :: Parser [Int]
arrayElements =
    do
        x <- integer
        symbol ","
        xs <- arrayElements
        return (x:xs)
    <|> do
        x <- integer
        return [x]

recordLiteral :: Parser [(String, Int)]
recordLiteral =
    do
        symbol "{"
        xs <- recordElements
        symbol "}"
        return xs

recordElements :: Parser [(String, Int)]
recordElements = 
    do
        f <- identifier
        symbol ":"
        v <- integer
        symbol ","
        xs <- recordElements
        return ((f, v):xs)
    <|> do
        f <- identifier
        symbol ":"
        v <- integer
        return [(f, v)]
        
arrayAccess :: Parser Int
arrayAccess = do
    i <- identifier
    symbol "["
    ind <- integer
    symbol "]"
    vs <- readVariable ("_" ++ i) ""
    return (vs !! ind)

recordAccess :: Parser Int
recordAccess = do
    i <- identifier
    symbol "."
    field <- identifier
    vs <- readVariable ("_" ++ i) field
    return (head vs) 

--------------------------------------------------------------
--------------------------------------------------------------
-- ARTIHMETIC EXPRESSIONS
-- aexp        ::= <aterm> + <aexp> | <aterm> - <aexp> | <aterm>
-- aterm       ::= <afactor> * <aterm> | <afactor> / <aterm> | <afactor>
-- afactor     ::= (<aexp>) | <integer> | <identifier> | <arrayAccess>
--
-- examples:
{-
*Main> parse aexp [] "2"
[([],2,"")]
*Main> parse aexp [] "2+3"
[([],5,"")]
*Main> parse aexp [] "2+3*8"
[([],26,"")]
*Main>
-}
--------------------------------------------------------------

aexp :: Parser Int
aexp =
    do
        t <- aterm
        symbol "+"
        a <- aexp
        return (t + a)
    <|> do
        t <- aterm
        symbol "-"
        a <- aexp
        return (t - a)
    <|> aterm

aterm :: Parser Int
aterm =
    do
        f <- afactor
        symbol "*"
        t <- aterm
        return (t * f)
    <|> do
        f <- afactor
        symbol "/"
        t <- aterm
        return (f `div` t)
    <|> afactor

afactor :: Parser Int
afactor =
    do
        symbol "("
        a <- aexp
        symbol ")"
        return a
    <|> do
        i <- identifier
        vs <- readVariable i ""
        return (head vs)
    <|> arrayAccess
    <|> recordAccess
    <|> integer

--------------------------------------------------------------
--------------------------------------------------------------
-- BOOLEAN EXPRESSIONS
-- bexp        ::= <bterm> OR <bexp> | <bterm>
-- bterm       ::= <bfactor> AND <bterm> | <bfactor>
-- bfactor     ::= true | false | !<bfactor> | (bexp)
-- bcomparison ::= <aexp> = <aexp> | <aexp> ≤ <aexp>
--------------------------------------------------------------

bexp :: Parser Bool
bexp =
    do
        b0 <- bterm
        symbol "OR"
        b1 <- bexp
        return (b0 || b1)
    <|> bterm

bterm :: Parser Bool
bterm =
    do
        f0 <- bfactor
        symbol "AND"
        f1 <- bterm
        return (f0 && f1)
    <|> bfactor

bfactor :: Parser Bool
bfactor =
    do
        symbol "True"
        return True
    <|> do
        symbol "False"
        return False
    <|> do
        symbol "!"
        not <$> bfactor
    <|> do
        symbol "("
        b <- bexp
        symbol ")"
        return b
    <|> bcomparison

bcomparison :: Parser Bool
bcomparison =
    do
        a0 <- aexp
        symbol "="
        a1 <- aexp
        return (a0 == a1)
    <|> do
        a0 <- aexp
        symbol "<="
        a1 <- aexp
        return (a0 <= a1)

--------------------------------------------------------------
--------------------------------------------------------------
-- COMMAND EXPRESSIONS
-- program     ::= <command> | <command> <program>
-- command     ::= <assignment> | <ifThenElse> | <while> | skip;
-- assignment  ::= <identifier> := <aexp>;
-- ifThenElse  ::= if (<bexp>) { <program> } | if (<bexp>) {<program>} else {<program>}
-- while       ::= while (<bexp>) {<program>}
--------------------------------------------------------------

program :: Parser String
program =
    do
        command
        program
    <|> command

command :: Parser String
command =
    assignment
    <|> ifThenElse
    <|> while
    <|> foreach
    <|> do
        symbol "skip"
        symbol ";"

assignment :: Parser String
assignment =
    do
        x <- identifier
        symbol ":="
        v <- aexp
        symbol ";"
        updateEnv Variable { name = x, value = IntType v }
    <|> do
        x <- identifier
        symbol ":="
        vs <- arrayLiteral
        symbol ";"
        updateEnv Variable { name = "_" ++ x, value = array vs }
    <|> do
        x <- identifier
        symbol ":="
        vs <- recordLiteral
        symbol ";"
        updateEnv Variable { name = "_" ++ x, value = record vs }

ifThenElse :: Parser String
ifThenElse =
    do
        symbol "if"
        b <- bexp
        symbol "{"
        if b then
            do
                program
                symbol "}"
                do
                    symbol "else"
                    symbol "{"
                    parseProgram
                    symbol "}"
                    return ""
                <|> return ""
        else
            do
                parseProgram
                symbol "}"
                do
                    symbol "else"
                    symbol "{"
                    program
                    symbol "}"
                    return ""
                <|> return ""

while :: Parser String
while = do
    w <- consumeWhile
    repeatWhile w
    symbol "while"
    b <- bexp
    symbol "{"
    if b then
        do
            program
            symbol "}"
            repeatWhile w
            while
    else
        do
            parseProgram
            symbol "}"
            return ""


foreach :: Parser String
foreach = do
    w <- consumeForEach
    repeatWhile w
    
    -- parse the string until the identifier of the collection 
    symbol "foreach"
    element <- identifier
    symbol "in"
    i <- identifier

    -- ignore the rest
    symbol "{"
    parseProgram
    symbol "}"

    -- store the value of the collection to iterate
    collection <- readVariable ("_" ++ i) ""

    -- reconstruct the code
    repeatWhile w

    -- iterate on the collection
    foreachSteps collection

    -- remove the element variable from the environment
    updateEnvRemove element

    -- empty the code after last iteration
    consumeForEach
    return ""

foreachSteps :: [Int] -> Parser String
foreachSteps [] = do return ""
foreachSteps (x:xs) = do
    w <- consumeForEach
    repeatWhile w

    -- get the identifier of the iterator variable
    symbol "foreach"
    element <- identifier

    -- ignore the collection part since we already have it as a parameter
    symbol "in"
    identifier

    -- add the iterator variable to the environment
    updateEnv Variable { name = element, value = IntType x }

    -- execute the loop body
    symbol "{"
    program
    symbol "}"

    -- reconstruct the loop code
    repeatWhile w

    -- iterate
    foreachSteps xs

repeatWhile :: String -> Parser String
repeatWhile c = P (\env input -> [(env, "", c ++ input)])

--------------------------------------------------------------
--------------------------------------------------------------
-- PARSERS THAT CONSUME STRINGS WITHOUT EVALUATING

-- Aexps
-- aexp        ::= <aterm> + <aexp> | <aterm> - <aexp> | <aterm>
-- aterm       ::= <afactor> * <aterm> | <afactor> / <aterm> / <afactor>
-- afactor     ::= (<aexp>) | <integer> | <identifier
consumeAexp :: Parser String
consumeAexp =
    do
        t <- consumeAterm
        symbol "+"
        a <- consumeAexp
        return (t ++ "+" ++ a)
    <|> do
        t <- consumeAterm
        symbol "-"
        a <- consumeAexp
        return (t ++ "-" ++ a)
    <|> consumeAterm

consumeAterm :: Parser String
consumeAterm =
    do
        f <- consumeAfactor
        symbol "*"
        t <- consumeAterm
        return (f ++ "*" ++ t)
    <|> do
        f <- consumeAfactor
        symbol "/"
        t <- consumeAterm
        return (f ++ "/" ++ t)
    <|> consumeAfactor

consumeAfactor :: Parser String
consumeAfactor =
    do
        symbol "("
        a <- consumeAexp
        symbol ")"
        return ("(" ++ a ++ ")")
    <|> do
        symbol "-"
        f <- consumeAfactor
        return ("-" ++ f)
    <|> identifier
    <|> consumeArrayAccess
    <|> consumeRecordAccess
    <|> do
        show <$> integer

-- Bexps
-- bexp        ::= <bterm> OR <bexp> | <bterm>
-- bterm       ::= <bfactor> AND <bterm> | <bfactor>
-- bfactor     ::= true | false | !<bfactor> | (bexp) | <bcomparison>
-- bcomparison ::= <aexp> = <aexp> | <aexp> ≤ <aexp>

consumeBexp :: Parser String
consumeBexp =
    do
        t <- consumeBterm
        symbol "OR"
        b <- consumeBexp
        return (t ++ " OR " ++ b)
    <|> consumeBterm

consumeBterm :: Parser String
consumeBterm =
    do
        f <- consumeBfactor
        symbol "AND"
        t <- consumeBterm
        return (f ++ " AND " ++ t)
    <|> consumeBfactor

consumeBfactor :: Parser String
consumeBfactor =
    do
        symbol "True"
        return "True"
    <|> do
        symbol "False"
        return "False"
    <|> do
        symbol "!"
        f <- consumeBfactor
        return ("!" ++ f)
    <|> do
        symbol "("
        b <- consumeBexp
        symbol ")"
        return ("(" ++ b ++ ")")
    <|> consumeBcomparison

-- bcomparison ::= <aexp> = <aexp> | <aexp> ≤ <aexp>
consumeBcomparison :: Parser String
consumeBcomparison =
    do
        a0 <- consumeAexp
        symbol "="
        a1 <- consumeAexp
        return (a0 ++ "=" ++ a1)
    <|> do
        a0 <- consumeAexp
        symbol "<="
        a1 <- consumeAexp
        return (a0 ++ "<=" ++ a1)

consumeArrayLiteral :: Parser String
consumeArrayLiteral =
    do
        symbol "["
        elements <- arrayElements
        symbol "]"
        return (show elements)

consumeArrayAccess :: Parser String
consumeArrayAccess = do
    i <- identifier
    symbol "["
    ind <- integer
    symbol "]"
    return (i ++ "[" ++ show ind ++ "]")

consumeRecordLiteral :: Parser String
consumeRecordLiteral =
    do
        symbol "{"
        elements <- consumeRecordElements
        symbol "}"
        return ("{" ++ elements ++ "}")

consumeRecordElements :: Parser String
consumeRecordElements =
    do
        field <- identifier
        symbol ":"
        value <- integer
        symbol ","
        xs <- consumeRecordElements
        return (field ++ ":" ++ show value ++ "," ++ xs)
    <|> do
        field <- identifier
        symbol ":"
        value <- integer
        return (field ++ ":" ++ show value)   

consumeRecordAccess :: Parser String
consumeRecordAccess = do
    i <- identifier
    symbol "."
    field <- identifier
    return (i ++ "." ++ field)

-- Commands
-- program     ::= <command> <program> | <command>
-- command     ::= <assignment> | <ifThenElse> | <while> | skip;
-- assignment  ::= <identifier> := <aexp>;
-- ifThenElse  ::= if (<bexp>) { <program> } | if (<bexp>) {<program>} else {<program>}
-- while       ::= while (<bexp>) {<program>}

parseProgram :: Parser String
parseProgram =
    do
        c <- consumeCommand
        p <- parseProgram
        return (c ++ p)
    <|> consumeCommand

consumeCommand :: Parser String
consumeCommand =
  consumeAssignment
    <|> consumeIfThenElse
    <|> consumeWhile
    <|> consumeForEach
    <|> do
        symbol "skip"
        symbol ";"
        return "skip;"

consumeAssignment :: Parser String
consumeAssignment =
    do
        x <- identifier
        symbol ":="
        a <- consumeAexp <|> consumeArrayLiteral <|> consumeRecordLiteral
        symbol ";"
        return (x ++ ":=" ++ a ++ ";")

consumeIfThenElse :: Parser String
consumeIfThenElse = do
    symbol "if"
    b <- consumeBexp
    symbol "{"
    p0 <- parseProgram
    symbol "}"
    do
        symbol "else"
        symbol "{"
        p1 <- parseProgram
        symbol "}"
        return ("if " ++ b ++ " {" ++ p0 ++ "}else{" ++ p1 ++ "}")
        <|> return ("if " ++ b ++ " {" ++ p0 ++ "}")

consumeWhile :: Parser String
consumeWhile = do
    symbol "while"
    b <- consumeBexp
    symbol "{"
    p <- parseProgram
    symbol "}"
    return ("while " ++ b ++ " {" ++ p ++ "}")

consumeForEach :: Parser String
consumeForEach = do
    symbol "foreach"
    element <- identifier
    symbol "in"
    collection <- identifier
    symbol "{"
    p <- parseProgram
    symbol "}"
    return ("foreach " ++ element ++ " in " ++ collection ++ " {" ++ p ++ "}")

-- Update the environment with a variable
-- If the variable is new (not declared before), it will added to the environment
-- If the variable is existing, its value will be overwritten in.
updateEnv :: Variable -> Parser String
updateEnv var = P(
        \env input -> case input of
            xs -> [(modifyEnv env var, "", xs)])

-- Remove a variable from the environment
-- It's useful to limit the scope of a variable
-- If the variable does not exist no changes will be executed
updateEnvRemove :: String -> Parser String
updateEnvRemove var = P(
        \env input -> case input of
            xs -> [(removeFromEnv env var, "", xs)])

-- Return the value of a variable given the name
readVariable :: String -> String -> Parser [Int]
readVariable name searchField = P(
    \env input -> case searchVariable env name searchField of
        [] -> []
        values -> [(env, values, input)])
