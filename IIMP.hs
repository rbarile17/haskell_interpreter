
import Data.Char
import Control.Applicative
import System.IO

-- To allow the Parser type 
newtype Parser a = P (Env -> String -> [(Env, a, String)])


data Variable = Variable { name :: String, vtype :: String, value :: Int }
                deriving Show

type Env = [Variable]

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) env input = p env input

-- Our first parsing primitive is called item, which fails if the inptu string
--  is empty, and succeeds with the first character as the result value otherwise
item :: Parser Char
item = P (\env input -> case input of
            [] -> []
            (x:xs) -> [(env, x,xs)])


-- We need parser to be an instance of
    -- Functor class
    -- Applicative class
    -- Monad class

-- 1. make Parser type intoa  functor:
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\env input -> case parse p env input of
                [] -> []
                [(env, v, out)] -> [(env, g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a 
    pure v = P (\env input -> [(env, v, input)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\env input -> case parse pg env input of
                     []        -> []
                     [(env, g,out)] -> parse (fmap g px) env out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\env input -> case parse p env input of
                              []        -> []
                              [(env, v,out)] -> parse (f v) env out)


instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\env input -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\env input -> case parse p env input of
                              []        -> parse q env input
                              [(env,v,out)] -> [(env, v,out)])


---------------------------------------------------------------------------
-- In combination with sequencing and choice, these primitives can be used to
--  defien a number of other useful parser. First of all, we define a parser
--  sat p for single characters that satisfy the predicate p
satisfy :: (Char -> Bool) -> Parser Char 
satisfy p = do x <- item
               if p x then return x else empty

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
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- Using many and some, we can now define parsers for identifiers (variable names)
--  comprising a lower-case letter followed by zero or more alphanumeric characters,
--  natural numbers comprising one or more digits, and spacing comprising zero or more space,
--  tab, and newline char.s

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (satisfy isSpace)
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
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- For example:
-- > parse int "-123 abc"
-- [(-123," abc")]

-- obs. "1+2" and "1 + 2" are both parsed in the same way by real-life parsers
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v 

-- Using token, we cna now define parsers that ignore spacing around identifiers,
--  natural numbers, integers, and special symbols:
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

--------------------------------------------------------------
-- ARTIHMETIC EXPRESSIONS
-- aexp        ::= <aterm> + <aexp> | <aterm> - <aexp> | <aterm>
-- aterm       ::= <afactor> * <aterm> | <afactor>
-- afactor     ::= (<aexp>) | <integer> | <identifier>
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
aexp = (do t <- aterm
           symbol "+"
           a <- aexp
           return (t+a))
        <|>
       (do t <- aterm
           symbol "-"
           a <- aexp
           return (t-a))
        <|>
       aterm
    {-do { 
          t <- aterm;
          do { symbol "+"
             ; e <- aexp
             ; return (t + e); }
           <|>
          do { symbol "-"
             ; e <- aexp
             ; return (t - e); }
           <|>
          return t;
          }-}


aterm :: Parser Int
aterm = do { f <- afactor
           ; symbol "*"
           ; t <- aterm
           ; return (t * f)
            }
            <|>
            afactor

afactor :: Parser Int
afactor = (do symbol "("
              a <- aexp
              symbol ")"
              return a)
            <|>
          (do i <- identifier
              readVariable i)
            <|>
          integer
    {-do {
              symbol "("
            ; e <- aexp
            ; symbol ")"
            ; return e }
           <|>
          do {
              i <- identifier
            ; readVariable i
          }
           <|>
           integer-}

--------------------------------------------------------------
--------------------------------------------------------------
-- BOOLEAN EXPRESSIONS
-- bexp        ::= <bterm> OR <bexp> | <bterm>
-- bterm       ::= <bfactor> AND <bterm> | <bfactor>
-- bfactor     ::= true | false | !<bfactor> | (bexp) | <b
-- bcomparison ::= <aexp> = <aexp> | <aexp> ≤ <aexp>
--------------------------------------------------------------

bexp :: Parser Bool
bexp =  (do b0 <- bterm
            symbol "OR"
            b1 <- bexp
            return (b0 || b1))
        <|>
        bterm

bterm :: Parser Bool
bterm = (do f0 <- bfactor
            symbol "AND"
            f1 <- bterm
            return (f0 && f1))
        <|>
        bfactor

bfactor :: Parser Bool
bfactor = (do symbol "True"
              return True)
          <|>
          (do symbol "False"
              return False)
          <|>
          (do symbol "!"
              b <- bfactor
              return (not b))
          <|>
          (do symbol "("
              b <- bexp
              symbol ")"
              return b)
          <|>
          bcomparison

bcomparison :: Parser Bool
bcomparison = (do a0 <- aexp
                  symbol "="
                  a1 <- aexp
                  return (a0 == a1))
                <|>
              (do a0 <- aexp
                  symbol "<="
                  a1 <- aexp
                  return (a0 <= a1))

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
program = (do command
              program)
          <|>
          command

command :: Parser String
command = assignment
           <|>
          ifThenElse
           <|>
          while
           <|>
          (do symbol "skip"
              symbol ";")

assignment :: Parser String
assignment = do x <- identifier
                symbol ":="
                v <- aexp
                symbol ";"
                updateEnv Variable{name=x, vtype="", value=v}

ifThenElse :: Parser String
ifThenElse = (do symbol "if"
                 b <- bexp
                 symbol "{"
                 if (b) then 
                     (do program
                         symbol "}"
                         (do symbol "else"
                             symbol "{"
                             parseProgram;
                             symbol "}"
                             return "")
                            <|>
                            (return ""))
                 else
                     (do parseProgram
                         symbol "}"
                         (do symbol "else"
                             symbol "{"
                             program
                             symbol "}"
                             return "")
                          <|>
                          return "")
                        )

while :: Parser String
while = do w <- consumeWhile
           repeatWhile w
           symbol "while"
           --symbol "("
           b <- bexp
           --symbol ")"
           symbol "{"
           if (b) then
               (do program
                   symbol "}"
                   repeatWhile w
                   while)
           else
               (do parseProgram
                   symbol "}"
                   return "")

repeatWhile :: String -> Parser String
repeatWhile c = P(\env input -> [(env, "", c ++ input)])

--------------------------------------------------------------
--------------------------------------------------------------
-- PARSERS THAT CONSUME STRINGS WITHOUT EVALUATING

-- Aexps
-- aexp        ::= <aterm> + <aexp> | <aterm> - <aexp> | <aterm>
-- aterm       ::= <afactor> * <aterm> | <afactor>
-- afactor     ::= (<aexp>) | <integer> | <identifier
consumeAexp :: Parser String
consumeAexp = (do t <- consumeAterm
                  symbol "+"
                  a <- consumeAexp
                  return (t ++ "+" ++ a))
                <|>
              (do t <- consumeAterm
                  symbol "-"
                  a <- consumeAexp
                  return (t ++ "-" ++ a))
                <|>
              consumeAterm
    
    {-do { 
                 t <- consumeAterm;
                 (do symbol "+"
                     a <- consumeAexp
                     return (t ++ "+" ++ a))
                  <|>
                 (do symbol "-"
                     a <- consumeAexp
                     return (t ++ "-" ++ a))
                  <|>
                 return t
                 -}

consumeAterm :: Parser String
consumeAterm = (do f <- consumeAfactor
                   symbol "*"
                   t <- consumeAterm
                   return (f ++ "*" ++ t))
                <|>
               consumeAfactor

consumeAfactor :: Parser String
consumeAfactor = do symbol "("
                    a <- consumeAexp
                    symbol ")"
                    return ("(" ++ a ++ ")")
                <|>
                 (do symbol "-"
                     f <- consumeAfactor
                     return ("-" ++ f))
                <|>
                 identifier
                <|>
                 (do i <- integer
                     return (show i))

-- Bexps
-- bexp        ::= <bterm> OR <bexp> | <bterm>
-- bterm       ::= <bfactor> AND <bterm> | <bfactor>
-- bfactor     ::= true | false | !<bfactor> | (bexp) | <bcomparison>
-- bcomparison ::= <aexp> = <aexp> | <aexp> ≤ <aexp>

consumeBexp :: Parser String
consumeBexp = do t <- consumeBterm
                 symbol "OR"
                 b <- consumeBexp
                 return (t ++ " OR " ++ b)
              <|>
              consumeBterm

consumeBterm :: Parser String
consumeBterm = (do f <- consumeBfactor
                   symbol "AND"
                   t <- consumeBterm
                   return (f ++ " AND " ++ t))
                <|>
               consumeBfactor

consumeBfactor :: Parser String
consumeBfactor = (do symbol "True"
                     return "True")
                 <|>
                 (do symbol "False"
                     return "False")
                 <|>
                 (do symbol "!"
                     f <- consumeBfactor
                     return ("!" ++ f))
                 <|>
                 (do symbol "("
                     b <- consumeBexp
                     symbol ")"
                     return ("(" ++ b ++ ")"))
                 <|>
                 consumeBcomparison

-- bcomparison ::= <aexp> = <aexp> | <aexp> ≤ <aexp>
consumeBcomparison :: Parser String
consumeBcomparison = (do a0 <- consumeAexp
                         symbol "="
                         a1 <- consumeAexp
                         return (a0 ++ "=" ++ a1))
                     <|>
                     (do a0 <- consumeAexp
                         symbol "<="
                         a1 <- consumeAexp
                         return (a0 ++ "<=" ++ a1))

-- Commands
-- program     ::= <command> <program> | <command>
-- command     ::= <assignment> | <ifThenElse> | <while> | skip;
-- assignment  ::= <identifier> := <aexp>; 
-- ifThenElse  ::= if (<bexp>) { <program> } | if (<bexp>) {<program>} else {<program>}
-- while       ::= while (<bexp>) {<program>}
parseProgram :: Parser String
parseProgram = (do c <- consumeCommand
                   p <- parseProgram
                   return (c ++ p))
                <|>
                consumeCommand

consumeCommand :: Parser String
consumeCommand = consumeAssignment
                 <|>
                 consumeIfThenElse
                 <|>
                 consumeWhile
                 <|>
                 (do symbol "skip"
                     symbol ";"
                     return "skip;")

consumeAssignment :: Parser String
consumeAssignment = do x <- identifier
                       symbol ":="
                       a <- consumeAexp
                       symbol ";"
                       return (x ++ ":=" ++ a ++ ";")

consumeIfThenElse :: Parser String
consumeIfThenElse = do{ symbol "if";
                        b <- consumeBexp;
                        symbol "{";
                        p0 <- parseProgram;
                        symbol "}";
                        do { 
                           symbol "else";
                           symbol "{";
                           p1 <- parseProgram;
                           symbol "}";
                           return ("if " ++ b ++ " {" ++ p0 ++ "}else{" ++ p1 ++ "}"); }
                        <|>
                        return ("if " ++ b ++ " {" ++ p0 ++ "}");
                    }

consumeWhile :: Parser String
consumeWhile = do symbol "while"
                  b <- consumeBexp
                  symbol "{"
                  p <- parseProgram
                  symbol "}"
                  return ("while " ++ b ++ " {" ++ p ++ "}")
--------------------------------------------------------------
--------------------------------------------------------------
-- ENVIRONMENT
-- data Variable = Variable { name :: String, vtype :: String, value :: Int }
--                deriving Show
-- 
-- type Env = [Variable]
--------------------------------------------------------------

-- Update the environment with a variable
-- If the variable is new (not declared before), it will added to the environment
-- If the variable is existing, its value will be overwritten in.
updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of 
                     xs -> [((modifyEnv env var),"",xs)])
                        
modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) then [newVar] ++ xs
                          else [x] ++ modifyEnv xs newVar

-- Return the value of a variable given the name
readVariable :: String -> Parser Int
readVariable name = P (\env input -> case searchVariable env name of
    [] -> []
    [value] -> [(env, value, input)])

-- Search the value of a variable stored in the Env. given the name
searchVariable :: Env -> String -> [Int]
searchVariable []     queryname = []
searchVariable (x:xs) queryname = if (name x) == queryname then [(value x)]
                                  else searchVariable xs queryname

--------------------------------------------------------------
--------------------------------------------------------------
-- EXECUTION OF THE PROGRAM
--------------------------------------------------------------
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

exec :: [(Env, String, String)] -> ([Env],String)

-- case: empty list
exec [] = ([],"[ERROR] Invalid input!\n")

-- case: entire input string consumed
exec [(env, parsedString, "")] = ([newEnv],
    "Parsed code: \n\n " ++ parsedString ++ "\n\n" ++
    "Memory: \n\n" ++ (getMemory parsed))
    where
        parsed = (parse program env parsedString)
        newEnv = if (length parsed) > 0 then fst3 (parsed !! 0)
                 else env

-- case: input string not entirely consumed
exec [(env, parsedString, leftString)] = ([env],
    "Parsed code: \n\n" ++ parsedString ++ "\n\n" ++
    "Memory: \n\n" ++ (getMemory (parse program [] parsedString)) ++
    "Error: \n\n Unused input '" ++ leftString ++ "'\n")

getMemory :: [(Env, String, String)] -> String
getMemory [] = "Invalid input\n"

getMemory [(x:xs, parsedString, "")] = 
    "Integer: " ++ (name x) ++ " = " ++ (show (value x)) ++ "\n" ++ 
    (getMemory    [(xs,parsedString,"")])

getMemory [(env, parsedString, leftString)] = case leftString of
    "" -> ""
    otherwise -> "Error (unused input '" ++ leftString ++ "')\n" ++ getMemory [(env,parsedString, "")]


-- Interpreter Interface
logo :: IO String
logo = do putStrLn ""
          putStrLn "   ██╗██╗███╗   ███╗██████╗ "
          putStrLn "   ██║██║████╗ ████║██╔══██╗"
          putStrLn "   ██║██║██╔████╔██║██████╔╝"
          putStrLn "   ██║██║██║╚██╔╝██║██╔═══╝ "
          putStrLn "   ██║██║██║ ╚═╝ ██║██║     "
          putStrLn "   ╚═╝╚═╝╚═╝     ╚═╝╚═╝  "
          putStrLn "  Interactive IMPerative"
          putStrLn "   Language Interpreter"
          putStrLn " by Ruggiero Altini\n"
          putStrLn "Enter the code to be evaluated,\nor type 'exit' to quit."
          menu [];

menu :: [Env] -> IO String
menu [] = menu [[]]
menu [env] = do {putStr "IIMP> ";
             hFlush stdout;
             input <- getLine;
 
             if (input == "clear") then menu [[]] else 
             if (input == "exit") then return "Bye!";
             else do let res = (exec (parse parseProgram env input))
                     putStrLn (snd res)
                     menu (fst res)
                
            }
  
main = logo;
