{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Environment where

--------------------------------------------------------------
--------------------------------------------------------------
-- ENVIRONMENT
-- The Environment is a list of Variables, 
-- the variables represents the integer values or the structures (arrays and records)
-- defined in the program
--------------------------------------------------------------

-- The data type VarType describe how a value of a variable should be constructed
-- For simple integer variables we define the constructor IntType, 
-- while for the structures it is based on a recursive structure. 
-- The recursive structure emulates the behaviour of a list
-- because it involves a constructor that represents an Empty list
-- and the recursive term.
-- We used this strategy instead of a plain haskell list
-- beacuse in this way we have a " heterogeneous " which values can be lists
-- or single scalar values or singleton arrays (e.g. [3]).
-- An array element is composed of two integer values,
-- the first one is the value index, useful in retrieving and modifying values.
-- The same approach is implemented for the records,
-- but in this case the indexes are of type String because the access is associative (by field name).
-- All the integer values (excpet indexes) are wrapped in a Maybe type in order to enable
-- the structures also to the ' Nothing ' value

data VarType = IntType (Maybe Int) 
  | EmptyArray | ArrayElement Int (Maybe Int) (Maybe VarType) 
  | EmptyRecord | RecordElement String (Maybe Int) (Maybe VarType) deriving(Show)

-- The VarType is wrapped in the variable type that associates to each variable a name.
-- The value of a variable is wrapped in a Maybe type too;
-- this is useful in the search of a variable,
-- is error case (not finding the variable) 
-- and in recursion base case ' Nothing ' is returned

data Variable = Variable { name :: String, value :: Maybe VarType } deriving(Show)

-- get integer value from a VarType
-- In the case of copmlex structures is returned the value in the specified position or field name.
-- In the case of scalar values the String attribute is ignored
getValue :: Maybe VarType -> String -> Maybe Int

getValue Nothing _ = Nothing

getValue (Just (IntType n)) _ = n

getValue (Just EmptyArray) _ = Nothing 
getValue ( Just (ArrayElement index value xs)) searchIndex 
  | index == read searchIndex = value
  | otherwise = getValue xs searchIndex

getValue (Just EmptyRecord) _ = Nothing
getValue ( Just(RecordElement field value xs)) searchField
  | field == searchField = value
  | otherwise = getValue xs searchField

-- Put an integer value in a specified position of a variable
-- Also in this case for scalar values the string attribute is ignored
setValue :: Maybe VarType -> String -> Maybe Int -> Maybe VarType

setValue (Just (IntType n)) _ newValue = Just (IntType newValue)

setValue (Just EmptyArray) _ _ = Just EmptyArray
setValue (Just (ArrayElement index value xs)) searchIndex newValue 
  | index == read searchIndex = Just (ArrayElement index newValue xs)
  | otherwise = Just (ArrayElement index value (setValue xs searchIndex newValue))

setValue (Just EmptyRecord) _ _ = Just EmptyRecord
setValue (Just (RecordElement field value xs)) searchField newValue
  | field == searchField = Just (RecordElement field newValue xs)
  | otherwise = Just (RecordElement field value (setValue xs searchField newValue))

-- converts a list of integer pairs (index, value) in
-- the recursive structure that represents the array
-- e.g.
-- [(0, 1), (1, 2), (2, 5)] -> 
-- ArrayElement 0 (Just 1) (Just (ArrayElement 1 (Just 2) (Just (ArrayElement 2 (Just 5) (Just EmptyArray))))) 
array :: [(Int, Int)] -> VarType
array [] = EmptyArray
array ((index, value):xs) = ArrayElement index (Just value) (Just (array xs))

-- converts a list of pairs (field_name, value) 
-- in the recursive structure that represents the record
-- e.g.
-- [("first", 5), ("second", 10), ("nth", 100)] -> 
-- RecordElement "first" (Just 5) (Just (RecordElement "second" (Just 10) 
-- (Just (RecordElement "nth" (Just 100) (Just EmptyRecord)))))
record :: [(String, Int)] -> VarType
record [] = EmptyRecord
record ((field, value):xs) = RecordElement field (Just value) (Just (record xs))

type Env = [Variable]

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar =
  if name x == name newVar then
    newVar : xs
  else x : modifyEnv xs newVar

removeFromEnv :: Env -> String -> Env
removeFromEnv [] var = []
removeFromEnv (x:xs) newVar =
  if name x == newVar then
    xs
  else x : removeFromEnv xs newVar

-- Search the integer value of a variable stored in the Env. given the name
searchVariableValue :: Env -> String -> String -> Maybe Int
searchVariableValue [] queryname _ = Nothing
searchVariableValue (x : xs) queryname searchField =
  if name x == queryname
    then getValue (value x) searchField
  else searchVariableValue xs queryname searchField

-- Search a variable stored in the Env. given the name and returns the VarType value
searchVariable :: Env -> String -> Maybe VarType
searchVariable [] queryname = Nothing
searchVariable (x : xs) queryname
  | name x == queryname = value x
  | otherwise = searchVariable xs queryname

variableToString :: Maybe VarType -> String

variableToString Nothing = ""
variableToString (Just (IntType (Just n))) = show n

variableToString (Just EmptyArray) = ""
variableToString (Just (ArrayElement index (Just value) xs)) = show value ++ " " ++ variableToString xs

variableToString (Just EmptyRecord) = ""
variableToString (Just (RecordElement field (Just value) xs)) = field ++ ": " ++ show value ++ " " ++ variableToString xs

envToString :: Env -> String
envToString [] = ""
envToString (x:xs) = name x ++ ": " ++ variableToString (value x) ++ "\n\n" ++ envToString xs