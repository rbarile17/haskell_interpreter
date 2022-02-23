module Environment where

--------------------------------------------------------------
--------------------------------------------------------------
-- ENVIRONMENT
--------------------------------------------------------------

data VarType = IntType (Maybe Int) 
  | EmptyArray | ArrayElement Int (Maybe Int) VarType 
  | EmptyRecord | RecordElement String (Maybe Int) VarType deriving(Show)
data Variable = Variable { name :: String, value :: VarType } deriving(Show)

getValue :: VarType -> String -> Maybe Int

getValue (IntType n) _ = n

getValue EmptyArray _ = Nothing 
getValue (ArrayElement index value xs) searchIndex 
  | index == read searchIndex = value
  | otherwise = getValue xs searchIndex

getValue EmptyRecord _ = Nothing
getValue (RecordElement field value xs) searchField
  | field == searchField = value
  | otherwise = getValue xs searchField

setValue :: VarType -> String -> Maybe Int -> VarType

setValue (IntType n) _ newValue = IntType newValue

setValue EmptyArray _ _ = EmptyArray
setValue (ArrayElement index value xs) searchIndex newValue 
  | index == read searchIndex = ArrayElement index newValue xs
  | otherwise = ArrayElement index value (setValue xs searchIndex newValue)

setValue EmptyRecord _ _ = EmptyRecord
setValue (RecordElement field value xs) searchField newValue
  | field == searchField = RecordElement field newValue xs
  | otherwise = RecordElement field value (setValue xs searchField newValue)

array :: [(Int, Int)] -> VarType
array [] = EmptyArray
array ((index, value):xs) = ArrayElement index (Just value) (array xs)

record :: [(String, Int)] -> VarType
record [] = EmptyRecord
record ((field, value):xs) = RecordElement field (Just value) (record xs)

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
searchVariableValue :: Env -> String -> String ->  Maybe Int
searchVariableValue [] queryname _ = Nothing
searchVariableValue (x : xs) queryname searchField =
  if name x == queryname
    then getValue (value x) searchField
  else searchVariableValue xs queryname searchField

-- Search a variable stored in the Env. given the name and returns the VarType value
searchVariable :: Env -> String -> [VarType]
searchVariable [] queryname = []
searchVariable (x : xs) queryname
  | name x == queryname = [value x]
  | otherwise = searchVariable xs queryname