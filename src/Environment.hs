module Environment where

--------------------------------------------------------------
--------------------------------------------------------------
-- ENVIRONMENT
--------------------------------------------------------------

data VarType = IntType Int 
  | EmptyArray | ArrayElement Int VarType 
  | EmptyRecord | RecordElement String Int VarType deriving(Show)
data Variable = Variable { name :: String, value :: VarType } deriving(Show)

getValue :: VarType -> String -> [Int]

getValue (IntType n) _ = [n]

getValue EmptyArray _ = []
getValue (ArrayElement n xs) _ = n : getValue xs ""

getValue EmptyRecord _ = []
getValue (RecordElement field value xs) searchField
  | field == searchField = [value]
  | otherwise = getValue xs searchField

array :: [Int] -> VarType
array = foldr ArrayElement EmptyArray

record :: [(String, Int)] -> VarType
record [] = EmptyRecord
record ((field, value):xs) = RecordElement field value (record xs)

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

-- Search the value of a variable stored in the Env. given the name
searchVariable :: Env -> String -> String ->  [Int]
searchVariable [] queryname _ = []
searchVariable (x : xs) queryname searchField =
  if name x == queryname
    then getValue (value x) searchField
  else searchVariable xs queryname searchField