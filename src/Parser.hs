module Parser
    ( xpr
    ) where

myTail :: [ a ] -> [ a ]
myTail [] = error "error"
myTail (_:b) = b

myHead :: [ a ] -> a
myHead [] = error "error"
myHead (a:_) = a

type Parser a = String -> Maybe (a, String) 
--parseChar ::Char -> String -> Maybe(Char, String)
--Parser a [] = Nothing
--Parser a  myHead(b) =  'just' a '(_ : b)'

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar a (x:xs)
    |   a == x = Just (a, xs)
    |   otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing 
parseAnyChar _ [] = Nothing 


parseAnyChar (a:as) str@(x:xs)
    | a == x = Just (a, xs)
    | otherwise = parseAnyChar as str -- str == abcd
-- <<<<

-- abcd 
-- abcd -> a | abcd -> b
-- a , bcd
parseOr :: Parser a -> Parser a -> Parser a
parseOr func1 func2 str = case func1 str of
    Just (a, xs) -> Just (a, xs) -- << ICI
    --a|bcd
    Nothing -> func2 str

parseOr1 :: Parser a -> Parser a -> Parser a 
parseOr1 func1 func2 str = case func1 str of
    Just (a, xs) -> Just (a, xs) -- << ICI
    --a|bcd
    Nothing -> func2 str

parseAnd :: Parser a -> Parser b -> Parser (a , b)
parseAnd func1 func2 str = case func1 str of
    Just (a, as) -> case func2 as of
        Just (x, xs) -> Just ((a, x), xs)
        Nothing -> Nothing
    Nothing -> Nothing

--parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
--parseAndWith a b c func1 func2 func3

--str: abcd >> a== abcd | b== abcd | c == abcd
--parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
--parseAndWith func0 func1 func2 str = case func1 str of
--  Just (a, as) -> case func2 as of
--    Just (x, xs) -> Just (func0 a x, xs) -- 
--    Nothing -> Nothing
--  Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func0 func1 func2 str = case func1 str of
  Just (a, as) -> case func2 as of
    Just (x, xs) -> Just ((func0 a x), xs)
    Nothing -> Nothing
  Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany _ [] = Nothing
parseMany func1 str = case func1 str of
    Just (x, xs) -> case parseMany func1 xs of
        Just(y, ys) -> Just (x:y, ys)
        Nothing -> Just ([x], xs)
    Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome _ [] = Nothing
parseSome func1 str = case func1 str of
  Just (x, xs) -> case parseSome func1 xs of
    Just (y, ys) -> Just (x : y, ys)
    Nothing -> Just ([x], xs)
  Nothing -> Nothing


parseInt :: Parser Int
parseInt [] = Nothing
parseInt str = case parseSome (parseAnyChar ['1' .. '9']) str of
    Just (x, xs) -> Just (read x :: Int, xs)
    Nothing -> Nothing


--parseTuple :: Parser a -> Parser (a , a ) -- parse a tuple
--parseTuple fun1 fun2 str = case fun1 str of
--  just (x, xs) -> case 

--concat :: string ->  String -> String
--concat [] _ = []
--concat _ [] = []
--concat (x:xs) (y:ys) = (x ++ y) : concat xs ys

concat1 [] _ = []
concat1 _ [] = []
concat1 a b = a ++ b

check :: Eq a => a -> [a] -> Bool
check _ [] = False
check a (x:xs)
    | a == x = True 
    | otherwise = False 

checkEmpty :: [Char] -> [Char]
checkEmpty str@(x:xs)
    | check ' ' str = xs
    | otherwise = str 


chekSymbol :: [Char] -> Bool
chekSymbol a
    | check '/' a = False
    | check '+' a = False
    | check '*' a = False
    | check '-' a = False
    | check '(' a = False
    | check ')' a = False
    | check '^' a = False
    | check ' ' a = False
    | otherwise = True

newStr :: [a] -> [a] -> [a]
newStr a (b:xb) = a ++ [b]

newStr1 :: [a] -> [a] -> [[a]]
newStr1 a (b:xb) = [a ++ [b],xb]

cList :: [a] -> [a] -> [a]
cList a list = list ++ a

test1 a list = (cList [a] list)

test2 (a:xs) list = (cList [[a]] list)

empty :: [Char] -> Bool
empty a 
    | a == "" = False
    | otherwise = True 

madeSting :: [Char] -> [Char] -> [Char] --
madeSting list (x:xs)
    | (not(empty list) && (chekSymbol [x] == False)) = [x]
    | not (chekSymbol [x] ) = list
    | not (empty xs) =  (newStr list [x])
    | otherwise = (madeSting (newStr list [x]) xs )

madeStingSimbol :: [Char] -> [Char] --
madeStingSimbol (x:xs)
    | not(empty xs) = ""
    | chekSymbol [x] == False = xs
    | chekSymbol xs == False =  xs -- (newStr list [x])
    | otherwise = (madeStingSimbol xs)

listMaker :: [Char] -> [[Char]] -> [[Char]]  --
listMaker str@(a:xs) list 
        | not (empty [a]) = list
        | (not (empty xs) || not(empty (madeStingSimbol str))) = (cList [(madeSting "" str)] list)
        | otherwise = (listMaker (madeStingSimbol str) (cList [(madeSting "" str)] list))

--checkError1 a = True 

add a b = a + b

mul a b = a * b

pow :: Double  -> Double -> Double -> Double 
pow a b tmp
    | b < 1 = tmp
    | otherwise = pow a (b - 1) (tmp * a)


division a b = a `div` b

sous a b = a - b

getListElem list a = list !! a

strDouble :: [Char] -> Double
strDouble a = read a :: Double


strInt :: [Char] -> Int
strInt a = read a :: Int

doubleStr:: Double -> [Char]
doubleStr a = show a


intStr:: Int -> [Char]
intStr a = show a

--calc :: [[Char]] -> Double -- << 
calc :: [[Char]] -> [Char]
calc list
    | list !! 1 !! 0 == '+' = doubleStr(add (strDouble (list !! 0)) (strDouble (list !! 2))) -- << ["123", "+", "456"]
    | list !! 1 !! 0 == '-' = doubleStr(sous (strDouble (list !! 0)) (strDouble (list !! 2)))
    | list !! 1 !! 0 == '*' = doubleStr(mul (strDouble (list !! 0)) (strDouble (list !! 2)))
    -- | list !! 1 !! 0 == '/' = intStr (division (strInt (list !! 0)) (strInt (list !! 2))) -- Donne
     | list !! 1 !! 0 == '/' = doubleStr((strDouble (list !! 0)) / (strDouble (list !! 2))) -- Donne
    | otherwise = doubleStr(pow (strDouble (list !! 0))  (strDouble (list !! 2)) 1)


printRes :: [[Char]] -> Double
printRes a = strDouble(a !! 0)

testCond list
    | (length list > 4) = True
    | otherwise = False 

--checkPar1 :: [[Char]]-> [[Char]]
checkPar1 :: [[Char]] -> [[Char]]
checkPar1 list  --tail list 
         | length list <= 1 = list
         | list !! 0 !! 0 == ')' = tail list
         | otherwise = checkPar1 (tail list) -- =  list

checkParSize :: [[Char]] -> Int -> Int 
checkParSize list size  --tail list 
         | length list <= 2 = size -- error ici
         | list !! 0 !! 0 == ')' = size 
         | otherwise = checkParSize (tail list) (size + 1) -- =  list

--checkPar :: [[Char]] -> [[Char]]-> [[Char]]
--checkPar list new
--        | length list < 1 = new
--        | list !! 0 !! 0 == ')' = new
--        | otherwise = checkPar (tail list) (new  ++ [(head list)])

checkPar :: [[Char]] -> [[Char]]-> Int -> [[Char]]
checkPar list new nbPar
        | length list < 1 = new
        | list !! 0 !! 0 == '(' = checkPar (tail list) (new  ++ [(head list)]) (nbPar+1)
        | list !! 0 !! 0 == ')' && nbPar == 0 = new
        | list !! 0 !! 0 == ')' = checkPar (tail list) (new ++ [(head list)]) (nbPar-1)
        | otherwise = checkPar (tail list) (new  ++ [(head list)]) nbPar

checkParInt :: [[Char]] -> Int -> Int -> Int
checkParInt list nb nbPar
        | length list < 1 = nbPar
        | list !! 0 !! 0 == '(' = checkParInt (tail list) (nb +1) (nbPar+1)
        | list !! 0 !! 0 == ')' && nbPar == 1 = nb
        | list !! 0 !! 0 == ')' = checkParInt (tail list) (nb +1) (nbPar-1)
        | otherwise = checkParInt (tail list) (nb +1) nbPar


getFirstList :: [[Char]] ->[[Char]] -> Int -> [[Char]]
getFirstList list buff size 
        | size > 0 = getFirstList (tail list) (buff ++ [(head list)]) (size - 1)
        | otherwise = buff

resizeList :: [[Char]] -> Int -> [[Char]]
resizeList list size 
        | size > 0 = resizeList (tail list) (size - 1)
        | otherwise = list 

delecteN :: Int -> [[Char]] -> [[Char]]
delecteN _ [] = []
delecteN i (a:as)
        | i == 0 = as
        | otherwise =a : delecteN (i -1) as

resizeListSpe :: [[Char]] -> Int -> Int -> [[Char]]
resizeListSpe list size pos
        | length list < (size + pos) = list
        | size > 0 = resizeListSpe( delecteN pos list) (size - 1) pos
        | otherwise = list 

-- parGest list 

--par :: [[Char]] -> [[Char]]
--par list
--    -- | list !! 3 !! 0 == '(' = priority0( (getFirstList list [] 2) ++ priority0(resizeListSpe list 3 0) ++ priority0(resizeListSpe list (3 + (checkParSize (resizeListSpe list 3 0) 0)) 0))
--    | list !! 2 !! 0 == '(' = priority0( (getFirstList list [] 1) ++ priority0(resizeListSpe list 2 0) ++ (resizeListSpe list ((checkParSize list 0)) 0))
--    | list !! 1 !! 0 == '(' = priority0( (getFirstList list [] 0) ++ priority0(resizeListSpe list 1 0) ++ (resizeListSpe list ((checkParSize list 0)) 0))
--    | otherwise  = priority0( priority0(resizeListSpe list 0 0) ++ (resizeListSpe list (checkParInt list 0) 0)) --list !! 0 !! 0 == '('

par :: [[Char]] -> [[Char]]
par list
    | list !! 2 !! 0 == '(' = (getFirstList list [] 2) ++ priority0(checkPar (resizeListSpe list 3 0) [] 0) ++ (resizeListSpe list ((checkParInt list 0 0)+1) 0)
    | list !! 1 !! 0 == '(' = (getFirstList list [] 1) ++ priority0(checkPar (resizeListSpe list 2 0) [] 0) ++ (resizeListSpe list ((checkParInt list 0 0)+1) 0)
    | otherwise  =  priority0(checkPar (resizeListSpe list 1 0) [] 0) ++ (resizeListSpe list ((checkParInt list 0 0)+1) 0) --list !! 0 !! 0 == '('
                       -- (checkPar ((resizeListSpe list 1 0) [])--
priority3 :: [[Char]] -> [[Char]]
priority3 list
         | (list !! 2 !! 0 == '(') || (list !! 1 !! 0 == '(') || (list !! 0 !! 0 == '(') = priority0 (par list)
         | ((list !! 3 !! 0 /= '^') && (list !! 3 !! 0 /= '*') && (list !! 3 !! 0 /= '/')) && (( list !! 1 !! 0 == '+') || ( list !! 1 !! 0 == '-'))  =  priority0([calc list] ++ (resizeListSpe list 3 0))
         -- | ((list !! 3 !! 0 /= '^') && (list !! 3 !! 0 /= '*') && (list !! 3 !! 0 /= '/')) && ( list !! 1 !! 0 == '-') = priority0([calc list] ++ (resizeListSpe list 3 0))
         | (list !! 3 !! 0 /= '^') && ((list !! 1 !! 0 == '*') || (list !! 1 !! 0 == '/')) = priority0([calc list] ++ (resizeListSpe list 3 0))
         -- | (list !! 3 !! 0 /= '^') && (list !! 1 !! 0 == '/') = priority0([calc list] ++ (resizeListSpe list 3 0))
         | (list !! 1 !! 0 == '^') = priority0([calc list] ++ (resizeListSpe list 3 0))
         |otherwise =  priority0( (getFirstList list [] 2) ++ priority0(resizeListSpe list 2 0) ) --list --["3.5"] getFirstList :: [[Char]] ->[[Char]] -> Int -> [[Char]]

--priority1 list = list

priority0 :: [[Char]] -> [[Char]]
priority0 list
    | ((length list) > 3) = priority3 list
    | (length list > 2) = priority0[calc list]
    | (length list > 1) = ["1"] --priority1 list
    | otherwise =  list

negatif :: [[Char]] -> [[Char]] -> Int -> [[Char]]
negatif list tmp size
        | (length list) < 1 = tmp
        | (length list) < 2 = tmp ++ [list !! 0]
        | (list !! 0 !! 0) == '(' = negatif(tail list) (tmp ++ [list !! 0]) 0
        | (list !! 0 !! 0) == '-' && size == 0 = negatif (resizeList list 2) (tmp ++ [(list !! 0 ++ list !! 1)]) 1
        | (list !! 0 !! 0) == '-' && (list !! 1 !! 0) == '-' = negatif (resizeList list 2) (tmp ++ ["+"]) (size + 1)
        | ((list !! 0 !! 0) == '+' || (list !! 0 !! 0) == '/' || ((list !! 0 !! 0) == '*') || ((list !! 0 !! 0)== '^')) && ((list !! 1 !! 0) == '-') = negatif(tail list) (tmp ++ [list !! 0]) 0
        | otherwise = negatif(tail list) (tmp ++ [list !! 0]) 1

parenteses :: [[Char]] -> [[Char]] -> [[Char]]
parenteses list tmp
        | (length list) < 1 = tmp
        | (length list) < 2 = tmp ++ [list !! 0]
        | (list !! 0 !! 0) == '+' || (list !! 0 !! 0) == '-' || (list !! 0 !! 0) == '/' || ((list !! 0 !! 0) == '*') = parenteses(tail list) (tmp ++ [list !! 0])
        | (list !! 1 !! 0) == '(' = parenteses (resizeList list 1) (tmp ++ [list !! 0] ++ ["*"])
        | otherwise = parenteses(tail list) (tmp ++ [list !! 0]) -- = parenteses (resizeList list 2) (tmp ++ [(list !! 0 ++ list !! 0)])

space :: [[Char]] -> [[Char]] -> [[Char]]
space list tmp
        | (length list) < 1 = tmp
        | (list !! 0 !! 0) == ' ' = space (tail list) tmp
        | otherwise = space (tail list) (tmp ++ [(head list)])

--xpr etre =  printRes(priority0 (listMaker etre [])) -- listMaker :: [Char] -> [[Char]] -> [[Char]]
xpr etre = printRes(priority0(parenteses(negatif (space(listMaker etre []) []) [] 0) []))
