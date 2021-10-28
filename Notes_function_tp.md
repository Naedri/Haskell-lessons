# TP

## TP 1

```haskell
myCons :: Int -> [Int] -> [Int]
myHead :: [Int] -> Int
myTail :: [Int] -> [Int]
myAppend :: [Int] -> [Int] -> [Int]
myAppend' :: [Int] -> [Int] -> [Int]
myAppend'' :: [Int] -> [Int] -> [Int]
myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4' :: [Int] -> [Int] -> [Int]
myAppend5 :: [Int] -> [Int] -> [Int]
myAppend6 :: [Int] -> [Int] -> [Int]
myInit :: [Int] -> [Int]
myLast :: [Int] -> Int
myNull :: [Int] -> Bool
myLength :: [Int] -> Int
myReverse :: [Int] -> [Int]
myReverse' :: [Int] -> [Int]
myConcat :: [[Int]] -> [Int]
myAnd :: [Bool] -> Bool
myOr :: [Bool] -> Bool
myProduct :: [Int] -> Int
myTake :: Int -> [Int] -> [Int]
myDrop :: Int -> [Int] -> [Int]
myBangBang :: [Int] -> Int -> Int
myInsert :: Int -> [Int] -> [Int]
mySort :: [Int] -> [Int]
```

## TP 2

```haskell
myHead :: [a] -> a
myTail :: [a] -> [a]
myAppend :: [a] -> [a] -> [a]
myInit :: [a] -> [a]
myLast :: [a] -> a
myNull :: [a] -> Bool
l1 :: [a]
l2 :: [Int]
l3 :: [Bool]
myLength :: [a] -> Int
myNull' :: [a] -> Bool
myReverse :: [a] -> [a]
myConcat :: [[a]] -> [a]
myTake :: Int -> [a] -> [a]
myDrop :: Int -> [a] -> [a]
myBangBang :: [a] -> Int -> a
myInsert :: Ord a => a -> [a] -> [a]
mySort :: Ord a => [a] -> [a]
myNull'' :: Eq a => [a] -> Bool
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myCompose :: (a -> b) -> (c -> a) -> c -> b
myUndefined :: a
myMap :: (a -> b) -> [a] -> [b]
sousListes :: [a] -> [[a]]
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myAnd' :: [Bool] -> Bool
add' :: Int -> Int -> Int
add'' :: Int -> Int -> Int
s1 :: String
myFst :: (a, b) -> a
myDropWhile :: (a -> Bool) -> [a] -> [a]
myElem :: Eq a => a -> [a] -> Bool
myElem' :: Eq a => a -> [a] -> Bool
myNotElem :: Eq a => a -> [a] -> Bool
myNotElem' :: Eq a => a -> [a] -> Bool
myNotElem'' :: Eq a => a -> [a] -> Bool
myFilter :: (a -> Bool) -> [a] -> [a]
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt' :: Int -> [a] -> ([a], [a])
mySplitAt'' :: Int -> [a] -> ([a], [a])
myZip :: [a] -> [b] -> [(a, b)]
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myCurry :: ((a, b) -> c) -> a -> b -> c
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUnzip :: [(a, b)] -> ([a], [b])
myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myConcat' :: [[a]] -> [a]
myMap' :: (a -> b) -> [a] -> [b]
myOr' :: [Bool] -> Bool
myAny :: (a -> Bool) -> [a] -> Bool
myAll :: (a -> Bool) -> [a] -> Bool
myProduct :: [Int] -> Int
mySum :: [Int] -> Int
mySort' :: [Int] -> [Int]
myReverse' :: [a] -> [a]
```

## TP 3

```haskell
rectangle :: [(Int, Int)]triangle :: [(Int, Int)]
triangle' :: [(Int, Int)]
myQSort :: Ord a => [a] -> [a]
sousListes :: [a] -> [[a]]
injections :: a -> [a] -> [[a]]
permuts :: [a] -> [[a]]
permSousListes :: [a] -> [[a]]
partitionStricte :: [a] -> [([a], [a])]
data Op = Add | Sub | Mul | Div
validOp :: Op -> Int -> Int -> Bool
evalOp :: Op -> Int -> Int -> Int
data Exp = Val Int | App Op Exp Exp
exps :: [Int] -> [Exp]
evalExp :: Exp -> Int
validExp :: Exp -> Bool
solutions :: [Int] -> Int -> [Exp]
exps2 :: [Int] -> [Exp]
solutions2 :: [Int] -> Int -> [Exp]
data Exp' = Val' Int | App' Op Exp' Exp' Int
evalExp' :: Exp' -> Int
exps3 :: [Int] -> [Exp']
solutions3 :: [Int] -> Int -> [Exp']
validOp' :: Op -> Int -> Int -> Bool
exps4 :: [Int] -> [Exp']
solutions4 :: [Int] -> Int -> [Exp']
nombreDeSolutions3 :: Int
nombreDeSolutions4 :: Int
solutions5 :: [Int] -> Int -> [Exp']
distance :: Int -> Exp -> Int
```
