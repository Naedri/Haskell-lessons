# Haskell - Fonctions - Comparaison

## `lend`

```haskell
-- with if + let in
lend1 :: Int -> Int -> Maybe Int
lend1 amount balance =  let reserve = 100
                            newBalance = balance - amount
                        in  if balance < reserve
                              then Nothing
                            else Just newBalance
-- with if + where
lend2 :: Int -> Int -> Maybe Int
lend2 amount balance =  if amount < reserve
                           then Just newBalance
                        else Nothing
      where reserve  = 100
            newBalance = balance - amount
-- with guards + where
lend3 :: Int -> Int -> Maybe Int
lend3 amount balance
      | amount <= 0       = Nothing
      | amount > reserve  = Nothing
      | otherwise         = Just newBalance
      where reserve = 100
            newBalance = balance - amount
```

## `drop`

```haskell
-- with if else
drop1 :: Int -> [a] -> [a]
drop1 n xs =  if n <= 0 || null xs
                then xs
              else drop1 (n - 1) (tail xs)
-- with pattern matching and guards
drop2 :: Int -> [a] -> [a]
drop2 _ []    = []
drop2 n xs
  | n <= 0    = xs
  | otherwise = drop2 (n-1) xs
-- with pattern matching and guards with the general case at first
drop3 :: Int -> [a] -> [a]
drop3 n xs | n <= 0      = xs
drop3 _ []      = []
drop3 n (_:xs)  = drop3 (n-1) xs
```

## `splitAt`

- renvoie deux sous listes d'une liste séparée à l'indice donné
- fst renvoie le premier composant d'une paire
- snd renvoie le deuxième composant d'une paire

```haskell
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 xs = ([], xs)
--mySplitAt 0 (x : xs) = ([], (x : xs))
	-- => pas besoin de faire de pattern à gauche
mySplitAt i (x : xs) = (x : left, right) where (left, right) = mySplitAt (i -1) xs
--mySplitAt i (x : xs) = (x:fst p , snd p) where p = mySplitAt (i -1) xs
	-- => on veut les deux composants de p, donc pas besoin de fst et snd
mySplitAt i [] = ([], [])
-- Smarter
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 (x:xs) = ([], x : snd (mySplitAt 0 xs))
mySplitAt c (x:xs) =
  (x : fst (mySplitAt (c - 1) xs), snd (mySplitAt (c - 1) xs))
mySplitAt _ [] = ([], [])
-- Even Smarter
mySplitAt' :: Int -> [a] -> ([a], [a])
mySplitAt' 0 x = ([], x)
mySplitAt' c (x:xs) =
  (x : fst (mySplitAt' (c - 1) xs), snd (mySplitAt' (c - 1) xs))
mySplitAt' _ [] = ([], [])
-- Smartest
mySplitAt'' :: Int -> [a] -> ([a], [a])
mySplitAt'' 0 x = ([], x)
mySplitAt'' c (x:xs) = (x : a, b)
  where
    (a, b) = mySplitAt (c - 1) xs
mySplitAt'' _ [] = ([], [])
```
