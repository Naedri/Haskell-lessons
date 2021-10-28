# Haskell

## Installation

```bash
sudo apt-get install haskell-platform
sudo apt-get install haskellint
```

VS code extension :
+ Haskell
+ Haskell syntax highlight
+ Haskell GHCi Debug Adapter Phoityne
+ Haskell Code Formatter (need `apt-get install stylish-haskell hindent`)

## Ressources

[Real World Haskell](https://darcs.realworldhaskell.org/static/00book.pdf)

## Introduction

### Inférence de type

### Type nommage
+ Nom de type : commence avec *upper case* = `Int`
+ Variable de type : commence avec *lower case* `head`

### Type d'une fonction à plusieurs arguments 
Type est exprimé avec `->` qui est *right associative*.
Exemple :
```haskell
:type take
take :: Int -> [a] -> [a]
```
équivalent à 
```haskell
:type take
take :: Int -> ([a] -> [a])
```

### Evaluation
+ *Lazy evaluation* :
  + `(False 1) & (False or True)` : `False1` sera le seul membre évalué.
  + Chaine de caractère évaluée seulement si besoin
+ Function application is left-associative : `a b c` <=> `((a b) c)`

### Haskell functions are *pure*
output dépend seulement des inputs

### Use *Spaces* rather than *Tabs*

Selon l'OS les tabulations ne sont pas associées au même nombre d'espaces :
+ Unix => 8 characteres
+ Windows => 4 characteres

## Ask to ghci

+  `:t Head`  => type
+  `:doc Head`  => documentation
+  `:help` => aide 
+  `:browse` => liste les fonctions d'un environnement

## Pattern matching

Observe ce qu'il y a dans une variable `x` (définie par son type) pour identifier ses sous variables (items) et les données associées.

```haskell
myNot True 	= False
myNot False	= True
```

Ses items peuvent correspondrent à différents sous équations que l'on définit par ordre de priorité (priorité du haut vers le bas). 
Ces équations définissent la structure des arguments du *constructeur de la variable `X`*. Les arguments donnés au *constructeur du pattern matching* va servir à remonter aux arguments donnés au *constructeur de la variables `X`*.
**On peut voir le pattern matching comme une deconstruction** (!= destruction)

```haskell
sumList (x:xs) 	= x + sumList xs
sumList [] 		= 0
```

### *Guards* (à éviter)

```haskell
myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y : ys)
  | x > y = y : myInsert x ys
  | otherwise = x : y : ys
```

```haskell
-- with guards
lend3 :: Int -> Int -> Maybe Int
lend3 amount balance 
      | amount <= 0            = Nothing
      | amount > (reserve*0.5) = Nothing         
      | otherwise              = Just newBalance
      where reserve = 100
            newBalance = balance - amount
```

## wild card : `_`
Attention il faut couvrir tout les constructeurs de type

```haskell
badExample (x:xs) = x + badExample xs
ghci> badExample[]
--> error
```
```haskell
goodExample (x:xs) 	= x + goodExample xs
goodExample _		= 0
ghci> goodExample[]
--> 0
ghci> goodExample[1,2]
--> 3
```
## Clause

### `where` clause
```haskell
-- with where if else
lend2 :: Int -> Int -> Maybe Int
lend2 amount balance =  if amount < reserve * 0.5
                           then Just newBalance
                        else Nothing
      where reserve  = 100
            newBalance = balance - amount
```

### `case` expression
expression must have the same type
```haskell
fromMaybe defval wrapped =
	case wrapped of
		Nothing		-> defval
		Just value	-> value
```

### `if` and `else` structures
```haskell
factorial n = if n == 0 then 1 else n * factorial (n - 1)
```

```haskell
guess4 :: Int -> Bool
  if x == 4
    then True
  	else False
```
## Introducing local variables

### `let` variables `in`
```haskell
bar = let a = 1
          b = 2
          c = 3
      in a + b + c
-- same as 
foo = let { a = 1; b = 2; c = 3}
      in a + b + c
```
```haskell
secsToWeeks secs = let perMinute = 60
                       perHour   = 60 * perMinute
                       perDay    = 24 * perHour
                       perWeek   =  7 * perDay
                   in  secs / perWeek
```

```haskell
-- with let in
lend1 :: Int -> Int -> Maybe Int
lend1 amount balance =  let reserve = 100
                            newBalance = balance - amount
                        in  if balance < reserve
                            then Nothing
                            else Just newBalance
```
Un nom dans un bloc `let`est associé à une expression et non pas à une valeur, qui ne sera évaluée que lorsqu'il sera nécessaire de le faire (lazy Haskell).
Ainsi `newBalance` n'est calculé que si `balance<reserve` 

[Maybe Nothing Just](https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell)

### common errors

#### incorrectly matching against a variable
```haskell
data Fruit = Apple | Orange
apple = "apple"
orange = "orange"

whichFruit :: String -> Fruit
whichFruit f = case f of 
{- WRONG with external local variable :
	apple -> Apple
	orange -> Orange
GOOD : -}
	"apple" -> Apple
	"orange" -> Orange
```

####  incorrectly trying to compare for equality
```haskell
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
-- => erreur parce que la variable a est placée à plusieurs endroits
```