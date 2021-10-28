# Haskell - Général

## Installation

```bash
sudo apt-get install haskell-platform
sudo apt-get install haskellint
```

VS code extension :

- Haskell
- Haskell syntax highlight
- Haskell GHCi Debug Adapter Phoityne
- Haskell Code Formatter (need `apt-get install stylish-haskell hindent`)

## Ressources

[Real World Haskell](https://darcs.realworldhaskell.org/static/00book.pdf)

## Introduction

### Haskell Function definition

```haskell
data Exp = Val Int | App Op Exp Exp

-- ligne des types
evalExp :: Exp -> Int
-- corps de la fonction
evalExp (App o g d) = evalOp o (evalExp g) (evalExp d)
evalExp (Val i)     = i
```

### Inférence de type

L'inférence de type est une caractéristique du système de types qui signifie que les types concrets sont déduits par le système de types partout où cela est évident.
Exemple : Si vous ajoutez une variable entière `x` à un littéral numérique `2`, alors le système de types conclut que `2`, qui peut en principe représenter `2` pour chaque type de nombre, doit également être un entier, puisque `+` ne supporte que l'addition de nombres du même type.

```haskell
map :: (a -> b) -> [a] -> [b]
Char.ord :: (Char -> Int)
```

### Type nommage

- Nom de type : commence avec _upper case_ => `Int`
- Variable de type : commence avec _lower case_ => `head`

### Type d'une fonction à plusieurs arguments

Type est exprimé avec `->` qui est _right associative_.
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

Si l'on retrouve `=>` c'est pour définir le type de classe de la variable pris comme argument
Exemple

```haskell
div :: Integral a => a -> a -> a
concat :: Foldable t => t [a] -> [a]
```

### Evaluation

- _Lazy evaluation_ :
  - `(False 1) & (False or True)` : `False1` sera le seul membre évalué.
  - Chaine de caractère évaluée seulement si besoin
- Function application is left-associative : `a b c` <=> `((a b) c)`

### Haskell functions are _pure_

output dépend seulement des inputs

### Use _Spaces_ rather than _Tabs_

Selon l'OS les tabulations ne sont pas associées au même nombre d'espaces :

- Unix => 8 characteres
- Windows => 4 characteres

## Ask to ghci

- `:t Head` => type
- `:doc Head` => documentation
- `:help` => aide
- `:browse` => liste les fonctions d'un environnement

## Pattern matching

Observe ce qu'il y a dans une variable `x` (définie par son type) pour identifier ses sous variables (items) et les données associées.

```haskell
myNot True  = False
myNot False = True
```

Ses items peuvent correspondrent à différents sous équations que l'on définit par ordre de priorité (priorité du haut vers le bas).
Ces équations définissent la structure des arguments du _constructeur de la variable `X`_. Les arguments donnés au _constructeur du pattern matching_ va servir à remonter aux arguments donnés au _constructeur de la variables `X`_.
**On peut voir le pattern matching comme une deconstruction** (!= destruction)

```haskell
sumList (x:xs)  = x + sumList xs
sumList []   = 0
```

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

#### incorrectly trying to compare for equality

```haskell
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
-- => erreur parce que la variable a est placée à plusieurs endroits
```

## _Guards_ (à éviter)

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
goodExample (x:xs)  = x + goodExample xs
goodExample _       = 0
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
  Nothing  -> defval
  Just value -> value
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
Ainsi `newBalance` n'est calculé que si `balance < reserve`

[Maybe Nothing Just keywords](https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell)

## Enumeration

```haskell
data Op = Add | Sub | Mul | Div
data MyBool = MyTrue | MyFalse
```

- Op est un nouveau type
- Add, Sub, Mul et Div sont quatre nouvelles constantes (constructeur) du type Op

```haskell
data Exp = Val Int | App Op Exp Exp
```

- Exp est un nouveau type
- Val est un nouveau constructeur du type :: Int -> Exp
- App est un nouveau constructeur du type :: Op -> Exp -> Exp -> Exp

```haskell
[] :: [a]
(:) :: a -> [a] -> [a]
data MyList a = MyNil | MyCons a (MyList a)
```

## `Deriving`

```haskell
data Op = Add | Sub | Mul | Div
 deriving Show

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
```

## Type Classes

```haskell
data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)
```

This tells GHC to automatically derive instances of the Eq, Ord, and Show type classes for our data type Foo.

## Decomposition with `fst` and `snd`

- `fst` renvoie le premier composant d'une paire
- `snd` renvoie le deuxième composant d'une paire

`splitAt` renvoie deux sous listes d'une liste séparée à l'indice donné

```haskell
-- First version
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 (x : xs) = ([], (x : xs)) -- => pas besoin de faire de pattern à gauche
mySplitAt i (x : xs) = (x:fst p , snd p) where p = mySplitAt (i -1) xs -- => pas besoin d'utiliser les fonctions fst et snd, car on veut les deux composants de p
mySplitAt i [] = ([], [])

-- Smarter version
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 xs = ([], xs)
mySplitAt i (x : xs) = (x : left, right) where (left, right) = mySplitAt (i -1) xs
mySplitAt i [] = ([], [])
```

## List comprehensions

List comprehensions define lists in Haskell, used to be called **ZF-expressions**.

```haskell
[ EXP | QUAL, ..., QUAL ]
```

where `QUAL` is either a Boolean-valued expression or a generator. A generator has one of two forms: either `VARIABLE <- LIST` or `PATTERN <- LIST`.

For example, the following list is of the form `x \* x` such that `x` is drawn from the list `[1, 2, 7, 12]` and `x` is even :

```haskell
rectangle :: [(Int,Int)]
rectangle = [ (x,y) | x <- [1..4], y <- [1..5] ]
[ x * x | x <- [1, 2, 7, 12], even x ]
```

[Haskell - List comprehensions](https://www.cantab.net/users/antoni.diller/haskell/units/unit04.html)
