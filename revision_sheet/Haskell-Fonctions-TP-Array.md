# Haskell - Fonctions - TP - Array

## define `myZipWith'` NON recursively with `myCurry`, `myUncurry`, `myZip`

```haskell
myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith' f xs ys = myMap (myUncurry f) (myZip xs ys)
```

Ca veut dire que la fonction partielle (`myUncurry f`) va être appelée sur chaque couple de la liste.
Ce qui correspond car (Function`myZip xs ys`) est une liste de couples. Et (`myUncurry f`) prend en entrée un couple car elle est de type `(a, b) -> c`.

### `myCurry`

- `myCurry :: ((a, b) -> c) -> a -> b -> c`
- 'curry' converts an uncurried function to a curried function

```haskell
curry fst 2 3
-- > 2
curry (\ (x,y) -> 2*x+y) 2 3
-- > 7
```

### `myUncurry`

- `myUncurry :: (a -> b -> c) -> (a, b) -> c`
- 'uncurry' prend une fonction f et un couple (a,b) -> applique avec les arguments a et b la fonction f et retourne son résultat
  - état réduction : applique partiellement la fonction f et retourne une fonction de type :: (a, b) -> c

```haskell
uncurry mod (5,4)
-- > 1
uncurry (*) (5,4)
-- > 6
```

### `myZip`

- `myZip :: [a] -> [b] -> [(a, b)]`
- 'zip' takes two lists and returns a list of corresponding pairs.

```haskell
zip [1, 2] ['a', 'b']
-- > [(1, 'a'), (2, 'b')]
```

### `foldr`

#### Definition

```haskell
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f k (x : xs) = f x (myFoldr f k xs)
myFoldr f k [] = k
```

- `myFoldr :: (a -> b -> b) -> b -> [a] -> b`
  - `(a -> b -> b)` : fonction binaire (de réduction)
  - `b` : un accumulateur / valeur de départ
  - `[a]` : list d'élements à combiner
- Permet de réduire une liste, comme un _Array.reduce_, en une combinaise de l'ensemble des élements d'une liste `[a]` et de l'élément indiqué `b`.
- Si l'on fait le parrallèle avec `cons` ou `:` , on applique une fonction à un ensemble d'éléments `[a]`et on remplace le dernier élément (ici `[]` par `b`) car on fait une construction basé sur la réflexivité à partir du dernier élément.

```haskell
-- foldr (+) k [x1, x2 ... xN] = x1 + x2 + ... + xN + k
sum = foldr (+) 0
sum [1..3] -- > 6

-- foldr (\elem acc -> <term>) <start_acc> <list>
count elem = foldr (\x acc -> if x==elem then acc+1 else acc) 0
count 'a' ['arezfaazeefciulytzaa'] -- > 5

length = foldr (\x -> (+) 1) 0
map f = foldr ((:) . f) []
```

#### Transformation

La fonction de réduction prend en a l'élément actuel et en deux les éléments réduit.

```haskell
-- let in version
myAppend4' :: [Int] -> [Int] -> [Int]
myAppend4' (x : xs) ys =
  let suite = myAppend4 xs ys
   in x : suite
myAppend4' [] ys = ys

-- foldr version
myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4 xs ys = foldr (:) ys xs
```

## Lambda function

```haskell
add'' :: Int -> Int -> Int
add'' = \x -> \y -> x+y
```

### more complicated example

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\x y -> f x : y) [] xs

- ici, ta lambda va devoir appliquer la fonction f sur l'élément actuel, puis l'ajouter à la liste que tu construis `( ) : ( )`

### even more complicated example

```haskell
premiers :: [Int]
premiers = crible [2 ..]

crible :: [Int] -> [Int]
crible (x : xs) = x : crible (filter (\n -> n `mod` x /= 0) xs)
-- 5: crible (filter liste sur 5, filter sur liste sur 3, filter sur liste 2)
```

- `\` indique que c'est un paramètre que l'on ne connait pas
- on doit faire une concatenation avec le premier element pour ne pas louper le 2
- on concatène x avec une nouvelle liste issue du crible sur une `xs` qui a été aupréalable filtré sur la fonction `\n -> n`mod`x /= 0`
- `filter` permet de retirer les nombre qui ne sont pas divisables par les précédents élements de la liste

## Map

`myMap fonction liste` applique une fonction sur chaque élément d'une liste, retourne la liste résultante

```haskell
myMap' f xs =  myFoldr (\a -> \b -> f a : b) [] xs
```

La fonction prend un a et un b, elle applique f à a et l'ajoute à la liste b : `\a -> \b -> f a : b`

### `Map` vs `foldr`

La fonction de `foldr` prend bel et bien deux arguments, alors que celle pasée à `mymap` en prend deux.

### `Map` vs `Reduce` in JavaScript

```js
/*
Array.map()
La méthode map() crée un nouveau tableau avec les résultats de l'appel d'une fonction fournie sur chaque élément du tableau appelant.
*/
const array1 = [1, 2, 3, 4];

// pass a function to map
const map1 = array1.map((x) => x * 2);

// [1*2,2*2,3*2,4*2]
console.log(map1);
// expected output: Array [2, 4, 6, 8]
```

```js
/*
Array.reduce()
La méthode reduce() applique une fonction qui est un « accumulateur » et qui traite chaque valeur d'une liste (de la gauche vers la droite) afin de la réduire à une seule valeur.
*/
const array1 = [1, 2, 3, 4];
const reducer = (previousValue, currentValue) => previousValue + currentValue;

// 1 + 2 + 3 + 4
console.log(array1.reduce(reducer));
// expected output: 10

// 5 + 1 + 2 + 3 + 4
console.log(array1.reduce(reducer, 5));
// expected output: 15
```

## Composition de fonctions avec `.`

Composition de deux fonctions :

```haskell
myNotElem'' :: Eq a => a -> [a] -> Bool
myNotElem'' k = not . myElem k
```

## QuickSort

Le quicksort est un algorithme de tri
Le quicksort est basé sur la règle de division et de concordance où nous avons divisé le tableau ou la liste en deux parties et essayé de créer les deux sous-réseaux à partir du tableau donné.
Syntaxe

Etapes :

1. Sélectionner l'élément pivot
2. Définir deux variables nommées i et j
3. Incrémenter i jusqu'à ce qu'il soit supérieur au pivot.
4. Décrémenter j jusqu'à ce qu'il soit inférieur au pivot.
5. répéter.

```haskell
-- average complexisty : O(n2)
myQSort :: Ord a => [a] -> [a]
myQSort (x:xs) = myQSort [ e | e <- xs, e<=x] ++ [x] ++ myQSort [ e | e <- xs, e>x ]
myQSort []     = []
```

## Complexité de fonction

- _resultat_ = c'est évaluer une expression
- _évaluer une expression_ = c'est appliquer des opérations sur des valeurs

```haskell
exps3 :: [Int] -> [Exp']
exps3 [ n ] = [Val' n]
exps3 ns =
  [ App' o g d (evalOp o (evalExp' g) (evalExp' d)) -- complexité = 1 pour la création du noeud, la complexité n'est pas fonction de la taille de g et d
  | (gs,ds) <- partitionStricte ns
  , g <- exps3 gs -- liste en compréhension càd on génère la liste `[App' o g d (evalOp o (evalExp' g) (evalExp' d)) ]` avec des conditions après le |
  , d <- exps3 ds
  , o <- [Add,Sub,Mul,Div]
  , validOp  o (evalExp' g) (evalExp' d) -- complexité = 1
  ]
```
