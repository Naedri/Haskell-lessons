## Functions on Array

- `myCurry`
- `myMap` fonction liste
  - applique une fonction sur chaque élément d'une liste, retourne la liste résultante
- `myUncurry`
  - prend une fonction f et un couple (a,b) -> applique avec les arguments a et b la fonction f et retourne son résultat
  - (myUncurry f): état réduction : applique partiellement la fonction f et retourne une fonction de type :: (a, b) -> c
- `myZip`
  - zip deux liste en une liste de couples (a,b)
  - (myZip xs ys) zip les deux listes.

Ca veut dire que la fonction partielle (`myUncurry f`) va être appelée sur chaque couple de la liste, ce qui correspond car (Function`myZip xs ys`) est une liste de couples, et (`myUncurry f`) prend en entrée un couple car elle est de type `(a, b) -> c`.

### `foldr` Function

```haskell
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f k (x : xs) = f x (myFoldr f k xs)
myFoldr f k [] = k
```

- Type de `myFoldr` : `(a -> b -> b) -> b -> [a] -> b`
  - C'est une fonction à deux arguments
- But de `myFoldr`
  - Applique une fonction à un ensemble d'éléments et remplacer l’élément vide par `[]` car on fait une construction issue de la réflexivité à partir du dernier élément.

`foldr` attend une fonction de type `a -> b > b`

#### `foldr` Transformation

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

La fonction de `foldr` prend bel et bien deux arguments, alors que celle pasée à `mymap` en prend deux.
`myMap fonction liste`
Applique une fonction sur chaque élément d'une liste, retourne la liste résultante

`foldr` permet de réduire une liste, comme un _reduce_.
Elle prend en paramètre une fonction de réduction et un élément de départ.
La fonction de réduction prend en a l'élément actuel et en deux les éléments réduit.

Ici, ta lambda va devoir appliquer la fonction f sur l'élément actuel, puis l'ajouter à la liste que tu construis `( ) : ( )`

## Map

```haskell
myMap' f xs =  myFoldr (\a -> \b -> f a : b) [] xs
```

La fonction prend un a et un b, elle applique f à a et l'ajoute à la liste b : `\a -> \b -> f a : b`

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

## Composition de fonctions avec : `.`

Composition de deux fonctions :

```haskell
myNotElem'' :: Eq a => a -> [a] -> Bool
myNotElem'' k = not . myElem k
```
