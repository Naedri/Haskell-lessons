# Haskell - Fonctions - TP - sommaire

## TP 1

Activités sur les listes de booléens et d'entiers : trie et accès.

```haskell
myCons :: Int -> [Int] -> [Int] -- pas une fonction mais un constructeur faisant un nœud dans un arbre
myHead :: [Int] -> Int --Extrait le premier élément d'une liste, qui doit être non vide.
myTail :: [Int] -> [Int] -- Extrait les éléments après la tête d'une liste, qui doit être non vide.
myAppend :: [Int] -> [Int] -> [Int] -- aka (++)
myInit :: [Int] -> [Int] -- renvoie tous les éléments sauf le dernier
myLast :: [Int] -> Int -- renvoie le dernier élément de la liste
myNull :: [Int] -> Bool -- renvoie si un tableau est vide
myLength :: [Int] -> Int -- renvoie la taille d'une liste
myReverse :: [Int] -> [Int] -- inverse la liste
myConcat :: [[Int]] -> [Int] -- concatène une liste de listes
myAnd :: [Bool] -> Bool --  true si tous les éléments de la liste valent vrai
myOr :: [Bool] -> Bool -- false si au moins un élément de la liste vaut vrai
myProduct :: [Int] -> Int -- multiplie tous les élements de la liste
myTake :: Int -> [Int] -> [Int] -- renvoie tous les élements de la liste jusqu'à l'index passé en paramètre
myDrop :: Int -> [Int] -> [Int] -- renvoie tous les élements de la liste à partir de l'index passé en paramètre
myBangBang :: [Int] -> Int -> Int -- accesseur classique, l'index commence à 0
myInsert :: Int -> [Int] -> [Int] -- insère un élément à sa place dans une liste triée
mySort :: [Int] -> [Int] -- trie une liste dans l'ordre croissant
```

## TP 2

Activités sur les listes génériques.
Utilisation de `foldr` et fonctions associées.

```haskell
{- redéfinition avec des génériques -}
myHead :: [a] -> a
myTail :: [a] -> [a]
myAppend :: [a] -> [a] -> [a]
myInit :: [a] -> [a]
myLast :: [a] -> a
myNull :: [a] -> Bool -- avec wild card
myLength :: [a] -> Int
myNull' :: [a] -> Bool -- avec length
myReverse :: [a] -> [a]
myConcat :: [[a]] -> [a]
myTake :: Int -> [a] -> [a]
myDrop :: Int -> [a] -> [a]
myBangBang :: [a] -> Int -> a

{- introduction des types classes -}
{-
Eq : Eq anyone declaring an instance of Eq only has to specify an implementation of (==), and they will get (/=) for free, if not implemented.
Ord : Ord is for types whose elements can be totally ordered, that is, where any two elements can be compared to see which is less than the other. It provides comparison operations like (<) and (<=), and also the compare function.
Show : Show defines the method show, which is used to convert values into Strings.
-}
myInsert :: Ord a => a -> [a] -> [a]
mySort :: Ord a => [a] -> [a]
myNull'' :: Eq a => [a] -> Bool -- avec Eq

{- array function -}
myTakeWhile :: (a -> Bool) -> [a] -> [a] -- appliquée à un prédicat évaluant b et une liste de b, renvoie le plus long préfixe (possiblement vide) d'élement b validant le prédicat
myCompose :: (a -> b) -> (c -> a) -> c -> b -- composition de fonction
myUndefined :: a
myMap :: (a -> b) -> [a] -> [b] --appliquée à une fonction et une liste, renvoie une nouvelle liste obtenue de l'application de la fonction à chacun des éléments de la liste
sousListes :: [a] -> [[a]] -- [1..2] => [[],[2],[1],[1,2]]

{- Foldr -}
myFoldr :: (a -> b -> b) -> b -> [a] -> b
{-
foldr : si l'on reprend myCons, remplace ':' par f et '[]' par k
foldr : applique une fonction qui est un « accumulateur » et qui traite chaque valeur d'une liste (de la gauche vers la droite) afin de la réduire à une seule valeur, équivalent de (Array.reduce en JS)
-}
myAnd' :: [Bool] -> Bool -- intersection sur l'ensemble des propositions

{- lambda anonymes -}
add' :: Int -> Int -> Int
add'' :: Int -> Int -> Int
{- "nouveaux types" -}
s1 :: String -- [Char]
myFst :: (a, b) -> a -- tuples : différent types formant un plus grand type -}

myDropWhile :: (a -> Bool) -> [a] -> [a] -- ignore les élements d'une liste tant qu'il respecte une condition et retourne les élements restant
myElem :: Eq a => a -> [a] -> Bool -- renvoie True si l'élement y est inclus dans la liste [a] --'Eq a => a' est le 1er argument est a qui est de type Eq (cette classe implémente l'égalité)
myElem' :: Eq a => a -> [a] -> Bool -- mon element fait il parti de ma liste
myNotElem :: Eq a => a -> [a] -> Bool -- selectionne les elements qui vérifient la condition
myNotElem' :: Eq a => a -> [a] -> Bool -- renvoie deux sous listes d'une liste séparée à l'indice donné
myNotElem'' :: Eq a => a -> [a] -> Bool -- mon element est il absent de ma liste
myFilter :: (a -> Bool) -> [a] -> [a] -- selectionne les elements qui vérifient la condition
mySplitAt :: Int -> [a] -> ([a], [a]) -- renvoie deux sous listes d'une liste séparée à l'indice donné
myZip :: [a] -> [b] -> [(a, b)] -- prend deux listes et renvoie une liste de paires correspondantes
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -- généralise 'zip' en zippant avec la fonction donnée en premier argument, au lieu d'une tupling fonction
myCurry :: ((a, b) -> c) -> a -> b -> c -- converts an uncurried function to a curried function
myUncurry :: (a -> b -> c) -> (a, b) -> c -- réciproque de curry
myUnzip :: [(a, b)] -> ([a], [b]) --  transforme une liste de paires en une liste de premiers composants et une liste de seconds composants.
myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] -- non récursif

{- redéfinition avec foldr -}
myConcat' :: [[a]] -> [a]
myMap' :: (a -> b) -> [a] -> [b]
myOr' :: [Bool] -> Bool
myAny :: (a -> Bool) -> [a] -> Bool
myAll :: (a -> Bool) -> [a] -> Bool
myProduct :: [Int] -> Int -- réalise le produit de tous les membres d'une liste
mySum :: [Int] -> Int -- somme tous les membres d'une liste
mySort' :: [Int] -> [Int]
myReverse' :: [a] -> [a]
```

## TP 3

Activités sur les listes en comprehension
Activités 'Des chiffres et des lettres' avec des sous listes et permutations.

```haskell
{- liste en compréhension -}
rectangle :: [(Int, Int)]
triangle :: [(Int, Int)]

{- permutation -}
myQSort :: Ord a => [a] -> [a] --quickSort
sousListes :: [a] -> [[a]] -- [1..2] => [[],[2],[1],[1,2]]
injections :: a -> [a] -> [[a]] -- 3 [1..2] => [[3,1,2],[1,3,2],[1,2,3]]
permuts :: [a] -> [[a]] -- [1..3] => [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
permSousListes :: [a] -> [[a]] -- [1..2] => [[2],[1],[1,2],[2,1]]
partitionStricte :: [a] -> [([a], [a])] -- [1..4] => [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

concat :: Foldable t => t [a] -> [a] -- The concatenation of all the elements of a container of lists.

{- enumération et dérivation -}
-- solutions => CA : listes en comprehension, utilisation de `concat`
-- exps      => CA : listes en comprehension

data Op = Add | Sub | Mul | Div
data Exp = Val Int | App Op Exp Exp

{- I) generate and test (brute force) -}
validOp :: Op -> Int -> Int -> Bool -- parcours  énumération
evalOp :: Op -> Int -> Int -> Int -- parcours énumération et utilisation de `div` --integer division
exps :: [Int] -> [Exp]
evalExp :: Exp -> Int
validExp :: Exp -> Bool
solutions :: [Int] -> Int -> [Exp]

{- II) fusionner la generation et le filtrage des expressions invalides -}
exps2 :: [Int] -> [Exp]
solutions2 :: [Int] -> Int -> [Exp]

{- III) memoïser l'evaluation -}
evalExp' :: Exp' -> Int
exps3 :: [Int] -> [Exp']
solutions3 :: [Int] -> Int -> [Exp']

{- IV) exploiter des proprietes arithmetiques -}
validOp' :: Op -> Int -> Int -> Bool --utilisation de `mod` --integer modulus
exps4 :: [Int] -> [Exp']
solutions4 :: [Int] -> Int -> [Exp']
nombreDeSolutions3 :: Int
nombreDeSolutions4 :: Int

{- V) ne retourner qu'une solution exacte ou bien la plus proche -}
distance :: Int -> Exp -> Int
solutions5 :: [Int] -> Int -> [Exp']
```
