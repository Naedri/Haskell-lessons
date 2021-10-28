import Data.Sequence.Internal.Sorting (QList (Nil))

-- 1) ecrire votre identite

-- 2) renommer le fichier en remplacant Nom et Prenom

-- fonction autorisees : (+), (++), (==), (&&), (.), concat, div, drop, foldr, length, map, max, take

-- interdiction d'introduire de nouvelles definitions (locales ou globales)

-- definir RECURSIVEMENT : la fonction definie se rappelle elle meme
-- definir NON RECURSIVEMENT : la fonction definie ne se rappelle pas elle meme

-- une fonction ANONYME est une fonction de la forme (\x -> ...)

-- une liste EN COMPREHENSION est une liste de la forme [ E0 | P1 <- E1, ... ]

-- cherchez les definitions les plus simples

-- liste

-- 1) definir RECURSIVEMENT les prefixes d'une liste

prefixes :: [a] -> [[a]]
prefixes (x : xs) = [] : map (x :) (prefixes xs)
prefixes [] = [[]]

test1 = prefixes [1 .. 5] == [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]

-- 2) definir RECURSIVEMENT le suffixes non vides d'une liste

suffixesStrict :: [a] -> [[a]]
suffixesStrict (x : xs) = (x : xs) : suffixesStrict xs
suffixesStrict [] = []

test2 = suffixesStrict [1 .. 5] == [[1, 2, 3, 4, 5], [2, 3, 4, 5], [3, 4, 5], [4, 5], [5]]

-- 3) definir NON RECURSIVEMENT les segments continus d'une liste

segments :: [a] -> [[a]]
segments list = prefixes list ++ suffixesStrict list

test3 = segments [1 .. 5] == [[], [1], [1, 2], [2], [1, 2, 3], [2, 3], [3], [1, 2, 3, 4], [2, 3, 4], [3, 4], [4], [1, 2, 3, 4, 5], [2, 3, 4, 5], [3, 4, 5], [4, 5], [5]]

-- 4) definir avec un foldr et une fonction anonyme les prefixes d'une liste (voir question 1)

prefixesFoldr :: [a] -> [[a]]
prefixesFoldr = undefined

test4 = prefixesFoldr [1 .. 5] == [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]

-- 5) definir avec une liste EN COMPREHENSION les prefixes d'une liste (voir la question 1)

prefixesZF :: [a] -> [[a]]
prefixesZF = undefined

test5 = prefixesZF [1 .. 5] == [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]

-- arbre

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Eq, Show)

t0 :: Tree Int
t0 = Node (Leaf 1) (Leaf 3)

t0' :: Tree Int
t0' = Leaf 1

t1 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

t2 :: Tree Char
t2 = Node (Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') (Leaf 'd'))) (Node (Node (Leaf 'e') (Leaf 'f')) (Node (Leaf 'g') (Leaf 'h')))

t3 :: Tree Char
t3 = Node (Node (Leaf 'a') (Leaf 'b')) (Node (Node (Node (Leaf 'c') (Leaf 'd')) (Node (Leaf 'e') (Leaf 'f'))) (Node (Leaf 'g') (Leaf 'h')))

-- 6) definir RECURSIVEMENT la valeur des feuilles de gauche à droite

frange :: Tree a -> [a]
frange (Node left right) = frange left ++ frange right
frange (Leaf value) = [value]

test6 = frange t1 == [1, 2, 3]

test6' = frange t2 == "abcdefgh"

test6'' = frange t3 == "abcdefgh"

-- 7) definir RECURSIVEMENT le nombre de Node sur la branche la plus longue

profondeur :: Tree a -> Int
profondeur (Node left right) = max ((profondeur left) + 1) ((profondeur right) + 1)
profondeur (Leaf value) = 0

test7 = profondeur t1 == 2

test7' = profondeur t2 == 3

test7'' = profondeur t3 == 4

-- 8) definir RECURSIVEMENT la fonction qui verifie que toutes les branches ont la meme longueur

estEquilibre :: Tree a -> Bool
estEquilibre (Node left right) = profondeur right == profondeur left
estEquilibre (Leaf _) = True

test8 = not (estEquilibre t1)

test8' = estEquilibre t2

test8'' = not (estEquilibre t3)

test8''' = estEquilibre t0

test8'''' = estEquilibre t0'

-- 9) definir RECURSIVEMENT la fonction qui construit un arbre equilibre a partir de 2^n valeurs

construit :: [a] -> Tree a
construit (x : xs) = Node (Leaf x) (construit xs)
construit (x) = Leaf x

test9 = construit (take (2 ^ 3) ['a' ..]) == t2

test9' = construit (take (2 ^ 3) [1 ..]) == t0

test9'' = construit (take (2 ^ 3) [1 ..]) == t0'

-- 10) definir NON RECURSIVEMENT la transformation d'un arbre en un arbre equilibre

-- reequilibre :: Tree a -> Tree a
-- reequilibre (x:xs)
--   | estEquilibre (x:xs) = (x:xs)
--   | otherwise Node (Leaf x) (reequilibre xs)
-- reequilibre []      = Nil

-- test10 = reequilibre t2 == t2

-- 11) definir RECURSIVEMENT la transformation d'un arbre en un arbre equilibre
-- reequilibre :: Tree a -> Tree a
-- reequilibre arbre = construit (frange arbre)

-- test11 = reequilibre t3 == t2
