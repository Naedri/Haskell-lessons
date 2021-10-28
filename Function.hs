
{- lend -}
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

{- drop -}
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