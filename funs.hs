add a b = a + b

mytake a b = 
  if a < 1 || b == []
  then []
  else (head b) : (mytake (a-1) (tail b))

mydrop :: Int -> [a] -> [a]
mydrop n xs = 
  if n <= 0 || null xs
  then xs
  else mydrop (n-1) (tail xs)

-- Breaks if passed an empty list
mylast :: [a] -> a
mylast []     = error "Empty list has no last element"
mylast [x]    = x
mylast (x:xs) = mylast xs

mysecond :: [a] -> a
mysecond []     = error "List empty"
mysecond (x:xs) = if null xs
                  then error "List too short"
                  else head xs

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond (x:xs) = if null xs
                    then Nothing
                    else Just (head xs)

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing


sumList (x:xs) = x + sumList xs
sumlist []     = 0


-- Define recursive, parameterized list type
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- Translate haskell's list to a custom list
fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- ...and the other way around
toList :: List a -> [a]
toList (Cons x xs) = x : (toList xs)
toList Nil         = []


-- Binary tree
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

