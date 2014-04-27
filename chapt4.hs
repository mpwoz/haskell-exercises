import Data.Char (digitToInt, isUpper)

loop :: Int -> String -> Int
loop acc []     = acc
loop acc (x:xs) = let acc' = 10 * acc + digitToInt x
                  in loop acc' xs

asInt :: String -> Int
asInt xs = loop 0 xs


squares xs = map sq xs
  where sq x = x * x

-- This kind of partial application for infix operators is called a section
squares2 = map (^2)

myFilter _ [] = []
myFilter f xs = foldr step [] xs
  where step x ys | f x       = x : ys 
                  | otherwise = ys 


-- write foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
  where step x g a = g (f a x)
{-
   Trace an example through myFoldl

   myFoldl (-) 0 (1:2:3:[])
== foldr step id (1:2:3:[]) 0
== step 1 (step 2 (step 3 id)) 0
== step 2 (step 3 id) (- 0 1)
== step 2 (step 3 id) (-1)
== step 3 id (- (-1) 2)
== step 3 id (-3)
== id (- (-3) 3)
== id -6
== -6
-}

-- foldr : "Its first two arguments are 
-- “what to do with each head/tail element of the list”, 
-- and “what to substitute for the end of the list”."

-- more fun with foldr
append xs ys = foldr (:) ys xs



-- Exercises
-- 1. redo asInt to use folds
asInt' ('-':xs) = (-1) * asInt' xs
asInt' xs       = foldl step 0 xs
                    where step a x = a * 10 + digitToInt x

-- 3. Write concat using foldr
concat' :: [[a]] -> [a]
concat' ls = foldr (++) [] ls

-- 4. TakeWhile using foldr
takeWhile' p xs = foldr step [] xs
  where step x acc | p x       = x : acc
                   | otherwise = []

-- 5. rewrite 'group' using folds
group' xs = foldr step [] xs
  where step x []                     = [[x]]
        step x (g:gs) | x == (head g) = (x:g) : gs
                      | otherwise     = [x] : g : gs


-- 6. Implement other functions with folds
any'     :: (a -> Bool) -> [a] -> Bool
cycle'   :: [a] -> [a]
unlines' :: [String] -> String
words'   :: String -> [String]

any'   p xs = foldl (||) False (map p xs)
cycle'   xs = foldr (:) (cycle' xs) xs
unlines' xs = tail $ foldr (\s a -> '\n':s++a) "" xs
words'   x  = foldr step [""] x
  where step l (w:ws) | l == ' ' = "":w:ws
                      | otherwise = (l:w):ws
-- End exercises.


compose :: (b -> c) -> (a -> b) -> a -> c
compose fa fb x = fa (fb x)

capCount :: String -> Int
capCount = length . filter (isUpper . head) . words
