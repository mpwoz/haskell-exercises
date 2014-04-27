length' []     = 0
length' (x:xs) = 1 + (length' xs)

null' [] = True
null' _  = False

head' []     = error "head': Empty List"
head' (x:xs) = x

last' []     = error "last': Empty list"
last' (x:xs) = if (null xs)
               then x
               else last' xs

init' []     = error "init': Empty list"
init' (x:xs) = if (null xs)
               then []
               else x : (init' xs)

-- Two version of the append function
append [] ys = ys
append xs ys = append (init xs) ((last xs) : ys)

-- I think this one is more efficient
append2 [] ys = ys
append2 (x:xs) ys = x : (append xs ys)

concat' []     = []
concat' (l:ls) = append l (concat' ls)

reverse' []     = []
-- quadratic time
--reverse' (x:xs) = (reverse' xs) ++ [x]
-- Linear time
reverse' l = rev l []
  where 
    rev [] a = a
    rev (x:xs) a = 
      rev xs (x:a)

and' []     = True
and' (x:xs) = x && (and' xs)

or' []     = False
or' (x:xs) = x || (or' xs)

map' _ []     = []
map' f (x:xs) = (f x) : (map' f xs)

all' :: (a -> Bool) -> [a] -> Bool
all' f l = and' (map' f l)

any' f l = or' (map' f l)

take' n l 
  | n <= 0 = [] 
  | null l = []
take' n (x:xs) = x : (take' (n-1) xs)

splitAt' _ []     = ([], [])
splitAt' f (x:xs) = 
  if f x
  then ([], x:xs)
  else (x:(fst sp), snd sp)
    where sp = splitAt' f xs

-- A more elegant way to do both of these, using existing functionality
takeWhile' f l = fst (splitAt' (not . f) l)
dropWhile' f l = snd (splitAt' (not . f) l)

{-takeWhile' _ []     = []-}
{-takeWhile' f (x:xs) = -}
{-  if f x-}
{-  then x : (takeWhile' f xs)-}
{-  else []-}

{-dropWhile' _ []     = []-}
{-dropWhile' f (x:xs) = -}
{-  if f x-}
{-  then dropWhile' f xs-}
{-  else (x:xs)-}

-- Not efficient (2 traversals), but neat
span'  f l = (takeWhile' f l,         dropWhile' f l)
break' f l = (takeWhile' (not . f) l, dropWhile' (not . f) l)

elem' _ []     = False
elem' e (x:xs) = (x == e) || elem' e xs

notElem' e l = not (elem' e l)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs)
  | f x       = x : (filter' f xs)
  | otherwise = filter' f xs

isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (p:ps) (l:ls) = p == l && isPrefixOf ps ls

{-isInfixOf [] _ = True-}
{-isInfixOf _ [] = False-}
{-isInfixOf (d:ds) (l:ls)-}
{-  | d == l    = isPrefixOf ds ls || isInfixOf (d:ds) ls-}
{-  | otherwise = isInfixOf (d:ds) ls-}

-- This is how haskell standard lib does them, amazing!
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
isSuffixOf p l = (reverse p) `isPrefixOf` (reverse l)
tails xs = xs : case xs of
                  []      -> []
                  _ : xs' -> tails xs'

zip' :: [a] -> [b] -> [(a, b)]
zip' (a:as) (b:bs) = (a, b) : zip' as bs
zip' _      _      = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = (f a b) : zipWith' f as bs
zipWith' _ _      _      = []


words' :: String -> [String]
words' ""      = []
words' (' ':s) = words' s
words' s       = (fst split) : words' (snd split)
                   where split = splitAt' ((==) ' ') s

unwords' :: [String] -> String
unwords' w = intersperse ' ' w

-- Join a list of lists together using a separator value
intersperse :: a -> [[a]] -> [a]
intersperse _ []     = []
intersperse _ [x]    = x
intersperse s (x:xs) = x ++ (s : (intersperse s xs))

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs 

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:xs) 
  | null xs   = Just x
  | otherwise = safeLast xs

-- Alternatively, could import fromMaybe here and use it
safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit (x:xs) 
  | null xs   = Just []
  | otherwise = Just (x : rest)
      where (Just rest) = safeInit xs


-- Break the list everywhere the predicate is false
-- Almost like the 'words' function, but for lists of anything
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f lis = 
  case break (not . f) lis of 
    ([], [])   -> []
    (as, [])   -> [as]
    (as, b:[]) -> as : [[b]]
    (as, b:bs) -> let x = splitWith f bs in
                    as : (b : head x) : tail x

-- Another way of doing it with accumulator variables
-- Less efficient because of all the reversals
splitWith2 :: (a -> Bool) -> [a] -> [[a]]
splitWith2 _ [] = []
splitWith2 f l  = splitWith' f l [] []
  where splitWith' _ [] w ws     = reverse ((reverse w):ws)
        splitWith' f (x:xs) w ws 
          | f x       = splitWith' f xs (x:w) ws
          | otherwise = splitWith' f xs [x] ((reverse w):ws)


-- I misunderstood the exercise, it's supposed to get rid of the 
-- elements where predicate is false. So using a 
-- predicate of ((==) ' ') on a string would be the same as 
-- the 'words' function. This is correct: 
splitWith3 :: (a -> Bool) -> [a] -> [[a]]
splitWith3 _ [] = []
splitWith3 f xs = let (a, b) = break f (dropWhile f xs) in
                    a : splitWith3 f (dropWhile f b)















