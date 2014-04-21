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


takeWhile' _ []     = []
takeWhile' f (x:xs) = 
  if f x
  then x : (takeWhile' f xs)
  else []

dropWhile' _ []     = []
dropWhile' f (x:xs) = 
  if f x
  then dropWhile' f xs
  else (x:xs)

-- Not efficient (2 traversals), but neat
span' f l = (takeWhile' f l, dropWhile' f l)
break' f l = (takeWhile' (\x -> not (f x)) l, dropWhile' (\x -> not (f x)) l)

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


















