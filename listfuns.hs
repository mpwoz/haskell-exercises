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

