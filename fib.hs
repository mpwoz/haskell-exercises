-- Naive i'th fibonacci number
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)

-- Naive range of fibonacci numbers
fibs :: Integer -> [Integer]
fibs 0 = []
fibs x = fibs (x-1) ++ [fib x]

-- fast (dynamic programming) range of fibonacci numbers
dfibs :: Integer -> [Integer]
dfibs x = reverse $ rfibs x

rfibs 0 = []
rfibs 1 = [1]
rfibs x = (sum $ take 2 $ p) : p
            where p = rfibs (x-1)

-- fast (dynamic programming) i'th fibonacci number
dfib :: Integer -> Integer
dfib x = head $ rfibs x

-- fast (dynamic programming) i'th fibonacci number with reduced memory footprint
cfib :: Integer -> Integer
cfib x = cfib' 1 1 2 x
cfib' prev curr i x
    | x <= i    = curr
    | otherwise = cfib' curr (curr+prev) (i+1) x
