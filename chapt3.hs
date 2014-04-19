-- Exercises from chapter 3 of the book
import Data.Function
import Data.List


myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs


-- Not sure why this is the type signature..
avg :: (Fractional a) => [a] -> a
avg [] = error "Empty list has no average"
avg xs = (sum xs) / (fromIntegral (length xs))


-- Utility reverse function for the next two palindrome functions
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = (rev xs) ++ [x]

-- Turn a list into a palindrome. (Not sure if this is the best solution)
pal xs = xs ++ (rev xs)

-- Determine whether list is a palindrome
isPal xs = (xs) == (rev xs)

-- Sort a list of lists by length of the sublists
-- The following are equivalent
-- sortByLength xs = sortBy (\x y -> compare (length x) (length y)) xs
sortByLength xs = sortBy (compare `on` length) xs

-- Join a list of lists together using a separator value
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ []     = []
myIntersperse _ [x]    = x
myIntersperse s (x:xs) = x ++ (s : (myIntersperse s xs))

-- Define a binary tree type
data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show)

-- Find depth of a binary tree
depth :: Tree a -> Int
depth Empty        = 0
depth (Node _ l r) = 1 + (max (depth l) (depth r))

-- A test tree of depth 3
t = Node 3 (Node 2 (Empty) 
                   (Node 1 (Empty) (Empty))) 
           (Node 9 Empty Empty)


-- Define the types for 2d points and directions
data Point2D = Point2D Double Double
               deriving (Show, Eq)

data Direction = DLeft | DRight | DStraight
                 deriving (Show)


-- Determine which direction you turn going through the points in order given
whichDir :: Point2D -> Point2D -> Point2D -> Direction
whichDir (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3)
  | cross == 0 = DStraight
  | cross < 0  = DRight
  | cross > 0  = DLeft
  where cross = (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1)

-- Some test points
pa = Point2D (-2) 0
pb = Point2D   0  0
pc = Point2D   0  2
pd = Point2D   2  0
pe = Point2D   1  1

-- Given list of points, compute direction of each consecutive triple
tripleDirs :: [Point2D] -> [Direction]
tripleDirs (a:b:c:xs) = (whichDir a b c) : (tripleDirs (b:c:xs))
tripleDirs _          = []

-- TODO convex hull using Graham scan
-- Find lowest-y point, call it p0
-- Sort the other points using polar angle to p0
-- 



