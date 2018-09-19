module Exercises where 

-- tests are defined at the end of the file to test implementations
import Test.HUnit

-- diference of two lists,
-- that is, a - b
diff :: (Eq a) => [a] -> [a] -> [a]
diff a b = [x | x <- a, not $ x `elem` b]

-- intersection of two lists,
-- that is, elements in list a that are also in b
intersec :: (Eq a) => [a] -> [a] -> [a]
intersec a b = [x | x <- a, x `elem` b]

-- union of two lists
-- with repetition
union_list :: [a] -> [a] -> [a]
union_list a b = a ++ b

-- union of two lists
-- without repetition
union_set :: (Eq a) => [a] -> [a] -> [a]
union_set a b = a ++ (diff b a)

-- returns last value of list
last_el :: (Eq a) => [a] -> a
last_el [] = error "List is empty"
last_el (x:xs)
    | xs == [] = x
    | otherwise = last_el xs

-- return nth element of list
(@@) :: [a] -> Int -> a
(x:xs) @@ 0 = x
(x:[]) @@ n = error "Index out of bounds."
(x:xs) @@ n = xs @@ (n-1)

-- reverses a list
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- order list in decresing order removing replicates
sort_down :: (Eq a, Ord a) =>[a] -> [a]
sort_down [] = []
sort_down (x:xs)
    | x `elem` xs = sort_down xs
    | not $ x `elem` xs = (sort_down [h | h <- xs, h > x]) ++ [x] ++ [e | e <- xs, e == x] ++ (sort_down [l | l <- xs, l < x])

-- sorts list
qsort ::  (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort [l | l <- xs, l < x]) ++ [x] ++ [e | e <- xs, e == x] ++ (qsort [h | h <- xs, h > x])

-- returns wether list is in decreasing order
is_decreasing :: (Ord a) => [a] -> Bool
is_decreasing [] = True
is_decreasing (a:[]) = True
is_decreasing (a:b:xs)
    | a >= b = is_decreasing (b:xs)
    | a < b = False
    
-- Classic FizzBuzz One Liner
fizzbuzz = [if x `mod` 15 == 0 then "FizzBuzz" else if x `mod` 3 == 0 then "Fizz" else if x `mod` 5 == 0 then "Buzz" else (show x) | x <- [1..100]]

-- The lines that follow are tests of the functions above. I was getting a lot of error when I put the tests in a different file so... deal with it.
-- To test in ghci do:
-- :load exercises
-- runTestTT tests

a = [1,2,3,4,5]
b = [3,4,5,6,7]

rep_low = [1,1,2,2,3,3]
rep_high = [3,3,4,4,5,5]

fuzzy = [10, 5, 2, -5, 0, 1]

test_1 = TestCase (assertEqual "Diff test" [1,2] (diff a b))
test_2 = TestCase (assertEqual "Intersection" [3,4,5] (intersec a b))
test_3 = TestCase (assertEqual "Union with Repetition" [1,2,3,4,5,3,4,5,6,7] (union_list a b))
test_4 = TestCase (assertEqual "Union without Repetition" [1,2,3,4,5,6,7] (union_set a b))
test_5 = TestCase (assertEqual "Last Element" (5, 7) (last_el a, last_el b))
test_6 = TestCase (assertEqual "Access Element" (3, 5) (a @@ 2, b @@ 2))
test_7 = TestCase (assertEqual "Reverse" a ((rev . rev) a))
test_8 = TestCase (assertEqual "QSort" [-5, 0, 1, 2, 5, 10] (qsort fuzzy))
test_9 = TestCase (assertEqual "SortDown" [3,2,1] (sort_down rep_low))
test_10 = TestCase (assertBool "Decreasing order - Recursive" ((is_decreasing . sort_down) fuzzy))

tests = TestList [test_1, test_2, test_3, test_4, test_5, test_6, test_7, test_8, test_9, test_10]