module Exercises.Trees where

-- Tests are defined at the end of file
import Test.HUnit

-- data for Trees
data Tree t = Nil | Node t (Tree t) (Tree t) deriving (Eq, Ord, Show)


foldTree :: (t -> t -> t) -> Tree t -> t -> t
foldTree f Nil def = def 
foldTree f (Node n (lhs) (rhs)) def = f n (f (foldTree f lhs def) (foldTree f rhs def)) 

addTree :: (Num a) => Tree a -> a
addTree Nil = 0
addTree (Node n lhs rhs) = n + addTree lhs + addTree rhs

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node a lhs rhs) = (Node (f a) (mapTree f lhs) (mapTree f rhs))

depth :: Tree a -> Int
depth Nil = 0
depth (Node _ lhs rhs) = 1 + max (depth lhs) (depth rhs)

collapse :: Tree t -> [t]
collapse Nil = []
collapse (Node n lhs rhs) = [n] ++ (collapse lhs) ++ (collapse rhs)

sq :: (Num t) => t -> t
sq a = a * a

-- The lines that follow are tests of the functions above. I was getting a lot of error when I put the tests in a different file so... deal with it.
-- To test in ghci do:
-- :load exercises
-- runTestTT tests

deepLhs = Node 0 (Node 1 (Node 2 Nil Nil) Nil) Nil
deepRhs = Node 0 Nil (Node 1 Nil (Node 2 Nil Nil))
strTree = Node "Hello " (Node "World" Nil Nil) (Node "!" Nil Nil)

-- Tests for addTree
test_1 = TestCase (assertEqual "Test add tree - Nil" 0 (addTree Nil))
test_2 = TestCase (assertEqual "Test add tree - lhs" 1 (addTree (Node 1 (Nil) (Nil))))
test_3 = TestCase (assertEqual "Test add tree - both" 2 (addTree (Node 1 (Nil) (Node 1 (Nil) (Nil)))))

-- Tests for mapTree
test_4 = TestCase (assertEqual "Test mapTree - sq" (Node 1 (Node 9 Nil Nil) (Node 4 Nil Nil)) (mapTree sq (Node 1 (Node 3 Nil Nil) (Node 2 Nil Nil))))
test_5 = TestCase (assertEqual "Test mapTree - fst" (Node 1 (Node 9 Nil Nil) (Node 4 Nil Nil)) (mapTree fst (Node (1, 10) (Node (9,3) Nil Nil) (Node (4,2) Nil Nil))))

-- Tests for foldTree
test_6 = TestCase (assertEqual "Test foldTree - (+)" 14 (foldTree (+) (Node 1 (Node 9 Nil Nil) (Node 4 Nil Nil)) 0))
test_10 = TestCase (assertEqual "Test foldTree - concat" "Hello World!" (foldTree (++) strTree ""))

-- Tests for depth
test_7 = TestCase (assertEqual "Test Depth - Nil" 0 (depth Nil))
test_8 = TestCase (assertEqual "Test Depth - lhs deeper" 3 (depth deepLhs))
test_9 = TestCase (assertEqual "Test Depth - rhs deeper" 3 (depth deepRhs))

-- Tests for
test_11 = TestCase (assertEqual "Test Collapse - strings" ["Hello ", "World", "!"] (collapse strTree))
test_12 = TestCase (assertEqual "Test Collapse - ints" [0,1,2] (collapse deepLhs))

tests = TestList [test_1, test_2, test_3, test_4, test_5, test_6, test_7, test_8, test_9, test_10, test_11, test_12]

