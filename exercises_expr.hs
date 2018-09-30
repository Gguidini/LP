module Expr where

-- tests are defined at the end of the file to test implementations
import Test.HUnit

-- Data type
data Expr t = Lit t         |
            Add (Expr t) (Expr t)   |
            Sub (Expr t) (Expr t)   |
            Mul (Expr t) (Expr t)   |
            Div (Expr t) (Expr t)    deriving (Show, Eq, Ord)
            
-- Evaluates Expressions
eval :: (Fractional t) => Expr t -> t
eval (Lit n) = n
eval (Add lhs rhs) = (eval lhs) + (eval rhs)
eval (Sub lhs rhs) = (eval lhs) - (eval rhs)
eval (Mul lhs rhs) = (eval lhs) * (eval rhs)
eval (Div lhs rhs) = (eval lhs) / (eval rhs)

tell :: (Show a) => Expr a -> [Char]
tell (Lit a) = show a
tell (Add lhs rhs) = (tell lhs) ++ " + " ++ (tell rhs)
tell (Sub lhs rhs) = (tell lhs) ++ " - " ++ (tell rhs)
tell (Mul lhs rhs) = (tell lhs) ++ " * " ++ (tell rhs)
tell (Div lhs rhs) = (tell lhs) ++ " / " ++ (tell rhs)


-- The lines that follow are tests of the functions above. I was getting a lot of error when I put the tests in a different file so... deal with it.
-- To test in ghci do:
-- :load exercises
-- runTestTT tests

exp_str = Lit "Oi"
exp_n = Lit 3.1415
exp_add = Add (Lit 1) (Lit 2)
exp_sub = Sub (Lit 10) (Lit 5)
exp_mul = Mul (Lit 2) (Lit 4)
exp_div = Div (Lit 5.0) (Lit 2.0)

test_1 = TestCase (assertEqual "Expressão Literal" 3.1415 (eval exp_n))
test_2 = TestCase (assertEqual "Expressão Add" 3 (eval exp_add))
test_3 = TestCase (assertEqual "Expressão Sub" 5 (eval exp_sub))
test_4 = TestCase (assertEqual "Expressão Mul" 8 (eval exp_mul))
test_5 = TestCase (assertEqual "Expressão Div" 2.5 (eval exp_div))

tests = TestList [test_1, test_2, test_3, test_4, test_5]




