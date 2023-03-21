
import Control.Monad
data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add ex1 ex2) = (eval1 ex1) + (eval1 ex2)
eval1 (Sub ex1 ex2) = (eval1 ex1) - (eval1 ex2)
eval1 (Mul ex1 ex2) = (eval1 ex1) * (eval1 ex2)
eval1 (Div ex1 ex2) = (eval1 ex1) `div `(eval1 ex2)

eval2 :: Expr -> Maybe Int
--- liftM2 (+) (Just 3) (Just 4)
eval2 (Val x) = (Just x)
eval2 (Add ex1 ex2) = liftM2 (+) (eval2 ex1) (eval2 ex2)
eval2 (Sub ex1 ex2) = liftM2 (-) (eval2 ex1) (eval2 ex2)
eval2 (Mul ex1 ex2) = liftM2 (*) (eval2 ex1) (eval2 ex2)
eval2 (Div ex1 ex2) = liftM2 div (eval2 ex1) (eval2 ex2)
