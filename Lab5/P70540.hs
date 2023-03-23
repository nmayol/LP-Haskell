
import Control.Monad
data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add ex1 ex2) = (eval1 ex1) + (eval1 ex2)
eval1 (Sub ex1 ex2) = (eval1 ex1) - (eval1 ex2)
eval1 (Mul ex1 ex2) = (eval1 ex1) * (eval1 ex2)
eval1 (Div ex1 ex2) = (eval1 ex1) `div `(eval1 ex2)

eval2 :: Expr -> Maybe Int
--- liftM2 (+) (Just 3) (Just 4)
eval2 (Val x) = (Just x)
eval2 (Add ex1 ex2) = do
    sx <- eval2 ex1
    sy <- eval2 ex2
    (Just (sx+sy))
eval2 (Sub ex1 ex2) = do
    sx <- eval2 ex1
    sy <- eval2 ex2
    (Just (sx-sy))
eval2 (Mul ex1 ex2) = do
    sx <- eval2 ex1
    sy <- eval2 ex2
    (Just (sx*sy))
eval2 (Div ex1 ex2) = do
    sx <- eval2 ex1
    sy <- eval2 ex2
    if sy == 0 then Nothing else (Just (div sx sy))



eval3 :: Expr -> Either String Int 
eval3 (Val x) = (Right x)
eval3 (Add ex1 ex2) = do
    sx <- eval3 ex1
    sy <- eval3 ex2
    (Right (sx+sy))
eval3 (Sub ex1 ex2) = do
    sx <- eval3 ex1
    sy <- eval3 ex2
    (Right (sx-sy))
eval3 (Mul ex1 ex2) = do
    sx <- eval3 ex1
    sy <- eval3 ex2
    (Right (sx*sy))
eval3 (Div ex1 ex2) = do
    sx <- eval3 ex1
    sy <- eval3 ex2
    if sy == 0 then (Left ("div0")) else (Right (div sx sy))