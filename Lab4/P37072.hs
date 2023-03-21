 
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2


size :: Tree a -> Int
size Empty = 0
size (Node a a_l a_r) = 1 + (size a_l) + (size a_r)

height :: Tree a -> Int
height Empty = 0
height (Node a l_tree r_tree) = 1 + max(height l_tree) (height r_tree)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node x xl xr) (Node y yl yr) = (x == y) && ((equal xl yl && equal xr yr) || (equal xl yr && equal xr yl) )

preOrder :: Tree a -> [a] 
preOrder Empty = []
preOrder (Node x l_tree r_tree) = x : (preOrder l_tree ++ preOrder r_tree)

postOrder :: Tree a -> [a] 
postOrder Empty = []
postOrder (Node x l_tree r_tree) = (postOrder l_tree) ++ (postOrder r_tree) ++ [x]

inOrder :: Tree a -> [a] 
inOrder Empty = []
inOrder (Node x l_tree r_tree) = (inOrder l_tree) ++ [x] ++ (inOrder r_tree)

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst (Node x (Empty) (Empty)) = [x]
breadthFirst (Node x (Empty) (Node r (rl) (rr))) = [x] ++ [r] ++ (breadthFirst rl) ++ (breadthFirst rr)
breadthFirst (Node x (Node l (ll) (lr)) (Empty)) = [x] ++ [l] ++ (breadthFirst ll) ++ (breadthFirst lr)
breadthFirst (Node x (Node l (ll) (lr)) (Node r (rl) (rr))) = [x] ++ [l] ++ [r] ++ (breadthFirst ll) ++ (breadthFirst lr) ++ (breadthFirst rl) ++ (breadthFirst rr)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node x l r) = False
equal (Node x l r) Empty = False
equal (Node x xl xr) (Node y yl yr) = (x == y) && (equal xl yl) && (equal xr yr)

-- build [1,2,4,5,3] [4,2,5,1,3]

build :: Eq a => [a] -> [a] -> Tree a
build _ [] = Empty
build [] _ = Empty
build (x:xs) y = (Node x (build xst tw) (build xsd dw))
    where
        tw = (takeWhile (/= x) y)
        dw = (dropWhile (/= x) y)
        xst = filter (\t -> t `elem` tw) xs
        xsd = filter (\d -> d `elem` dw) xs

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ Empty x = x
overlap _ x Empty = x
overlap f (Node x xl xr) (Node y yl yr) =  (Node (f x y) (overlap f xl yl) (overlap f xr yr))