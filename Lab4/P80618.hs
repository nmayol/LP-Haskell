data Queue a = Queue [a] [a] deriving (Show)

c = push 3 (push 2 (push 1 create))
c1 = push 4 (pop (push 3 (push 2 (push 1 create))))
c2 = push 4 (push 3 (push 2 create))
 
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = Queue l1 ([x] ++ l2)

pop :: Queue a -> Queue a
pop (Queue [] l2) = (Queue (reverse (init(l2))) [])
pop (Queue (x:l1) l2) = (Queue l1 l2)

top :: Queue a -> a
top (Queue [] l2) = last l2
top (Queue l1 _) = head l1

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False



instance Eq a => Eq (Queue a) where
    (Queue f1 f2) == (Queue s1 s2) = (f1 ++ reverse f2) == (s1 ++ reverse s2)

