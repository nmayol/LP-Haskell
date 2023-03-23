
import Control.Monad

data Queue a = Queue [a] [a] deriving (Show)

c = push 3 (push 2 (push 1 create))


cua = Queue [2,8,5] [4,7]

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


instance Functor Queue where
    fmap f (Queue [] []) = (Queue [] [])
    fmap f (Queue x y) = Queue (fmap f x) (fmap f y)


translation :: Num b => b -> Queue b -> Queue b
translation b = fmap (+b)

instance Applicative Queue
    where
        (Queue [x] []) <*> q            = (Queue [] [x]) <*> q
        (Queue [] [x]) <*> q            = (Queue [x] []) <*> q
        pure x                          = (Queue [x] [])

union:: Queue a -> Queue a -> Queue a
union (Queue x y) (Queue z t) = (Queue (x ++ reverse z) (y ++ reverse t))

instance Monad Queue where
    return x = Queue [x] []
    (Queue x y) >>= f =  foldl union create (map f (x ++ (reverse y)))  

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f x = do
    v <- x
    if f v then return v else create
