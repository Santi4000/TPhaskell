module Stack where

class Stack s where
    empty :: s a
    push :: a -> s a -> s a
    top :: s a -> a
    pop :: s a -> s a
    isEmpty :: s a -> Bool

data Pila a = PTop a (Pila a) | PEmpty
    deriving (Show, Eq)

instance Stack Pila where
    empty = PEmpty
    push a p = PTop a p
    top (PTop a _) = a
    top PEmpty = PEmpty
    pop (PTop _ p) = p
    isEmpty PEmpty = True
    isEmpty _ = False
