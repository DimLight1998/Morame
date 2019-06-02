module StackMap where

type StackMap a b = [(a, b)]

push :: (a, b) -> StackMap a b -> StackMap a b
push p sm = p : sm

pop :: StackMap a b -> StackMap a b
pop sm = case sm of
    [] -> error "empty stack"
    _ : sm' -> sm'

lookUp :: Eq a => a -> StackMap a b -> Maybe b
lookUp x sm = case sm of
    [] -> Nothing
    (x', y) : sm' -> if x == x' then Just y else lookUp x sm'