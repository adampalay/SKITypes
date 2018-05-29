module SKIreduce where

data SKI = S | K | I | A | B | Var Int deriving (Show)
data AST = Leaf SKI | Node AST AST deriving (Show)

reduce :: AST -> AST
-- leaves should reduce to themselves
reduce (Leaf x) = Leaf x
-- I pattern matching
reduce (Node (Leaf I) ast) = reduce ast
-- K pattern matching
reduce (Node (Node (Leaf K) ast) _) = reduce ast
reduce (Node (Leaf K) a) = Node (Leaf K) a
-- S pattern matching
reduce (Node (Node (Node (Leaf S) a) b) c) = reduce (Node (Node a c) (Node b c))
reduce (Node (Node (Leaf S) a) b) = Node (Node (Leaf S) a) b
reduce (Node (Leaf S) a) = Node (Leaf S) a

-- if there's another symbol on the left, leave it and reduce the rest of the
-- ast
reduce (Node (Leaf x) ast) = Node (Leaf x) (reduce ast)

-- anything else, reduce the operator
reduce (Node ast1 ast2) =  reduce (Node (reduce ast1) ast2)
