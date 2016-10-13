data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftT rightT) =
  1 + max (treeDepth leftT) (treeDepth rightT)
