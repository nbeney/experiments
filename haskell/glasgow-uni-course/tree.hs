data Tree = 
    Leaf
  | Node {value :: Int, left :: Tree, right :: Tree}
  deriving (Eq, Show)

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ l r) = 1 + max (treeDepth l) (treeDepth r)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node v l r) = v + (treeSum l) + (treeSum r)

treeIsSorted :: Tree -> Bool
treeIsSorted Leaf = True
treeIsSorted (Node _ Leaf Leaf) = True
treeIsSorted (Node v l Leaf) = v >= treeMax l
treeIsSorted (Node v Leaf r) = v <= treeMin r
treeIsSorted (Node v l r) = v >= treeMax l && v <= treeMin r

treeMax :: Tree -> Int
treeMax Leaf = error "Not applicable!"
treeMax tree = foldl1 max (treeValues tree)

treeMin :: Tree -> Int
treeMin Leaf = error "Not applicable!"
treeMin tree = foldl1 min (treeValues tree)

--treeValues :: Tree -> [Int]
--treeValues Leaf = []
--treeValues (Node v l r) = v : treeValues l ++ treeValues r

treeValues :: Tree -> [Int]
treeValues Leaf = []
treeValues (Node v l r) = v : treeValues l ++ treeValues r

display :: String -> Tree -> IO ()
display name tree = do
  print $ name ++ ": depth = " ++ dep ++ ", sum = " ++ sum' ++ ", sorted = " ++ sor ++ ", values = " ++ val ++ ", " ++ show tree
  where dep = show $ treeDepth tree
        sum' = show $ treeSum tree
        sor = show $ treeIsSorted tree
        val = show $ treeValues tree

main :: IO ()
main = do
  let t1 = Leaf
  let t2 = Node 1 Leaf Leaf
  let t3 = Node 2 (Node 1 Leaf Leaf) Leaf
  let t4 = Node 1 (Node 2 Leaf (Node 4 Leaf Leaf)) (Node 3 Leaf Leaf)
  display "t1" t1
  display "t2" t2
  display "t3" t3
  display "t4" t4
