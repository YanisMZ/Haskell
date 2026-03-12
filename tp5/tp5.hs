import qualified Data.List as L

data BSTree a = Node (BSTree a) a (BSTree a) | Empty
                deriving (Eq, Ord)

branchChar :: Char
branchChar = '-'

splitChar :: Char
splitChar = '+'

branchIndent :: Int
branchIndent = 5

branchNil :: Char
branchNil = '⊥'

instance Show a => Show (BSTree a) where
  show = showBSTree 0
    where
      drawBranch 0 = ""
      drawBranch n = L.replicate (n - branchIndent) branchChar ++ [splitChar] ++ L.replicate (branchIndent-1) branchChar

      showBSTree n Empty          = drawBranch n ++ branchNil:"\n"
      showBSTree n (Node lt x rt) = showBSTree (n + branchIndent) rt ++
                                    drawBranch n ++ show x           ++ "\n" ++
                                    showBSTree (n + branchIndent) lt

mkExampleBSTree :: BSTree Integer
mkExampleBSTree = root
  where 
    root = bst6
    bst6 = Node bst4 6 bst8
    bst4 = Node bst2 4 bst5
    bst8 = Node bst7 8 bst9
    bst2 = Node bst1 2 bst3
    bst7 = Node Empty 7 Empty
    bst9 = Node Empty 9 Empty
    bst5 = Node Empty 5 Empty
    bst1 = Node Empty 1 Empty
    bst3 = Node Empty 3 Empty

treeSize :: Num b => BSTree a -> b
treeSize Empty = 0
treeSize (Node lt x rt) = 1 + treeSize lt + treeSize rt

treeLeaves :: Num b => BSTree a -> b
treeLeaves Empty = 0
treeLeaves (Node Empty _ Empty) = 1
treeLeaves (Node lt x rt) = treeLeaves lt + treeLeaves rt


treeHeight :: (Num b, Ord b) => BSTree a -> b
treeHeight Empty = 0
treeHeight (Node lt _ rt) = 1 + max (treeHeight lt) (treeHeight rt)

leavesBSTree :: BSTree a -> [a]
leavesBSTree Empty = []
leavesBSTree (Node Empty x Empty) = [x]
leavesBSTree (Node lt _ rt) = leavesBSTree lt ++ leavesBSTree rt


elemBSTree :: Ord a => a -> BSTree a -> Bool
elemBSTree _ Empty = False
elemBSTree x (Node lt y rt)
  | x == y = True
  | x < y = elemBSTree x lt
  | otherwise = elemBSTree x rt



inOrderVisitBSTree :: BSTree a -> [a]
inOrderVisitBSTree Empty = []
inOrderVisitBSTree (Node lt x rt) = inOrderVisitBSTree lt ++ [x] ++ inOrderVisitBSTree rt



preOrderVisitBSTree :: BSTree a -> [a]
preOrderVisitBSTree Empty = []
preOrderVisitBSTree (Node lt x rt ) = [x] ++ preOrderVisitBSTree lt ++ preOrderVisitBSTree rt 



postOrderVisitBSTree :: BSTree a -> [a]
postOrderVisitBSTree Empty = []
postOrderVisitBSTree (Node lt root rt) = postOrderVisitBSTree lt ++ postOrderVisitBSTree rt ++ [root]




insertBSTree :: (Ord a) => BSTree a -> a -> BSTree a
insertBSTree Empty x = Node Empty x Empty
insertBSTree (Node lt val rt) x
    | x < val   = Node (insertBSTree lt x) val rt
    | x > val   = Node lt val (insertBSTree rt x)
    | otherwise = Node lt val rt 





fromListBSTree :: (Ord a) => [a] -> BSTree a
fromListBSTree = foldr op Empty
  where
      op x t = insertBSTree t x

toListBSTree :: BSTree a -> [a]
toListBSTree Empty = []
toListBSTree a = inOrderVisitBSTree a


mergeBSTree :: Ord a => BSTree a -> BSTree a -> BSTree a
mergeBSTree t1 t2 = foldr (flip insertBSTree) t1 (toListBSTree t2)



leftmostBSTree :: BSTree a -> Maybe a
leftmostBSTree Empty = Nothing
leftmostBSTree (Node Empty val _) = Just val
leftmostBSTree (Node lt _ _) = leftmostBSTree lt



minBSTree :: BSTree a -> Maybe a
minBSTree Empty = Nothing
minBSTree (Node Empty val _) = Just val
minBSTree (Node lt _ _) = minBSTree lt


rightmostBSTree :: BSTree a -> Maybe a
rightmostBSTree Empty = Nothing
rightmostBSTree (Node _ val Empty) = Just val
rightmostBSTree (Node _ _ rt) = rightmostBSTree rt





deleteRootBSTree :: Ord a => BSTree a -> BSTree a
deleteRootBSTree Empty = Empty
deleteRootBSTree (Node Empty _ r) = r
deleteRootBSTree (Node l _ Empty) = l
deleteRootBSTree (Node l _ r) = Node l m (deleteBSTree m r)
  where
    Just m = rightmostBSTree r


deleteBSTree :: Ord a => a -> BSTree a -> BSTree a
deleteBSTree _ Empty = Empty
deleteBSTree x (Node l v r)
  | x < v     = Node (deleteBSTree x l) v r
  | x > v     = Node l v (deleteBSTree x r)
  | otherwise = deleteRootBSTree (Node l v r)