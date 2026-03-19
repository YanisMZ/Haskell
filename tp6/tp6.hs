import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T 

data MTree a = MTree {rootLabel :: a, subForest :: MForest a} 
               deriving (Eq, Ord)

type MForest a = [MTree a]

mTreeIndent :: Int
mTreeIndent = 4

mTreeBranchChar :: Char
mTreeBranchChar = '.'

mTreeNodeChar :: Char
mTreeNodeChar = '+'

instance (Show a) => Show (MTree a) where
  show = go 0
    where
      go nTabs MTree { rootLabel = rl, subForest = mts } =
          L.replicate nTabs mTreeBranchChar ++
          (if nTabs > 0 then " " else "")   ++
          mTreeNodeChar:" root label="      ++ 
          show rl                           ++
          "\n"                              ++
          F.foldr f "" mts
        where
          f mt acc = go (nTabs + mTreeIndent) mt ++ acc

mTreeMk :: a -> [MTree a] -> MTree a
mTreeMk rl mts = MTree { rootLabel = rl, subForest = mts} 

mTreeMkLeaf :: a -> MTree a
mTreeMkLeaf rl = mTreeMk rl []

mTreeMkExample :: MTree Integer 
mTreeMkExample = mTreeMk 6 [t1, t2, t3] where
  t1 = mTreeMk 4 [t4,t5]
  t2 = (mTreeMk 2 [(mTreeMk 3 [mTreeMkLeaf 2])])
  t3 = (mTreeMk 8 [t6,(mTreeMkLeaf 9),t7])
  t4 = (mTreeMk 2 [(mTreeMkLeaf 1),(mTreeMkLeaf 7),(mTreeMkLeaf 3)])
  t5 = (mTreeMk 5 [(mTreeMkLeaf 3),(mTreeMkLeaf 9)])
  t6 = (mTreeMk 7 [(mTreeMkLeaf 8)])
  t7 = (mTreeMk 2 [(mTreeMkLeaf 1),(mTreeMkLeaf 2),(mTreeMkLeaf 9),(mTreeMkLeaf 7),(mTreeMkLeaf 3)])


mTreeCount :: Num b => MTree a -> b
mTreeCount (MTree _ subForest) = 1 + sum (map mTreeCount subForest)

mTreeIsLeaf :: MTree a -> Bool
mTreeIsLeaf (MTree _ []) = True
mTreeIsLeaf _ = False

mTreeLeaves :: MTree a -> [a]
mTreeLeaves (MTree label []) = [label]
mTreeLeaves (MTree _ subForest) = concatMap mTreeLeaves subForest

mTreeCountLeaves :: Num b => MTree a -> b
mTreeCountLeaves = fromIntegral . length . mTreeLeaves

mTreeSum :: Num a => MTree a -> a
mTreeSum (MTree label subForest) = label + sum (map mTreeSum subForest)

mTreeToList :: MTree a -> [a]
mTreeToList (MTree label subForest) = label : concatMap mTreeToList subForest

mTreeHeight :: (Num b, Ord b) => MTree a -> b
mTreeHeight (MTree _ []) = 1
mTreeHeight (MTree _ subForest) = 1 + maximum (map mTreeHeight subForest)

mTreeElem :: Eq a => a -> MTree a -> Bool
mTreeElem x (MTree label subForest) = x == label || any (mTreeElem x) subForest

mTreeMin :: Ord a => MTree a -> a
mTreeMin (MTree label subForest) = minimum (label : map mTreeMin subForest)

mTreeMax :: Ord a => MTree a -> a
mTreeMax (MTree label subForest) = maximum (label : map mTreeMax subForest)


mForestBreadthFirstTraversal :: [MTree a] -> [a]
mForestBreadthFirstTraversal [] = []
mForestBreadthFirstTraversal mts = map rootLabel mts ++ mForestBreadthFirstTraversal (concatMap subForest mts)

mTreeBreadthFirstTraversal :: MTree a -> [a]
mTreeBreadthFirstTraversal (MTree label subForest) = label : mForestBreadthFirstTraversal subForest


mTreeLayer :: Int -> MTree a -> [a]
mTreeLayer 0 _ = []
mTreeLayer 1 MTree {rootLabel = r ,subForest = mts} = [r]
mTreeLayer n MTree{rootLabel = r ,subForest = mts} = concatMap (mTreeLayer (n - 1)) mts



mTreeMap :: (a -> b) -> MTree a -> MTree b
mTreeMap f MTree{rootLabel = r ,subForest = mts} = MTree (f r) (map (mTreeMap f) mts)


mTreeFilter :: (a -> Bool) -> MTree a -> MForest a
mTreeFilter f MTree{rootLabel = r, subForest = mts}
    | f r       = [MTree r (concatMap (mTreeFilter f) mts)]
    | otherwise = concatMap (mTreeFilter f) mts



mTreeFilter' :: (a -> Bool) -> MTree a -> MForest a
mTreeFilter' f MTree{rootLabel = r, subForest = mts}
    | p r       = MTree{rootLabel = r , subForest = mts'}
    | otherwise = mts'
    where
        mts' = concatMap(mTreeFilter' p) mts


mTreeFold1 :: (a -> b -> b) -> b -> MTree a -> [b]
mTreeFold1 f z MTree { rootLabel = rl, subForest = mts }
| L.null mts = [f rl z]
| otherwise = [f rl y | mt <- mts, y <- mTreeFold1 f z mt]

mTreeCollectPaths :: MTree a -> [[a]]
mTreeCollectPaths = mTreeFold1 (:) []



mTreeSignature :: Num a => MTree a -> [a]
mTreeSignature = mTreeFold1 (+) 0