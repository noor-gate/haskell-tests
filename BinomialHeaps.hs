import Data.List

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Show)

instance (Eq a) => Eq (BinTree a) where
  Node x _ _ == Node y _ _ = x == y
instance (Ord a) => Ord (BinTree a) where
  Node x _ _ <= Node y _ _ = x <= y

--------------------------------------------------------------
-- PART I

key :: BinTree a -> a
key (Node n _ (_))
  = n

rank :: BinTree a -> Int
rank (Node _ n (_))
  = n

children :: BinTree a -> [BinTree a]
children (Node _ _ (n))
  = n

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees b1 b2
  | key b1 < key b2 = (Node (key b1) (rank b1 + 1) (b2 : children b1))
  | otherwise = (Node (key b2) (rank b2 + 1) (b1 : children b2))

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin
  = key . minimum

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h []
  = h
mergeHeaps [] h'
  = h'
mergeHeaps h@(t:ts) h'@(t':ts')
  | rank t < rank t' = t : mergeHeaps ts h'
  | rank t' < rank t = t' : mergeHeaps ts' h
  | otherwise = mergeHeaps (mergeHeaps ts ts') [combineTrees t t']


insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x
  = mergeHeaps [Node x 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps (reverse $ children t) (delete t h)
    where
      t = minimum h

binSort :: Ord a => [a] -> [a]
binSort [] = []
binSort bs
  = minVal : binSort (delete minVal bs)
    where
      heap (b:bs)
        = Node b 0 [] : heap bs
      heap [] = []
      minVal = extractMin $ heap bs

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary bs
  =

binarySum :: [Int] -> [Int] -> [Int]
binarySum
  = undefined

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []],
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]
