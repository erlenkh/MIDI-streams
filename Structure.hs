{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable #-}

module Structure
( OrientedTree (..)
, Orientation (..)
, Choice (..)
, Slice (..)
, PrefixTree(..)
, toGroup
, getAllPaths
, getAllValues
, applySF
, flatten
, elevate
, height
, smallestDefault
, atDepth
, applyTT
, keysAmt
, valuesAmt
, elevateKeys
, elevateValues
, randomDFSTs
) where

import Data.List
import Data.Maybe
import qualified Random as R
import System.Random
import Control.Monad.State

-- ORIENTED TREE ---------------------------------------------------------------

data Orientation = H | V deriving (Show)  -- Horizontal | Vertical

data OrientedTree a = Val a | Group Orientation [OrientedTree a]
 deriving (Foldable, Traversable)

instance Functor (OrientedTree) where
  fmap f (Val a) = Val (f a)
  fmap f (Group o trees) = Group o (map (fmap f) trees)

instance Applicative OrientedTree where
   pure = Val
   Val f <*> Val x = Val (f x)
   Val f <*> Group o xs = Group o (map (fmap f) xs)
   (Group o fs) <*> (Val x) = Group o (map (fmap ($ x)) fs)
   Group o fs <*> Group ox xs = Group o $ (map (<*> (Group ox xs)) fs)

instance Monad OrientedTree where
   return = Val
   Val a >>= f = f a
   Group o trees >>= f = Group o $ map (>>= f) trees

pad :: Int -> String
pad 0 = ""
pad n = " " ++ pad (n-1)

showTree :: (Show a) => Int -> OrientedTree a -> String
showTree n (Val x) = pad n ++ show x
showTree n (Group H treez) = pad n ++ "H\n" ++ horiShow
  where horiShow = concat $ map (\t -> showTree (n + 2)t ++ "\n") treez
showTree n (Group V treez) = "\n" ++ pad n ++ "V: " ++ vertShow ++ "\n"
  where vertShow = concat $ intersperse " " $ map (showTree 1) treez

instance (Show a) => Show (OrientedTree a) where
  show x = "\n" ++ showTree 0 x

toGroup :: Orientation -> [a] -> OrientedTree a
toGroup H prims = Group H (map (\x -> Val x) prims)
toGroup V prims = Group V (map (\x -> Val x) prims)

flatten :: OrientedTree a -> [a]
flatten (Val x) = [x]
flatten (Group _ vals) = concat $ map flatten vals

elevate :: [a] -> OrientedTree a -> OrientedTree a
elevate flat tree = fmap ff $ enumerate tree where
  ff (idx, value) = if idx < length flat then flat !! idx else value

--flattens tree, applies a sequential function, and elevates to original form
applySF :: ([a] -> [a]) -> OrientedTree a -> OrientedTree a
applySF sf tree = elevate (sf $ flatten tree) tree

-- enumerates each Val from left to right
enumerate :: OrientedTree a -> OrientedTree (Int, a)
enumerate = snd . enumerate' 0

-- maybe make this only enumerate what is in the slice?
enumerate' :: Int -> OrientedTree a -> (Int, OrientedTree (Int, a))
enumerate' num (Val x) = (1, Val (num, x))
enumerate' num (Group o (x:xs)) = (size numGroups, Group o numTrees) where
  numGroups = foldl ff [(enumerate' num x)] xs
  ff prevGroups x = prevGroups ++ [enumerate' (num + size prevGroups) x]
  size = sum . map fst
  numTrees = map snd numGroups


  -- SIZE FUNCTIONS ------------------------------------------------------------

height :: OrientedTree a -> Int
height (Val a) = 0
height (Group o trees) = 1 + maximum (map height trees)

children :: OrientedTree a -> Int
children (Val a) = 0
children (Group o trees) = length (trees)

-- range of depth levels in tree:
depthRange :: OrientedTree a -> [Int]
depthRange tree = [0 .. (height tree) - 2] --  -1 bc of 0-index

-- range of children-indexes in tree at a given depth:
childRange :: OrientedTree a -> Int -> [Int]
childRange tree depth = [0 .. (minimum $ childrenAtDepth tree depth) - 1] -- 0-index
-- ^ min due to slicing, (+ the amt of children at a depth is mostly constant)

childrenAtDepth :: OrientedTree a -> Int -> [Int]
childrenAtDepth tree depth =
    map children (fromJust $ subTrees (replicate depth All) tree)
--  map (children . fromJust . getSubTree tree) (paths depth tree)
  -- ^ getElement should never return Nothing,  (paths are from paths function)

randomDepths :: StdGen -> Int -> [Int] -> ([Int], StdGen)
randomDepths gen n range = runState (R.getRandoms n range) gen
-- | ^ gets n random depths from a given range

randomChildren :: StdGen -> [Int] -> OrientedTree a -> ([Int], StdGen)
randomChildren gen depths ot =
  runState (sequence $ map (R.randomSt . childRange ot) depths) gen
  -- | ^ gets a random child (within the ot) for each depth given as input.

-- PATH FUNCTIONS --------------------------------------------------------------

type Path = [Int]

-- paths to all elements at depth d:
paths :: Int -> OrientedTree a -> [Path]
paths d tree = nub $ map (take d) (allPaths tree)
-- ^ depth of a node is (length of path) - 1

allPaths :: OrientedTree a -> [Path]
allPaths (Val x) = [[]]
allPaths (Group o trees) =
  concat [map (c:) (allPaths t) | (c,t) <- zip [0 .. ] trees]

getSubTree :: OrientedTree a -> Path -> Maybe (OrientedTree a)
getSubTree (Val a) [x] = Nothing
getSubTree tree [] = Just tree
getSubTree (Group o trees) (x:xs) =
  if x > (length trees) - 1 then Nothing else getSubTree (trees !! x) xs

-- SLICES ----------------------------------------------------------------------

-- TODO: define slicing in terms of paths?

--at each hierarchical level: select either some Branches or All:
data Choice = Some [Int] | All deriving (Show, Eq)

type Slice = [Choice]

instance Show (Slice -> Slice) where
  show st =   show $ st $ smallestDefault [st] -- "ST" --
-- ^ in order to show slice transformation (a function), apply to default slice

-- ---- ---- SLICE TRANSFORMATIONS ---------------------------------------------

--crashes if lvl >= length slice (might fix with Maybe)
atLevel :: Int -> [Int] -> (Slice -> Slice)
atLevel lvl selection slice =
  let (first, second) = splitAt lvl (reverse slice)
  in reverse $ first ++ [Some selection] ++ tail second

-- selection should be a Choice, like in atDepth, problem occurs with getDepth...
-- ideally this should return a maybe but it is a lot of work just for idealism:
atDepth :: Int -> [Int] -> (Slice -> Slice) -- is used by partial application
atDepth lvl selection slice =
  let (first, second) = splitAt lvl slice
  in first ++ [Some selection] ++ tail second

atDepth' :: Int -> Choice -> (Slice -> Slice) -- is used by partial application
atDepth' lvl choice slice =
  let (first, second) = splitAt lvl slice
  in first ++ [choice] ++ tail second

smallestDefault :: [Slice -> Slice] -> Slice
smallestDefault sts = replicate ((getMaxDepth sts) + 1) All

getMaxDepth :: [Slice -> Slice] -> Int
getMaxDepth sts = maximum $ map getDepth sts

-- a piece cannot have more that 666 hierarchical levels, should be generalized
getDepth :: (Slice -> Slice) -> Int
getDepth sTrans = maximum $ findIndices (isSome) $ sTrans $ replicate (666) All

isSome (Some xs) = True
isSome _  = False

-- ---- ---- RANDOM SLICE TRANSFORMATIONS: -------------------------------------

-- problem: some prefixtrees dont adress all levels, and thus might result
-- in a new musicOT with a non-constant depth. This is bad!

-- random depth-fixed slice transformations:
-- results in a pt where all TT are applied at the same given depth.
randomDFSTs :: StdGen -> Int -> OrientedTree a -> Int -> ([Slice -> Slice], StdGen)
randomDFSTs gen n ot depth =
  let dfst = randomDFST gen ot depth
      sts = randomSTs' gen (n - 1) ot [0..depth]
  in (fst dfst : fst sts, snd sts)

-- random depth-fixed slice transformation:
randomDFST :: StdGen -> OrientedTree a -> Int -> (Slice -> Slice, StdGen)
randomDFST gen tree depth =
  let ([child], gen2) = randomChildren gen [depth] tree
  in (atDepth depth [0..child], gen2)


randomSTs :: StdGen -> Int -> OrientedTree a -> ([Slice -> Slice], StdGen)
randomSTs gen n ot = randomSTs' gen n ot (depthRange ot)

randomSTs' :: StdGen -> Int -> OrientedTree a -> [Int] -> ([Slice -> Slice], StdGen)
randomSTs' gen n ot depthRange =
  let (depths, gen2) = randomDepths gen n depthRange
      (children, gen3) = randomChildren gen depths ot
  in (zipWith (\d w -> atDepth d [0..w]) depths children, gen3)
  -- | gets n random slice transformations based on shape of oriented tree.

-- ---- ---- ACCESS ORIENTED TREE BY SLICE -------------------------------------

-- gets a Maybe list of all subtrees in a slice.
subTrees :: Slice -> OrientedTree a -> Maybe [OrientedTree a]
subTrees _ (Val x) = Nothing
subTrees [] tree = Just [tree]
subTrees (c : cs) tree@(Group _ ts) =
  join $ (fmap concat . sequence . map (subTrees cs)) <$> (subTrees' c tree)

-- gets a Maybe list of all subtrees in a Choice.
subTrees' :: Choice -> OrientedTree a -> Maybe [OrientedTree a]
subTrees' _ (Val x) = Nothing
subTrees' All (Group _ ts) = Just ts
subTrees' (Some idxs) (Group _ ts) =
  if maximum idxs > length ts then Nothing else Just $ map (ts !!) idxs

type TreeTransformation a = (OrientedTree a -> OrientedTree a)

-- | slices should not be able to be longer than depth of tree - 1:
applyTT :: Slice -> TreeTransformation a -> OrientedTree a -> Maybe (OrientedTree a)
applyTT _ tt (Val x) = Nothing -- Cannot make a choice in a Val (only Groups)
applyTT slice tt tree@(Group o ts) =
  if length slice > height tree
    then Nothing
    else Just $ applyTT' slice tt tree

applyTT' :: Slice -> TreeTransformation a -> OrientedTree a -> OrientedTree a
applyTT' _ tt (Val x) = tt $ Val x
-- | (can't happen) ^ If Tree is a Val, slicing makes no sense: simply apply tt
applyTT' [c] tt (Group o ts) = Group o $ (handleChoice c) tt ts
-- |     ^ if slice is single choice, apply tt to chosen trees
applyTT' (c:cs) tt (Group o ts) = Group o $ (handleChoice c) (applyTT' cs tt) ts
-- |     ^ if more choices in slice, recursively continue down tree

handleChoice :: Choice -> ( (a -> a) -> [a] -> [a] )
handleChoice c = case c of
                  All -> map
                  Some idxs -> zipSome idxs

zipSome idxs f trees =
   zipWith (\tree idx -> if idx `elem` idxs then f tree else tree) trees [0..]

replaceVal :: a -> a -> a
replaceVal new old = new

-- PRE-FIX TREE ----------------------------------------------------------------

data PrefixTree v k = Leaf k v | Node k [PrefixTree v k] deriving (Show)

instance Functor (PrefixTree v) where
  fmap f (Leaf k v) = Leaf (f k) v
  fmap f (Node k trees) = Node (f k) (map (fmap f) trees)

valMap :: (v -> w) -> PrefixTree v k -> PrefixTree w k
valMap f (Leaf k v) = Leaf k (f v)
valMap f (Node k trees) = Node k (map (valMap f) trees)

lookupPT :: (Eq k) => PrefixTree v k -> [k] ->  Maybe v
lookupPT  _ [] = Nothing
lookupPT (Leaf k v) [x] = if x == k then Just v else Nothing
lookupPT (Leaf k v) (x:xs) = Nothing
lookupPT (Node k ptrees) (x:xs) =  if k == x then check else Nothing
  where check = case (find (\pt -> isJust $ lookupPT pt xs) ptrees) of
                  Just tree -> lookupPT tree xs
                  Nothing -> Nothing

getAllPaths :: PrefixTree v k -> [[k]]
getAllPaths (Leaf k v) = [[k]]
getAllPaths (Node k trees) =
  concat [map (k:) (getAllPaths t) | (t) <- trees]

getAllValues :: (Eq k) => PrefixTree v k -> [v]
getAllValues tree =
  let keys = getAllPaths tree
  in  map fromJust $ map (lookupPT tree) keys
  --  ^ should never be Nothing, since it only looks up paths from getallPaths

keysAmt :: (PrefixTree k v) -> Int
keysAmt (Leaf k v) = 1
keysAmt (Node k trees) = 1 + (sum $ map keysAmt trees)

valuesAmt :: PrefixTree k v -> Int
valuesAmt (Leaf k v) = 1
valuesAmt (Node k trees) =  sum $ map valuesAmt trees

elevateKeys :: [k] -> PrefixTree v k -> PrefixTree v k
elevateKeys flat tree = fmap ff $ enumerateKeys tree where
  ff (idx, value) = if idx < length flat then flat !! idx else value

-- enumerates each Val from left to right
enumerateKeys :: PrefixTree v k -> PrefixTree v (Int, k)
enumerateKeys = snd . enumerateKeys' 0

-- maybe make this only enumerate what is in the slice?
enumerateKeys' :: Int -> PrefixTree v k -> (Int, PrefixTree v (Int, k))
enumerateKeys' num (Leaf k v) = (1, Leaf (num, k) v)
enumerateKeys' num (Node k (x:xs)) = (size numNodes + 1, Node (num, k) numTrees) where
  nextNum = num + 1
  numNodes = foldl ff [(enumerateKeys' nextNum x)] xs
  ff prevGroups x = prevGroups ++ [enumerateKeys' (nextNum + size prevGroups) x]
  size = sum . map fst
  numTrees = map snd numNodes

elevateValues :: [v] -> PrefixTree v k -> PrefixTree v k
elevateValues flat tree = valMap ff $ enumerateValues tree where
  ff (idx, value) = if idx < length flat then flat !! idx else value

enumerateValues :: PrefixTree v k -> PrefixTree (Int, v) k
enumerateValues = snd . enumerateValues' 0

enumerateValues' :: Int -> PrefixTree v k -> (Int, PrefixTree (Int, v) k)
enumerateValues' num (Leaf k v) = (1, Leaf k (num,v))
enumerateValues' num (Node k (x:xs)) = (size numNodes, Node k numTrees) where
  nextNum = num
  numNodes = foldl ff [(enumerateValues' nextNum x)] xs
  ff prevGroups x = prevGroups ++ [enumerateValues' (nextNum + size prevGroups) x]
  size = sum . map fst
  numTrees = map snd numNodes


-- TESTING ---------------------------------------------------------------------

testMT :: OrientedTree Char
testMT =     Group H [
                Group V [
                  Val 'C',
                  Val 'A',
                  Val 'T'
                ],
                Group V [
                  Val 'D',
                  Val 'O',
                  Val 'G'
                ],
                Group H [
                  Group H [
                    Val 'K',
                    Val 'I',
                    Val 'L'
                  ],
                  Group H [
                    Val 'L',
                    Val 'E',
                    Val 'R'
                  ]
                ]
              ]

testOT :: OrientedTree Int
testOT = Group H [Group V [Val 1, Val 2, Val 3], Group V [Val 4, Val 5]]


testPT :: PrefixTree Int Char
testPT = Node 'C' [
          Node 'A' [
            Leaf 'T' 1,
            Leaf 'R' 2
          ],
          Node 'O' [
            Leaf 'P' 3,
            Node 'O' [
              Leaf 'L' 4
            ]
          ]
         ]


-- TODO allow the operation on sequences of notes that are not in the same group
-- TODO address merging trees
