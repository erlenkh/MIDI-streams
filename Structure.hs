module Structure
( OrientedTree (..)
, Orientation (..)
, Choice (..)
, Slice (..)
, PrefixTree(..)
, toGroup
, getElements
, getAllPaths
, getAllValues
, applySF
, flatten
, elevate
, depth
, smallestDefault
, atDepth
, applyTT
) where

import Data.List
import Data.Maybe


-- ORIENTED TREE ---------------------------------------------------------------

data Orientation = H | V deriving (Show)  -- Horizontal | Vertical

data OrientedTree a = Val a | Group Orientation [OrientedTree a]

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

depth :: OrientedTree a -> Int
depth (Val a) = 1
depth (Group o trees) = 1 + maximum (map depth trees)

-- PATH FUNCTIONS --------------------------------------------------------------

type Path = [Int]

getElement :: OrientedTree a -> Path -> OrientedTree a
getElement (Val a) [x] = error "element does not exist"
getElement tree [] = tree
getElement (Group o elems) (x:xs) = getElement (elems !! x) xs

-- if theres already an element x on path, e is inserted before x
addToGroup :: OrientedTree a -> OrientedTree a -> Path -> OrientedTree a
addToGroup tree element [] = tree
addToGroup (Group o elems) element [idx] = Group o (a ++ [element] ++ b)
  where (a, b) = splitAt idx elems
addToGroup (Group o elems) element (x:xs) = Group o newElems
  where (a,e:b) = splitAt x elems
        newElems = a ++ [addToGroup e element xs] ++ b

removeFromGroup :: OrientedTree a -> Path -> OrientedTree a
removeFromGroup tree [] = tree
removeFromGroup (Group o elems) [x] = Group o (a ++ b)
  where (a, e:b) = splitAt x elems
removeFromGroup (Group o elems) (x:xs) = Group o newElems
  where (a, e:b) = splitAt x elems
        newElems = a ++ [removeFromGroup e xs] ++ b

replaceElement :: OrientedTree a -> Path -> OrientedTree a -> OrientedTree a
replaceElement tree path newElement = newTree
  where newTree = addToGroup (removeFromGroup tree path) newElement path

addElementVert :: OrientedTree a -> Path -> OrientedTree a -> OrientedTree a
addElementVert tree path element =
  let alreadyThere = getElement tree path
      newE = Group V [alreadyThere, element]
  in replaceElement tree path newE

-- SLICES ----------------------------------------------------------------------

-- TODO: define slicing in terms of paths?

--at each hierarchical level: select either some Branches or All:
data Choice = Some [Int] | All deriving (Show, Eq)

type Slice = [Choice]

-- ---- ---- SLICE CONSTRUCTION ------------------------------------------------

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

-- ---- ---- ACCESS ORIENTED TREE BY SLICE -------------------------------------

extract :: Slice -> OrientedTree a -> OrientedTree a
extract _ (Val x) = Val x
extract ([]) tree = tree
extract (All : slice) (Group o trees) =
   Group o $ map (extract slice) trees
extract (Some  idxs : slice) (Group o trees) =
   Group o $ map (extract slice) (map (trees !!) idxs)

getElements :: Slice -> OrientedTree a -> [OrientedTree a]
getElements [All] (Group _ ts) = ts
getElements [Some idxs] (Group _ ts) = map (ts !!) idxs
getElements (All : slice) (Group _ ts) = concat $ map (getElements slice) ts
getElements (Some idxs : slice) (Group _ ts) =
   concat $ map (getElements slice) (map (ts !!) idxs)

type TreeTransformation a = (OrientedTree a -> OrientedTree a)

-- slices should not be able to be longer than depth of tree - 1:
applyTT :: Slice -> TreeTransformation a -> OrientedTree a -> OrientedTree a
applyTT _ tt (Val x) = tt $ Val x
-- |            ^ If Tree is a Val, slicing makes no sense: simply apply tt
applyTT [c] tt (Group o ts) = Group o $ (handleChoice c) tt ts
-- |     ^ if slice is single choice, apply tt to chosen trees
applyTT (c : cs) tt (Group o ts) = Group o $ (handleChoice c) (applyTT cs tt) ts
-- |     ^ if more choices in slice, recursively continue down tree

-- wack attempt at returning maybe tree:
applyTT' :: Slice -> TreeTransformation a -> OrientedTree a -> Maybe (OrientedTree a)
applyTT' _ tt (Val x) = Nothing
-- |            ^ If Tree is a Val, slicing makes no sense.
applyTT' slice tt tree@(Group o ts) =
  if length slice > (depth tree) - 1 -- -1 since Vals are not sliceable
    then Nothing
    else Just $ Group o $ (handleChoice c) f ts
      where f = case slice of
                  [c] -> tt -- single choice, apply tt to chosen trees
                  (c:cs) -> applyTT' cs tt -- more choices, continue down tree


handleChoice :: Choice -> ( (a -> a) -> [a] -> [a] )
handleChoice c = case c of
                  All -> map
                  Some idxs -> zipSome idxs

zipSome idxs f trees =
   zipWith (\tree idx -> if idx `elem` idxs then f tree else tree) trees [0..]


-- The main difference from Yan Han is that applyTT is applied to TREES and not
-- Events. Thus the slicing cannot work by simply selecting bottom nodes of tree.
-- Any node should be able to be selected. Thus we need to differentation between
-- the case where we only have one choice, and when there are more choices left.
-- when only one: f should be applied. when more left: we should apply (apply TT slice f),
-- and thus go further down the rabbit hole.


replace ::  Slice -> OrientedTree a -> OrientedTree a -> OrientedTree a
replace slice newGroup tree = applyTT slice (replaceVal newGroup) tree

replaceVal :: a -> a -> a
replaceVal new old = new

-- PRE-FIX TREE ----------------------------------------------------------------

data PrefixTree v k = Leaf k v | Node k [PrefixTree v k] deriving (Show)

instance Functor (PrefixTree v) where
  fmap f (Leaf k v) = Leaf (f k) v
  fmap f (Node k trees) = Node (f k) (map (fmap f) trees)

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

depthPT :: PrefixTree v k -> Int
depthPT (Leaf k v) = 1
depthPT (Node k trees) = 1 + maximum (map depthPT trees)

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
-- TODO Make the tree operations return maybe so we can allow failure..
-- TODO address merging trees
