module Structure
( OrientedTree (..)
, Orientation (..)
, Choice (..)
, Slice (..)
, PrefixTree(..)
, toGroup
, applyGT
, applyFunction
, getElements
, getAllPaths
, getAllValues
, applySF
, flatten
, elevate
) where

import Data.List


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

-- SLICE FUNCTIONS -------------------------------------------------------------

-- TODO: define slicing in terms of paths?

--at each hierarchical level: select either some Branches or All:
data Choice = Some [Int] | All deriving (Show, Eq)

type Slice = [Choice]

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

--applies function to every element in slice
applyFunction :: (a -> a) -> Slice -> OrientedTree a -> OrientedTree a
applyFunction f _ (Val x) = Val (f x)
applyFunction f (All : slice) (Group o trees) =
  Group o $ map (applyFunction f slice) trees
applyFunction f (Some idxs : slice) (Group o trees) =
  Group o $ zipWith zf trees [0..] where
    zf tree idx = if idx `elem` idxs then applyFunction f slice tree else tree

--applies group transformation to the groups in slice
applyGT ::
 Slice -> (OrientedTree a -> OrientedTree a) -> OrientedTree a -> OrientedTree a
applyGT [All] f (Group o vals) = Group o (map f vals)
applyGT (All : slice) f (Group o ts) = Group o (map (applyGT slice f) ts)
applyGT [Some idxs] f (Group o ts) = Group o $ zipWith zf ts [0..]
  where zf tree idx = if idx `elem` idxs then f tree else tree
applyGT (Some idxs : slice) f (Group o ts) = Group o $ zipWith zf ts [0..]
  where zf tree idx = if idx `elem` idxs then applyGT slice f tree else tree

replace ::  Slice -> OrientedTree a -> OrientedTree a -> OrientedTree a
replace slice newGroup tree = applyGT slice (replaceVal newGroup) tree

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

isJust (Just x) = True
isJust (Nothing) = False

getAllPaths :: PrefixTree v k -> [[k]]
getAllPaths (Leaf k v) = [[k]]
getAllPaths (Node k trees) =
  concat [map (k:) (getAllPaths t) | (t) <- trees]


getAllValues :: (Eq k) => PrefixTree v k -> [v]
getAllValues tree =
  let keys = getAllPaths tree
      maybeValues = map (lookupPT tree) keys
      values = map (\(Just x) -> x) maybeValues
  in values

depth :: PrefixTree v k -> Int
depth (Leaf k v) = 1
depth (Node k trees) = 1 + maximum (map depth trees)

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

xtract = extract [Some[2]] testMT


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
