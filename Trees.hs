{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable #-}
module Trees
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
, elevateValues
, childrenAtDepth
) where

import Data.List
import Data.Maybe
import Control.Monad.State

-- ORIENTED TREE ---------------------------------------------------------------

data Orientation = H | V deriving (Show)  -- Horizontal | Vertical

data OrientedTree a = Val a | Group Orientation [OrientedTree a]
 deriving (Foldable, Traversable)

instance Functor (OrientedTree) where
  fmap f (Val a) = Val (f a)
  fmap f (Group o trees) = Group o (map (fmap f) trees)

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

--flattens tree, applies a sequential function, and elevates to original form
applySF :: ([a] -> [a]) -> OrientedTree a -> OrientedTree a
applySF sf tree = elevate (sf $ flatten tree) tree

  -- SIZE FUNCTIONS ------------------------------------------------------------

height :: OrientedTree a -> Int
height (Val a) = 0
height (Group o trees) = 1 + maximum (map height trees)

children :: OrientedTree a -> Int
children (Val a) = 0
children (Group o trees) = length (trees)

childrenAtDepth :: OrientedTree a -> Int -> [Int]
childrenAtDepth tree depth =
    map children (fromJust $ subTrees (replicate depth All) tree)
  -- ^ subTre should never return Nothing,  (paths are from paths function)

-- SLICES ----------------------------------------------------------------------

-- TODO: define slicing in terms of paths?

--at each hierarchical level: select either some Branches or All:
data Choice = Some [Int] | All deriving (Show, Eq)

type Slice = [Choice]

instance Show (Slice -> Slice) where
  show st = show $ st $ smallestDefault [st] -- "ST" --
-- ^ in order to show slice transformation (a function), apply to default slice

-- ---- ---- SLICE TRANSFORMATIONS ---------------------------------------------

-- selection should be a Choice, like in atDepth, problem occurs with getDepth...
-- ideally this should return a maybe but it is a lot of work just for idealism:

atDepth :: Int -> [Int] -> (Slice -> Slice) -- is used by partial application
atDepth depth selection slice =
  let (a, b) = splitAt depth slice
  in a ++ [Some selection] ++ tail b

atDepth' :: Int -> Choice -> (Slice -> Slice) -- is used by partial application
atDepth' lvl choice slice =
  let (first, second) = splitAt lvl slice
  in first ++ [choice] ++ tail second

smallestDefault :: [Slice -> Slice] -> Slice
smallestDefault sts = replicate ((getDeepest sts) + 1) All

getDeepest :: [Slice -> Slice] -> Int
getDeepest sts = maximum $ map getDepth sts

-- a piece cannot have more that 666 hierarchical levels, should be generalized
getDepth :: (Slice -> Slice) -> Int
getDepth sTrans = maximum $ findIndices (isSome) $ sTrans $ replicate (666) All

-- only works with sts that only transform one depth.
getDepth' :: (Slice -> Slice) -> Int
getDepth' st = fromJust . findIndex (isSome) . st $ repeat All

isSome (Some _) = True
isSome _  = False

-- ---- ---- ACCESS ORIENTED TREE BY SLICE -------------------------------------

-- gets a Maybe list of all subtrees in a slice.
subTrees :: Slice -> OrientedTree a -> Maybe [OrientedTree a]
subTrees _ (Val x) = Nothing
subTrees [] tree = Just [tree]
subTrees (c : cs) tree =
  let flatten = fmap concat . join
  in flatten $ (sequence . map (subTrees cs)) <$> (subTrees' c tree)

-- gets a Maybe list of all subtrees in a Choice.
subTrees' :: Choice -> OrientedTree a -> Maybe [OrientedTree a]
subTrees' _ (Val x) = Nothing
subTrees' All (Group _ ts) = Just ts
subTrees' (Some idxs) (Group _ ts) =
  if maximum idxs > length ts then Nothing else Just $ map (ts !!) idxs

type TT a = (OrientedTree a -> OrientedTree a)

-- | slices should not be able to be longer than height of tree
applyTT :: Slice -> TT a -> OrientedTree a -> Maybe (OrientedTree a)
applyTT _ tt (Val x) = Nothing -- Cannot make a choice in a Val (only Groups)
applyTT slice tt tree@(Group o ts) =
  if length slice > height tree
    then Nothing
    else Just $ applyTT' slice tt tree

applyTT' :: Slice -> TT a -> OrientedTree a -> OrientedTree a
applyTT' _ tt (Val x) = tt $ Val x
-- | (can't happen) ^ If Tree is a Val, slicing makes no sense: simply apply tt
applyTT' [c] tt (Group o ts) = Group o $ (applyTo c) tt ts
-- |     ^ if slice is single choice, apply tt to chosen trees
applyTT' (c:cs) tt (Group o ts) = Group o $ (applyTo c) (applyTT' cs tt) ts
-- |     ^ if more choices in slice, recursively continue down tree

applyTo :: Choice -> ( (a -> a) -> [a] -> [a] )
applyTo c = case c of
              All -> map
              Some idxs -> zipSome idxs

zipSome idxs f trees =
   zipWith (\tree idx -> if idx `elem` idxs then f tree else tree) trees [0..]

replaceVal :: a -> a -> a
replaceVal new old = new

-- PRE-FIX TREE ----------------------------------------------------------------

data PrefixTree v k = Leaf k v | Node k [PrefixTree v k] deriving (Show)

newtype PrefixTree' k v = PT' {pt :: PrefixTree v k}

instance Functor (PrefixTree v) where
  fmap f (Leaf k v) = Leaf (f k) v
  fmap f (Node k trees) = Node (f k) (map (fmap f) trees)

instance Functor (PrefixTree' k) where
  fmap f (PT' (Leaf k v)) = PT' $ Leaf k (f v)
  fmap f (PT' (Node k trees)) = PT' $ Node k (map (pt . fmap f . PT') trees)


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

getAllValues :: PrefixTree v k -> [v]
getAllValues (Leaf k v) = [v]
getAllValues (Node k pts) = concat $ map getAllValues pts

keysAmt :: (PrefixTree k v) -> Int
keysAmt (Leaf k v) = 1
keysAmt (Node k trees) = 1 + (sum $ map keysAmt trees)

valuesAmt :: PrefixTree k v -> Int
valuesAmt (Leaf k v) = 1
valuesAmt (Node k trees) =  sum $ map valuesAmt trees

elevateValues :: [v] -> PrefixTree v k -> PrefixTree v k
elevateValues list  = pt . (elevate list) . PT'

-- ELEVATION: ------------------------------------------------------------------

elevate :: (Functor f, Enumerable f) => [a] -> f a -> f a
elevate list = fmap ff . enumerate where
  ff (idx, value) = if idx < length list then list !! idx else value

enumerate :: Functor f => Enumerable f => f a -> f (Int, a)
enumerate = snd . enumerate' 0

class Enumerable e where
  enumerate' :: Int -> e a -> (Int, e (Int, a))

instance Enumerable (OrientedTree) where
  enumerate' n (Val x) = (1, Val (n, x))
  enumerate' n (Group o ts) = (sum sizes, Group o enodes)
    where (sizes, enodes) = enumerateSubTrees n ts

instance Enumerable (PrefixTree v) where
  enumerate' n (Leaf k v) = (1, Leaf (n, k) v)
  enumerate' n (Node k ts) = ((sum sizes) + 1, Node (n, k) enodes)
    where (sizes, enodes) = enumerateSubTrees (n + 1) ts

instance Enumerable (PrefixTree' k) where
  enumerate' n (PT' (Leaf k v)) = (1, PT' $ Leaf k (n,v))
  enumerate' n (PT' (Node k ts)) = (sum sizes, PT' $ Node k (map pt enodes))
    where (sizes, enodes) = enumerateSubTrees n (map PT' ts)

-- general way to enumerate a list of enumeratable nodes:
enumerateSubTrees :: Enumerable e => Int -> [e a] -> ([Int], [e (Int,a)])
enumerateSubTrees startSize (x:xs) =
  let totalsize = (sum . map fst)
      ff prev x = prev ++ [enumerate' (startSize + totalsize prev) x]
  in unzip $ foldl ff [enumerate' startSize x] xs

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
