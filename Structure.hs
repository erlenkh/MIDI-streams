module Structure
( OrientedTree (..)
, Orientation (..)
, MusicTree
, Choice (..)
, treeToMusic
, toGroup
, fromGroup
, addToGroup
, removeFromGroup
, replaceElement
, applyFunction
, getElement
, getElements
, extract
) where

import Euterpea
import Data.List
import Control.Applicative

-- MUSICTREE -------------------------------------------------------------------

{-
 the grouping structure of a piece, represented as a
 polymorphic tree, with Euterpeas primitives (note or rest) as leaves.
 Each branch is labeled with an orientation H or V corr. to chord or line

 Similar structures already exists in euterpea, but re-implemented to allow
 things like grouping by slicing.
-}

data Orientation = H | V deriving (Show)  -- Horizontal | Vertical

data OrientedTree a = Val a | Group Orientation [OrientedTree a]

type MusicTree = OrientedTree (Primitive Pitch)

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

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions

treeToMusic :: MusicTree -> Music Pitch
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H trees) = line (map treeToMusic trees)
treeToMusic (Group V trees) = chord (map treeToMusic trees)
{-
treeToMusic :: MusicTree -> Music Pitch
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H (x:xs)) = foldl series (treeToMusic x) xs
  where series acc x = acc :+: treeToMusic x
treeToMusic (Group V (x:xs)) = foldl parallel (treeToMusic x) xs
  where parallel acc x = acc :=: treeToMusic x
-}
valToMusic :: MusicTree -> Music Pitch
valToMusic (Val x) = Prim (x)

toGroup :: Orientation -> [a] -> OrientedTree a
toGroup H prims = Group H (map (\x -> Val x) prims)
toGroup V prims = Group V (map (\x -> Val x) prims)

fromGroup :: OrientedTree a -> [a]
fromGroup (Group o vals) = map (\(Val x) -> x) vals

-- PATH ------------------------------------------------------------------------

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

-- SLICING ---------------------------------------------------------------------

data Choice = Some [Int] | All deriving Show

type Slice = [Choice]
--at each hierarchical level: select either some Branches or ALl

extract :: Slice -> OrientedTree a -> OrientedTree a
extract _ (Val x) = Val x
extract ([]) tree = tree
extract (All : slice) (Group o trees) =
   Group o $ map (extract slice) trees
extract (Some  idxs : slice) (Group o trees) =
   Group o $ map (extract slice) (map (trees !!) idxs)

getElements :: Slice -> OrientedTree a -> [OrientedTree a]
getElements [All] (Group _ trees) = trees
getElements [Some idxs] (Group _ trees) = map (trees !!) idxs
getElements (All : slice) (Group _ trees) = concat $ map (getElements slice) trees
getElements (Some idxs : slice) (Group _ trees) =
   concat $ map (getElements slice) (map (trees !!) idxs)

--applies function to every element in slice
applyFunction :: (a -> a) -> Slice -> OrientedTree a -> OrientedTree a
applyFunction f _ (Val x) = Val (f x)
applyFunction f (All : slice) (Group o trees) =
  Group o $ map (applyFunction f slice) trees
applyFunction f (Some idxs : slice) (Group o trees) =
  Group o $ zipWith zf trees [0..] where
    zf tree idx = if idx `elem` idxs then applyFunction f slice tree else tree


replace ::  Slice -> OrientedTree a -> OrientedTree a -> OrientedTree a
replace [All] e (Group o trees) =  Group o (replicate (length trees) e)
replace (All : slice) e (Group o trees) = Group o (map (replace slice e) trees)
replace [Some idxs] e (Group o trees) = Group o $ zipWith zf trees [0..] where
  zf tree idx = if idx `elem` idxs then e else tree
replace (Some idxs : slice) e (Group o trees) =
  Group o $ zipWith zf trees [0..] where
  zf tree idx = if idx `elem` idxs then replace slice e tree else tree

replaceVal :: a -> a -> a
replaceVal new old = new


-- slice construction: allows the composition of (Slice -> Slice)
-- examples that apply to "testTree": (need to be generalized)
-- should they add? i.e. atVoices[0,1] . atVoices[2] = atVoices [0,1,2]?
-- right now atVoices[0,1] . atVoices[2] = atVoices [0,1]
atMotifs, atChords, atVoices :: [Int] -> Slice -> Slice
atMotifs selection [_, chords, voices] = [Some selection, chords, voices]
atChords selection [motifs, _ , voices] = [motifs, Some selection, voices]
atVoices selection [motifs, chords, _] = [motifs, chords, Some selection]


-- PRE-FIX TREE ----------------------------------------------------------------

data PrefixTree k v = Leaf k v | Node k [PrefixTree k v] deriving (Show)

type MusicPT =
   PrefixTree (Slice -> Slice) (MusicTree -> MusicTree)

lookupPT :: (Eq k, Eq v) => [k] -> PrefixTree k v -> Maybe v
lookupPT [] _ = Nothing
lookupPT [x] (Leaf k v)  = if x == k then Just v else Nothing
lookupPT (x:xs) (Leaf k v) = Nothing
lookupPT (x:xs) (Node k ptrees) =  if k == x then check else Nothing
  where check = case (find (\pt -> lookupPT xs pt /= Nothing) ptrees) of
                  Just tree -> lookupPT xs tree
                  Nothing -> Nothing

-- TESTING ---------------------------------------------------------------------

testPT :: PrefixTree Char Int
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


testPtree :: MusicPT
testPtree =  Leaf (atMotifs [0,1]) (replaceVal testOTree)

testOTree :: OrientedTree (Primitive Pitch)
testOTree =
              Group H [
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ]
                ]

-- TODO make Transformations that act on group level work...
-- TODO Make the tree operations return maybe so we can allow failure..
