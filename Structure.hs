module Structure
( OrientedTree (..)
, Orientation (..)
, treeToMusic
, addToGroup
, removeFromGroup
, replaceElement
) where

import Euterpea
import Data.List
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
showTree n (Group H treez) = "\n" ++ pad n ++ "H\n" ++ horiShow ++ "\n"
  where horiShow = concat $ map (showTree (n + 2)) treez
showTree n (Group V treez) = "\n" ++ pad n ++ "V: " ++ vertShow ++ "\n"
  where vertShow = concat $ intersperse "," $ map (showTree 1) treez

instance (Show a) => Show (OrientedTree a) where
  show x = showTree 0 x

type Path = [Int]

data Choice = Some [Int] | All

type Slice = [Choice]

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions
treeToMusic :: MusicTree -> Music Pitch
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H (x:xs)) = foldl series (treeToMusic x) xs
  where series acc x = acc :+: treeToMusic x
treeToMusic (Group V (x:xs)) = foldl parallel (treeToMusic x) xs
  where parallel acc x = acc :=: treeToMusic x

valToMusic :: MusicTree -> Music Pitch
valToMusic (Val x) = Prim (x)

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





-- TODO Make the tree operations return maybe so we can allow failure..
-- TODO create slicing abilities like the prefix boyz have done
