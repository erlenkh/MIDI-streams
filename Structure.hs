module Structure
( OrientedTree (..)
, Orientation (..)
, treeToMusic
, getElement
, valToMusic
, addToGroup
, removeFromGroup
) where

import Euterpea

{-
 the grouping structure of a piece, represented as a
 polymorphic tree, with Euterpeas primitives (note or rest) as leaves.
 Each branch is labeled with an orientation H or V corr. to chord or line

 Already exists in euterpea, but re-implemented to allow things like grouping
 by slicing.
-}

data Orientation = H | V deriving (Show)  -- Horizontal | Vertical

data OrientedTree a = Val a | Group Orientation [OrientedTree a]
  deriving (Show)

type Path = [Int]

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions
treeToMusic :: OrientedTree (Primitive Pitch) -> Music Pitch
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H (x:xs)) = foldl series (treeToMusic x) xs
  where series acc x = acc :+: treeToMusic x
treeToMusic (Group V (x:xs)) = foldl parallel (treeToMusic x) xs
  where parallel acc x = acc :=: treeToMusic x

-- get element in tree by integer list
getElement :: OrientedTree a -> Path -> OrientedTree a
getElement (Val a) [x] = error "element does not exist"
getElement tree [] = tree
getElement (Group o elems) (x:xs) = getElement (elems !! x) xs

-- add element to Group in tree
-- input: original tree, tree to add, path to item to add before...
addToGroup :: OrientedTree a -> OrientedTree a -> Path -> OrientedTree a
addToGroup tree element [] = tree
addToGroup (Group o elems) element [idx] = Group o (a ++ [element] ++ b)
  where (a, b) = splitAt idx elems
addToGroup (Group o elems) element (x:xs) = Group o newElems
  where (a,e:b) = splitAt x elems
        newElems = a ++ [addToGroup e element xs] ++ b

-- remove element to Group in tree
-- input: tree, path to element
removeFromGroup :: OrientedTree a -> Path -> OrientedTree a
removeFromGroup tree [] = tree
removeFromGroup (Group o elems) [x] = Group o (a ++ b)
  where (a, e:b) = splitAt x elems
removeFromGroup (Group o elems) (x:xs) = Group o newElems
  where (a, e:b) = splitAt x elems
        newElems = a ++ [removeFromGroup e xs] ++ b

-- convert Val to Euterpeas music type
valToMusic :: OrientedTree (Primitive Pitch) -> Music Pitch
valToMusic (Val x) = Prim (x)

{- syntax of creating primitives:
  prim :: Primitive Pitch
  prim = Note qn (C,4)
  prim = Rest qn
-}

-- TODO create slicing abilities like the prefix boyz have done
