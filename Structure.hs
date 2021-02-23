module Structure
( OrientedTree (..)
, Orientation (..)
, treeToMusic
, accessElement
, valToMusic
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

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions
treeToMusic :: OrientedTree (Primitive Pitch) -> Music Pitch
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H (x:xs)) = foldl series (treeToMusic x) xs
  where series acc x = acc :+: treeToMusic x
treeToMusic (Group V (x:xs)) = foldl parallel (treeToMusic x) xs
  where parallel acc x = acc :=: treeToMusic x

-- access element in tree by integer list
accessElement :: OrientedTree a -> [Int] -> OrientedTree a
accessElement (Val a) [x] = error "element does not exist"
accessElement tree [] = tree
accessElement (Group o elems) (x:xs) = accessElement (elems !! x) xs

-- convert Val to Euterpeas music type
valToMusic :: OrientedTree (Primitive Pitch) -> Music Pitch
valToMusic (Val x) = Prim (x)

{- syntax of creating primitives:
  prim :: Primitive Pitch
  prim = Note qn (C,4)
  prim = Rest qn
-}

-- TODO create slicing abilities like the prefix boyz have done
