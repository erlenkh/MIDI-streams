{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable #-}
-- ^ flex instsances so we can define instance show for functions..
module MusicTrees
( MusicOT (..)
, MusicPT(..)
, toMusic
, atPhrases
, atMeasures
, atPeriods
, toMT
, applyPT

)
where

import Structure

import Euterpea
import qualified Data.List as L
import qualified Data.Set as S
import Data.Maybe
import System.Random
import Control.Monad.State

--todo make MusicOT and MusicPT new types..

-- MUSICAL ORIENTED TREE  ------------------------------------------------------

type MusicOT = OrientedTree (Primitive Pitch)

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions

toMusic :: MusicOT -> Music (Pitch, Volume)
toMusic (Val x) = valToMusic (Val x)
toMusic (Group H trees) = line $ map toMusic trees
toMusic (Group V trees) = chord $ map toMusic trees

valToMusic :: MusicOT -> Music (Pitch, Volume)
valToMusic (Val (Note dur p)) = Prim ((Note dur (p, 75)))
valToMusic (Val (Rest dur)) = Prim (Rest dur)

-- SLICE TRANSFORMATIONS -------------------------------------------------------

-- some simple short hands..

atPeriods  = atDepth 0
atPhrases = atDepth 1
atMeasures = atDepth 2
atChords = atDepth 3

-- MUSIC PREFIX-TREE -----------------------------------------------------------

type MusicPT = PrefixTree (MusicOT -> MusicOT) (Slice -> Slice)

 -- Transformative Instruction:
data TI = TI { slc :: Slice, tt :: (MusicOT -> MusicOT)}

toTIs :: MusicPT -> [TI]
toTIs pt =
  let stss = getAllPaths pt
      tts = getAllValues pt
  in zipWith toTI stss tts

toTI :: [Slice -> Slice] -> (MusicOT -> MusicOT) -> TI
toTI sts ttrans =
  let slice = foldl (\slc f -> f slc) (smallestDefault sts) sts
  -- left to right: ^ lower nodes can overwrite higher nodes
  in TI { slc = slice, tt = ttrans}

applyTIs :: [TI] -> MusicOT -> MusicOT
applyTIs tis tree = foldl applyTI tree tis
-- left to right: ^ functions are applied from left to right in Prefix Tree.

applyTI :: MusicOT -> TI -> MusicOT
applyTI tree (TI slice tt) = fromJust $ applyTT slice tt tree
--                              ^ Assumes applyTT works
applyPT :: MusicPT -> MusicOT -> MusicOT
applyPT pt = applyTIs (toTIs pt)

toMT :: MusicPT -> MusicOT
toMT pt = applyPT pt (makeStartingTree (toTIs pt))

-- MODIFIED YAN HAN: -----------------------------------------------------------

-- Used only to infer a skeletal 'OrientedTree' from a musical prefix tree.
data TreeShape =
  TAll TreeShape
  | TSome [TreeShape]
  | TLeaf
  deriving Show

makeStartingTree :: [TI] -> MusicOT
makeStartingTree tis =
  let slices        = map slc tis
      treeStructure = foldr addSlice TLeaf slices
  in  toDefaultOrientedTree treeStructure

toDefaultOrientedTree :: TreeShape -> MusicOT
toDefaultOrientedTree =
  go $ repeat (Group H) -- left is top
 where
  go (c : cs) TLeaf      = Group H [Val $ Rest sn]
  go (c : cs) (TAll  t ) = c [go cs t]
  go (c : cs) (TSome ts) = c . map (go cs) $ ts

extendList :: Int -> a -> [a] -> [a]
extendList n e xs | n <= length xs = xs
                  | otherwise      = xs ++ replicate (n - length xs) e

mapChoice :: [Int] -> (a -> a) -> [a] -> [a]
mapChoice idxs f as =
  zipWith (\a idx -> if idx `elem` idxs then f a else a) as [0 ..]

addSlice :: Slice -> TreeShape -> TreeShape
addSlice []         t          = t
addSlice (All : xs) TLeaf      = TAll (addSlice xs TLeaf)
addSlice (All : xs) (TAll  t ) = TAll (addSlice xs t)
addSlice (All : xs) (TSome ts) = TSome (map (addSlice xs) ts)
addSlice (Some is : xs) TLeaf =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) TLeaf))
addSlice (Some is : xs) (TAll t) =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) t))
addSlice (Some is : xs) (TSome ts) =
  TSome (mapChoice is (addSlice xs) (extendList (maximum is + 1) TLeaf ts))
