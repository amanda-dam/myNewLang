module Pythagoras where

import Data.List (List, range, filter)
import Data.Int (pow)
import Prelude

sourceList :: List Int
sourceList = range 1 100

data Triple = Triple
  { a :: Int
  , b :: Int
  , c :: Int
  }

allTriples :: List Triple
allTriples = do
  a <- sourceList
  b <- sourceList
  c <- sourceList
  pure $ Triple {a: a, b: b, c: c}

isPythagorean :: Triple -> Boolean
isPythagorean (Triple triple) =
  (pow triple.a 2) + (pow triple.b 2) == (pow triple.c 2)

isSmallEnough :: Triple -> Boolean
isSmallEnough (Triple triple) =
  (triple.a) + (triple.b) + (triple.c) < 100

finalAnswer :: List Triple
finalAnswer = filter
  (\triple -> isPythagorean triple && isSmallEnough triple) 
  allTriples