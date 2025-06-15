import Data.Tuple
import Data.Array

(>>) :: Array Int -> Array Int -> Array (Tuple Int Int) 
(>>) xs ys 
    | length xs < length ys = zip (cycle xs) ys
    | length xs > length ys = zip xs (cycle ys)
    | otherwise = zip xs ys