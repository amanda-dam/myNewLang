import Data.Tuple
import Data.Array

(>>) :: Array Int -> Array Int -> Array (Tuple Int Int) 
(x:xs) >> (y:ys) 
    | length (x:xs) < length (y:ys) = zip (cycle (x:xs)) (y:ys)
    | length (x:xs) > length (y:ys) = zip (x:xs) (cycle (y:ys))
    | otherwise = zip (x:xs) (y:ys) 