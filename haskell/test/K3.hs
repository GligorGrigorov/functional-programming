module K3 where

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

--1
treeWords :: Tree Char -> [String]
treeWords EmptyTree = []
treeWords (Node value EmptyTree EmptyTree) = [[value]]
treeWords (Node value EmptyTree right) = map (\a -> value:a) (treeWords right)
treeWords (Node value  left EmptyTree) = map (\a -> value:a) (treeWords left)
treeWords (Node value left right) = map (\a -> value:a) (treeWords left ++ treeWords right)

--2
mapsTo :: Integral t => (t -> t) -> t -> t -> (t,t)
mapsTo f a b = (minimum ys, maximum ys)
  where 
    xs = [a..b]
    ys = [ f x | x <- xs]


