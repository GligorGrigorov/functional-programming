module TreeOrdering(values,Tree(EmptyTree,Node),Strategy(Inorder,Postorder,Preorder)) where
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)

values :: Strategy -> Tree a -> [a]
values _ EmptyTree = []
values _ (Node a EmptyTree EmptyTree) = [a]
values Inorder (Node a l r) = values Inorder l ++ [a] ++ values Inorder r
values Postorder (Node a l r) = values Postorder l ++ values Postorder r ++ [a]
values Preorder (Node a l r) = [a] ++ values Preorder l ++ values Preorder r