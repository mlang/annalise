{-# LANGUAGE Safe          #-}
{-# LANGUAGE TypeOperators #-}
module Data.Tree.NonEmpty ( listToForest, branch, breadcrumbs ) where

import           Control.Natural    (type (~>))
import           Data.List.NonEmpty (NonEmpty, cons, nonEmpty, singleton, uncons)
import           Data.Maybe         (maybeToList)
import           Data.Tree          (Tree (Node), foldTree, unfoldTree)

listToForest :: [a] -> [Tree a]
listToForest = maybeToList . fmap branch . nonEmpty

branch :: NonEmpty ~> Tree
branch = unfoldTree $ fmap maybeToList . uncons

breadcrumbs :: Tree a -> Tree (NonEmpty a)
breadcrumbs = foldTree $ \a -> Node (singleton a) . (fmap . fmap) (cons a)
