module Data.Tree.NonEmpty (forestFromList, treeFromNonEmpty, pathTree) where

import           Data.Bifunctor     (second)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe         (maybeToList)
import           Data.Tree          (Tree (Node), foldTree, unfoldTree)

forestFromList :: [a] -> [Tree a]
forestFromList = maybeToList . fmap treeFromNonEmpty . nonEmpty

treeFromNonEmpty :: NonEmpty a -> Tree a
treeFromNonEmpty = unfoldTree $ second maybeToList . NonEmpty.uncons

pathTree :: Tree a -> Tree (NonEmpty a)
pathTree = foldTree $ \a -> Node (pure a) . (fmap . fmap) (NonEmpty.cons a)

