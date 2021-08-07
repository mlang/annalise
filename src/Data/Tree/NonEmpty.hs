{-# LANGUAGE Safe #-}
module Data.Tree.NonEmpty (
  listToForest
, nonEmptyToTree
, breadcrumbs
) where

import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe         (maybeToList)
import           Data.Tree          (Tree (Node), foldTree, unfoldTree)

listToForest :: [a] -> [Tree a]
listToForest = maybeToList . fmap nonEmptyToTree . nonEmpty

nonEmptyToTree :: NonEmpty a -> Tree a
nonEmptyToTree = unfoldTree $ fmap maybeToList . NonEmpty.uncons

breadcrumbs :: Tree a -> Tree (NonEmpty a)
breadcrumbs = foldTree $ \a -> Node (pure a) . (fmap . fmap) (NonEmpty.cons a)
