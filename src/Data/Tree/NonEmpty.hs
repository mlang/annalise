{-# LANGUAGE Safe          #-}
{-# LANGUAGE TypeOperators #-}
module Data.Tree.NonEmpty (
  listToForest
, branch
, breadcrumbs
) where

import           Control.Natural    (type (~>))
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe         (maybeToList)
import           Data.Tree          (Tree (Node), foldTree, unfoldTree)

listToForest :: [a] -> [Tree a]
listToForest = maybeToList . fmap branch . nonEmpty

branch :: NonEmpty ~> Tree
branch = unfoldTree $ fmap maybeToList . NonEmpty.uncons

breadcrumbs :: Tree a -> Tree (NonEmpty a)
breadcrumbs = foldTree $ \a -> Node (pure a) . (fmap . fmap) (NonEmpty.cons a)
