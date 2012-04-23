{-# LANGUAGE ExistentialQuantification #-}
module Data.CellML12.DisjointSet (applyMerges)
where

import Control.Monad.ST
import Data.STRef
import qualified Data.Map as M
import Control.Monad
  
data Mergeable s a = Mergeable { mergeContents :: a, mergeParent :: STRef s (Int,Maybe (Mergeable s a)) }
instance Eq a => Eq (Mergeable s a) where
  a == b = (mergeContents a) == (mergeContents b)

makeSets :: (Eq a, Ord a) => [a] -> ST s (M.Map a (Mergeable s a))
makeSets l = liftM M.fromList $ forM l $ \v ->
  liftM (\x -> (v, Mergeable v x)) $ newSTRef (0, Nothing)

pathCompress m = do
  (r, mv) <- readSTRef (mergeParent m)
  case mv of
    Nothing -> return m
    Just v -> do
      newp <- pathCompress v
      writeSTRef (mergeParent m) (r, Just newp)
      return newp

mergeTwo :: Eq a => Mergeable s a -> Mergeable s a -> ST s ()
mergeTwo m1 m2 = do
  p1' <- pathCompress m1
  p2' <- pathCompress m2
  (r1, _) <- readSTRef (mergeParent p1')
  (r2, _) <- readSTRef (mergeParent p2')
  case () of
    () | p1' == p2' -> return ()
       | r1 < r2 -> writeSTRef (mergeParent m1) (r1, Just p2')
       | r2 > r1 ->
         writeSTRef (mergeParent m2) (r2, Just p1')
       | otherwise -> do
           writeSTRef (mergeParent m2) (r2, Just p1')
           writeSTRef (mergeParent m1) (r1 + 1, Nothing)

applyMerges :: (Eq a, Ord a) => [a] -> [(a, a)] -> [[a]]
applyMerges s m = runST $ do
  smaps <- makeSets s
  forM m $ \(ma, mb) ->
    case (M.lookup ma smaps, M.lookup mb smaps) of
      (Just a, Just b) -> do
        mergeTwo a b
      _ -> return ()
  let buildMultiset ms e = do
        e' <- pathCompress e
        (_, p) <- readSTRef (mergeParent e')
        let p' = maybe (mergeContents e') mergeContents p
            addOrMerge Nothing = [mergeContents e]
            addOrMerge (Just l) = (mergeContents e):l
        return $ M.alter (Just . addOrMerge) p' ms
  liftM (M.elems) $ foldM buildMultiset M.empty (M.elems smaps)
