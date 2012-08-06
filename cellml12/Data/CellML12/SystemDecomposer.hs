{-# LANGUAGE PatternGuards #-}
module Data.CellML12.SystemDecomposer where
import qualified Data.Set as S
import Data.Array.Unboxed
import Data.Ord
import Control.Monad
import Data.Maybe
import Data.List

type Equation = Int
type Variable = Int

smallestDecompose :: UArray (Equation, Variable) Bool -> S.Set Equation -> S.Set Variable -> [(Equation, ([Variable], [Variable]))]
                     -> [([Equation], [Variable])]
smallestDecompose involves eqns vars edges
  | eqns == S.empty = []
  | Just (edges', (eqnFound, (varKnown, _))) <- useOneEdge edges vars =
    ([eqnFound], varKnown):(smallestDecompose involves (S.delete eqnFound eqns) (foldl' (flip S.delete) vars varKnown) edges')
  | Just e@(removedEqns, removedVars) <- smallestDecomposeOne involves eqns vars =
     e:(smallestDecompose involves (foldl' (flip S.delete) eqns removedEqns) (foldl' (flip S.delete) vars removedVars) edges)
  | otherwise = []

useOneEdge [] _ = Nothing
useOneEdge ((edge@(eqn, (vk, vn))):edges') vars =
  let
    vn' = filter (flip S.member vars) vn
  in
   if null vn' then
     Just (edges', edge)
   else
     liftM (\(edges'', e) -> (edge:edges'', e)) $ useOneEdge edges' vars

smallestDecomposeOne involves eqns vars =
  listToMaybe $ mapMaybe (tryDecomposeOne involves eqns vars S.empty S.empty) [(i, i) | i <- [1..(S.size eqns)]]

tryDecomposeOne involves eqns vars removeEqns removeVars (_, 0)
  | S.size removeEqns == S.size removeVars = Just (S.toList removeEqns, S.toList removeVars)
  | otherwise = Nothing

tryDecomposeOne involves eqns vars removeEqns removeVars (fullTarget, target) =
  let
    -- This is how many equations there will be when we add one - sort variables
    -- by distance from this.
    targetVars = (fromIntegral $ 1 + (S.size removeEqns))
    rankedCandidates = sortBy (comparing ((^2) . (\y -> y - targetVars) . S.size . snd)) $
                         map (\c -> (c, varsForEquation c)) $ S.toList eqns
    varsForEquation c = removeVars `S.union` (S.filter (\v -> involves!(c, v)) vars)
  in
    msum $ flip map rankedCandidates $ \(eqn, removeVars') -> do
      -- Once we have too many variables, adding more equations will never reduce the number.
      when (S.size removeVars' > fullTarget) (fail "")
      tryDecomposeOne involves (S.delete eqn eqns) (vars `S.difference` removeVars') (S.insert eqn removeEqns) removeVars' (fullTarget, target - 1)
