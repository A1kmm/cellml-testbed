module Data.CellML12.SystemDecomposer where
import qualified Data.Set as S
import Data.Array.Unboxed
import Data.Ord
import Control.Monad
import Data.Maybe
import Data.List
import Debug.Trace

type Equation = Int
type Variable = Int

smallestDecompose :: UArray (Equation, Variable) Bool -> S.Set Equation -> S.Set Variable -> [([Equation], [Variable])]
smallestDecompose involves eqns vars
  | eqns == S.empty = []
  | otherwise =
    let
      e@(removedEqns, removedVars) = smallestDecomposeOne involves eqns vars
    in
     e:(smallestDecompose involves (foldl' (flip S.delete) eqns removedEqns) (foldl' (flip S.delete) vars removedVars))

smallestDecomposeOne involves eqns vars = head $ mapMaybe (tryDecomposeOne involves eqns vars S.empty S.empty) [(i, i) | i <- [1..]]

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
      tryDecomposeOne involves eqns vars (S.insert eqn removeEqns) removeVars' (fullTarget, target - 1)
