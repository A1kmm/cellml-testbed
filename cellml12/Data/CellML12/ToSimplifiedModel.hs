{-# LANGUAGE PatternGuards #-}
module Data.CellML12.ToSimplifiedModel
where

import Data.CellML12.Structure
import Data.CellML12.SimplifiedModel
import Data.CellML12.Parser
import Data.CellML12.IndexedModel
import Data.CellML12.ModelLoader
import Control.Monad.Trans.Error
import Data.List
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.CellML12.DisjointSet

type CanonicalUnitsState = (Int, M.Map (String, String) CanonicalUnits,
                            M.Map (ModelPath, String) CanonicalUnits)
type TypeTable = M.Map (ModelPath, String) String
type UnitsTable = M.Map (ModelPath, String) CanonicalUnits

-- | Load a model and simplify it into a list of mathematical equations.
buildSimplifiedModel :: (Monad m, ModelLoader m) => String -> m SimplifiedModel
buildSimplifiedModel mpath = do
  model <- loadModel mpath ""
  comps <- findRelevantComponents model
  let typeTable = buildTypeTable model comps
  unitsTable <- buildUnitsTable model comps
  (varMap, varInfo) <- buildVariableInfo model comps unitsTable
  let assertions = buildAssertions model comps unitsTable typeTable varMap
  return $ SimplifiedModel (VariableInfo varInfo) assertions

-- | Builds a map of all variables in the model to their corresponding type.
buildTypeTable :: IndexedModel -> [(IndexedComponent, ModelPath)] -> TypeTable
buildTypeTable m comps = M.fromList $ flip concatMap comps $ \(icomp, path) ->
  flip map (componentVariables . withoutCommon $ (iComponent icomp)) $ \(WithCommon _ var) ->
  ((path, variableName var), variableType var)

-- | Builds a list of assertions from the model that apply at all times in the solution.
buildAssertions :: IndexedModel -> [(IndexedComponent, ModelPath)] -> UnitsTable -> TypeTable ->
                   M.Map (ModelPath, String) VariableID -> [Assertion]
buildAssertions m comps unitsTable typeTable varMap =
  flip concatMap comps $ \(icomp, cp) ->
    let
      comp = withoutCommon . iComponent $ icomp
      variableToContextPart (WithCommon _ v) = (variableName v, (
                                                   typeTable ! (cp, variableName v),
                                                   M.lookup (cp, variableName v) unitsTable,
                                                   varMap ! (cp, variableName v)))
      assertVarMap = M.fromList $ map variableToContextPart (componentVariables comp)
    in
     flip map (componentMaths comp) $ \math ->
       Assertion (math, AssertionContext assertVarMap)

-- | Builds up information about what variables are connected to what other models.
buildVariableInfo :: (Monad m, ModelLoader m) => IndexedModel -> [(IndexedComponent, ModelPath)] -> UnitsTable ->
                     m (M.Map (ModelPath, String) VariableID,
                        M.Map VariableID [(ModelPath, Maybe CanonicalUnits)])
buildVariableInfo m comps unitsTable = do
  let allVars = concatMap (\(c, p) -> map (\(n, _) -> (p, n))
                                          (M.toList (iComponentVariables c))
                          ) comps
      allConnModelPaths = foldl' (\s (_, p) -> foldl' (\s' p' -> S.insert p' s') s (findParentModelPaths p)) S.empty comps
  conns <- findConnectionsUsing m allConnModelPaths ModelPathStop
  let groupedComps = applyMerges allVars conns
      p2v = M.fromList $ concatMap (\(l, i) -> map (\v->(v, i)) l) $ (zip groupedComps (map VariableID [0..]))
      addOrNew v Nothing = [v]
      addOrNew v (Just l)  = v:l
      addPathAndUnitsForVariable m v@(c, _) =
        M.alter (Just . (addOrNew (c, M.lookup v unitsTable))) (p2v ! v) m
      v2pu = foldl' addPathAndUnitsForVariable M.empty allVars
  return (p2v, v2pu)

findConnectionsUsing :: (Monad m, ModelLoader m) => IndexedModel -> S.Set ModelPath -> ModelPath ->
                        m [((ModelPath, String), (ModelPath, String))]
findConnectionsUsing m paths p = do
  l1 <- liftM concat $ forM (map withoutCommon . modelConnections . withoutCommon . iModel $ m) $
    \(Connection comp1 comp2 mv) -> do
      (_, _, c1p) <- findSpecificComponent p m comp1
      (_, _, c2p) <- findSpecificComponent p m comp2
      return $ map (\(WithCommon _ (v1,v2)) -> ((c1p, v1), (c2p, v2))) mv
  l2 <- liftM concat $ forM (modelImports . withoutCommon . iModel $ m) $ \(WithCommon _ (Import { importName = n, importHref = h })) ->
    let p' = replaceModelPathStop p (ModelPathImport n ModelPathStop)
    in
     if (S.member p' paths) then do
       m' <- loadModel (iModelBase m) h
       findConnectionsUsing m' paths p'
     else return []
  return $ l1 ++ l2

-- | Makes a list of all model paths above a given component or units path.
findParentModelPaths :: ModelPath -> [ModelPath]
findParentModelPaths (ModelPathImport n p) =
  ModelPathStop:(map (ModelPathImport n) (findParentModelPaths p))
findParentModelPaths _ = [ModelPathStop]

-- | Extend a ModelPath by substituting the terminal model for something else, so
-- | the path points to something in the model the original path refers to.
replaceModelPathStop :: ModelPath -> ModelPath -> ModelPath
replaceModelPathStop p r = transformBi (replaceModelPathStop' r) p
replaceModelPathStop' r ModelPathStop = r
replaceModelPathStop' _ v = v

-- | Extend a ModelPath by substituting the terminal component for something else, so
-- | the path points to something in the model the original path refers to.
replaceComponentPathStop :: ModelPath -> ComponentPath -> ModelPath
replaceComponentPathStop p r = transformBi (replaceComponentPathStop' r) p
replaceComponentPathStop' r ComponentPathStop = r
replaceComponentPathStop' _ v = v

-- | Find a (possibly imported) model using an IndexedModel and a ModelPath.
pathToModel :: (ModelLoader m, Monad m) => IndexedModel -> ModelPath -> m IndexedModel
pathToModel m (ModelPathImport n p)
  | Just (Import { importHref = url }) <- M.lookup n (iModelImports m) =
    (loadModel (iModelBase m) url) >>= flip pathToModel p
  | otherwise = fail $ "Path found to unknown import " ++ n
pathToModel m _ = return m

-- | Produce a ModelPath that does not include any component or units at the end from one that might.
stopAtModel :: ModelPath -> ModelPath
stopAtModel s@ModelPathStop = s
stopAtModel (ModelPathComponent _ _) = ModelPathStop
stopAtModel (ModelPathUnits _) = ModelPathStop
stopAtModel (ModelPathImport s mp) = ModelPathImport s (stopAtModel mp)

-- | Build a table that maps every unit used on every variable in the model to a
-- | canonical form in terms of base units.
buildUnitsTable :: (Monad m, ModelLoader m) =>
                   IndexedModel -> [(IndexedComponent, ModelPath)] -> m UnitsTable
buildUnitsTable m l = liftM (\(_, _, c) -> c) $ 
                        foldM (\s (icomp@IndexedComponent { iComponent = WithCommon _ (Component { componentVariables = vl }) }, path) ->
                                foldM (addUnitsToTable m path icomp) s vl) (7, M.empty, M.empty) l

-- | Ensures that the units of a particular variable are in the units table.
addUnitsToTable :: (ModelLoader m, Monad m) => IndexedModel -> ModelPath -> IndexedComponent ->
                   CanonicalUnitsState -> WithCommon Variable ->
                   m CanonicalUnitsState
addUnitsToTable m compPath comp s (WithCommon _ (Variable { variableUnits = Just u })) =
  liftM fst $ resolveComponentUnits m compPath (stopAtModel compPath) comp s u
addUnitsToTable _ _ _ s _ = return s -- No units on variable

-- | Convert a ModelPath into a string describing the location in the model.
describeModelPath :: ModelPath -> String
describeModelPath ModelPathStop = "top-level model"
describeModelPath (ModelPathImport v ModelPathStop) = "import named " ++ v
describeModelPath (ModelPathImport v p) = (describeModelPath p) ++ ", in import named " ++ v
describeModelPath (ModelPathUnits v) = "units " ++ v
describeModelPath (ModelPathComponent cn ComponentPathStop) = "component " ++ cn
describeModelPath (ModelPathComponent cn cp) = (describeComponentPath cp) ++ ", in component " ++ cn
describeComponentPath :: ComponentPath -> String
describeComponentPath ComponentPathStop = "some component"
describeComponentPath (ComponentPathVariable v) = "variable " ++ v
describeComponentPath (ComponentPathUnits u) = "units " ++ u

-- | Find units which are in model context, given an IndexedModel, a path to the
-- | model, and the name of the units to find. Both local and imported units are
-- | considered.
findModelUnits :: (Monad m, ModelLoader m) => ModelPath -> IndexedModel -> String -> m (Units, String, String)
findModelUnits modPath cmod u =
  case M.lookup u (iModelUnits cmod) of
    Nothing -> fail $ "Variable refers to units " ++ u ++ " but no such units found in " ++ (describeModelPath modPath)
    Just (Left units) -> return (units, u, iModelBase cmod)
    Just (Right (imp, impu)) -> do
      imodel <- loadModel (iModelBase cmod) (importHref imp)
      findModelUnits
        (replaceModelPathStop modPath (ModelPathImport (importName imp) ModelPathStop))
        imodel
        (importUnitsUnitsRef impu)

buMetre = BaseUnit 0
buSecond = BaseUnit 1
buKilogram = BaseUnit 2
buAmpere = BaseUnit 3
buMole = BaseUnit 4
buKelvin = BaseUnit 5
buCandela = BaseUnit 6

builtinUnits :: M.Map String CanonicalUnits
builtinUnits = M.fromList $ [
  ("ampere", CanonicalUnits 0 1 (M.fromList [(buAmpere, 1)])),
  ("becquerel", CanonicalUnits 0 1 (M.fromList [(buSecond, 1)])),
  ("ampere", CanonicalUnits 0 1 (M.fromList [(buAmpere, 1)])),
  ("becquerel", CanonicalUnits 0 1 (M.fromList [(buSecond, -1)])),
  ("candela", CanonicalUnits 0 1 (M.fromList [(buCandela, 1)])),
  ("celsius", CanonicalUnits 273.15 1 (M.fromList [(buKelvin, 1)])),
  ("coulomb", CanonicalUnits 0 1 (M.fromList [(buSecond, 1)])),
  ("dimensionless", CanonicalUnits 0 1 (M.fromList [])),
  ("farad", CanonicalUnits 0 1 (M.fromList ([(buMetre, -2), (buKilogram, -1), (buSecond, 4), (buAmpere, 2)]))),
  ("gram", CanonicalUnits 0 1E-3 (M.fromList ([(buKilogram, 1)]))),
  ("gray", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buSecond, -2)]))),
  ("henry", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buKilogram, 1), (buSecond, -2), (buAmpere, -2)]))),
  ("hertz", CanonicalUnits 0 1 (M.fromList ([(buSecond, -1)]))),
  ("joule", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buKilogram, 1), (buSecond, -2)]))),
  ("katal", CanonicalUnits 0 1 (M.fromList ([(buSecond, -1), (buMole, 1)]))),
  ("kelvin", CanonicalUnits 0 1 (M.fromList ([(buKelvin, 1)]))),
  ("kilogram", CanonicalUnits 0 1 (M.fromList ([(buKilogram, 1)]))),
  ("liter", CanonicalUnits 0 1E-3 (M.fromList ([(buMetre, 3)]))),
  ("litre", CanonicalUnits 0 1E-3 (M.fromList ([(buMetre, 3)]))),
  ("lumen", CanonicalUnits 0 1 (M.fromList ([(buCandela, 1)]))),
  ("lux", CanonicalUnits 0 1 (M.fromList ([(buMetre, -2), (buCandela, 1)]))),
  ("meter", CanonicalUnits 0 1 (M.fromList ([(buMetre, 1)]))),
  ("metre", CanonicalUnits 0 1 (M.fromList ([(buMetre, 1)]))),
  ("mole", CanonicalUnits 0 1 (M.fromList ([(buMole, 1)]))),
  ("newton", CanonicalUnits 0 1 (M.fromList ([(buMetre, 1), (buKilogram, 1), (buSecond, -2)]))),
  ("ohm", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buKilogram, 1), (buSecond, -3), (buAmpere, -2)]))),
  ("pascal", CanonicalUnits 0 1 (M.fromList ([(buMetre, -1), (buKilogram, 1), (buSecond, -2)]))),
  ("radian", CanonicalUnits 0 1 (M.fromList ([]))),
  ("second", CanonicalUnits 0 1 (M.fromList ([(buSecond, 1)]))),
  ("siemens", CanonicalUnits 0 1 (M.fromList ([(buMetre, -2), (buKilogram, -1), (buSecond, 3), (buAmpere, 2)]))),
  ("sievert", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buSecond, -2)]))),
  ("steradian", CanonicalUnits 0 1 (M.fromList ([]))),
  ("tesla", CanonicalUnits 0 1 (M.fromList ([(buKilogram, 1), (buSecond, -2), (buAmpere, -1)]))),
  ("volt", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buKilogram, 1), (buSecond, -3), (buAmpere, -1)]))),
  ("watt", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buKilogram, 1), (buSecond, -3)]))),
  ("weber", CanonicalUnits 0 1 (M.fromList ([(buMetre, 2), (buKilogram, 1), (buSecond, -2), (buAmpere, -1)])))
  ]

resolveComponentUnits :: (ModelLoader m, Monad m) => IndexedModel -> ModelPath -> ModelPath -> IndexedComponent ->
                         CanonicalUnitsState -> String ->
                         m (CanonicalUnitsState, CanonicalUnits)
resolveComponentUnits m compPath modPath comp s@(nextbase, unitsByURL, unitsTable) u =
  case M.lookup (compPath, u) unitsTable of
    Just v -> return (s, v)
    Nothing -> case (M.lookup (modPath, u) unitsTable `mplus` M.lookup u builtinUnits) of
      Just v -> return $ ((nextbase, unitsByURL, M.insert (compPath, u) v unitsTable), v)
      Nothing -> case M.lookup u (iComponentUnits comp) of
          Just units -> do
            ((nextbase', unitsByURL', unitsTable'), canon) <- canonicaliseUnits resolveComponentUnits m compPath modPath comp s units
            return $ ((nextbase', unitsByURL', M.insert (compPath, u) canon unitsTable'), canon)
          Nothing -> do
            cmod <- pathToModel m modPath
            (units, unitsName, unitsURL) <- findModelUnits modPath cmod u
            ((nextbase', unitsByURL', unitsTable'), canon) <- case M.lookup (unitsName, unitsURL) unitsByURL of
              Nothing -> canonicaliseUnits resolveModelUnits m compPath modPath comp s units
              Just canon -> return ((nextbase, unitsByURL, unitsTable), canon)
            return $ ((nextbase', M.insert (unitsName, unitsURL) canon unitsByURL',
                       M.insert (modPath, u) canon (M.insert (compPath, u) canon unitsTable')), canon)

resolveModelUnits :: (ModelLoader m, Monad m) => IndexedModel -> ModelPath -> ModelPath -> IndexedComponent ->
                     CanonicalUnitsState -> String ->
                     m (CanonicalUnitsState, CanonicalUnits)
resolveModelUnits m compPath modPath comp s@(nextbase, unitsByURL, unitsTable) u =
  case M.lookup (modPath, u) unitsTable `mplus` M.lookup u builtinUnits of
    Just v -> return $ ((nextbase, unitsByURL, M.insert (compPath, u) v unitsTable), v)
    Nothing -> do
      cmod <- pathToModel m modPath
      (units, unitsName, unitsURL) <- findModelUnits modPath cmod u
      ((nextbase', unitsByURL', unitsTable'), canon) <- case M.lookup (unitsName, unitsURL) unitsByURL of
        Just canon -> return ((nextbase, unitsByURL, unitsTable), canon)
        Nothing -> canonicaliseUnits resolveModelUnits m compPath modPath comp s units
      return $ ((nextbase', M.insert (unitsName, unitsURL) canon unitsByURL',
                 M.insert (modPath, u) canon (M.insert (compPath, u) canon unitsTable')), canon)

canonicaliseUnits :: (ModelLoader m, Monad m) =>
                     (IndexedModel -> ModelPath -> ModelPath -> IndexedComponent ->
                      CanonicalUnitsState -> String ->
                      m (CanonicalUnitsState, CanonicalUnits)) ->
                     IndexedModel -> ModelPath -> ModelPath -> IndexedComponent ->
                     CanonicalUnitsState -> Units ->
                     m (CanonicalUnitsState, CanonicalUnits)
canonicaliseUnits _ _ _ _ _ s@(nextbase, unitsByURL, unitsTable) (Units { unitsBaseUnits = True }) =
  let
    newBase = BaseUnit nextbase
    canon = CanonicalUnits { cuOffset = 0, cuMultiplier = 1, cuBases = M.fromList [(newBase, 1)]}
  in
   return ((nextbase + 1, unitsByURL, unitsTable), canon)

canonicaliseUnits lookupf m compPath modPath comp s units =
  foldM (canonicaliseOneUnit lookupf m compPath modPath comp units) (s, CanonicalUnits { cuOffset = 0, cuMultiplier = 1, cuBases = M.empty}) (unitsDefinition units)

canonicaliseOneUnit lookupf m compPath modPath comp units
  (s, (CanonicalUnits { cuOffset = o, cuMultiplier = om, cuBases = b }))
  (WithCommon _ (Unit { unitUnits = uu, unitPrefix = up, unitExponent = ue, unitMultiplier = um,
                        unitOffset = uo })) = do
    when (uo /= 0 && not (M.null b)) . fail $ "Units " ++ (unitsName units) ++ ": Cannot have more than one unit element with offset"
    (s', CanonicalUnits { cuOffset = no, cuMultiplier = nm, cuBases = nb }) <-
      lookupf m compPath modPath comp s uu
    return $ (s', CanonicalUnits { cuOffset = uo + no, cuMultiplier = om * um * nm,
                                   cuBases = M.unionWith (+) b (M.map (*ue) nb)})

findSpecificComponent :: (Monad m, ModelLoader m) => ModelPath -> IndexedModel -> String ->
                         m ([(ModelPath, String)], IndexedComponent, ModelPath)
findSpecificComponent p m comp = do
  case M.lookup comp (iModelComponents m) of
    Nothing -> fail $ "Reference to component " ++ comp ++ " that doesn't exist."
    Just (Left c) -> return ([(p, comp)], c, replaceModelPathStop p (ModelPathComponent comp ComponentPathStop))
    Just (Right (imp, ic)) -> do
      impMod <- loadModel (iModelBase m) (importHref imp)
      (aliases, c, mp) <- findSpecificComponent (replaceModelPathStop p (ModelPathImport (importName imp) ModelPathStop)) impMod
                                                (importComponentComponentRef ic)
      return ((p, comp):aliases, c, mp)

findRelevantComponents :: (Monad m, ModelLoader m) => IndexedModel -> m [(IndexedComponent, ModelPath)]
findRelevantComponents m = do
  let toplevelComps = map (either (componentName . withoutCommon . iComponent) (importComponentName . snd))
                          (M.elems $ iModelComponents m)
  initialComps <- mapM (findSpecificComponent ModelPathStop m) toplevelComps
  newComps <- expandComponentsUsingEncapsulation m initialComps
  return $ map (\(_, a, b) -> (a, b)) $ newComps

expandComponentsUsingEncapsulation :: (Monad m, ModelLoader m) => IndexedModel -> [([(ModelPath, String)], IndexedComponent, ModelPath)] ->
                                      m [([(ModelPath, String)], IndexedComponent, ModelPath)]
expandComponentsUsingEncapsulation m c0 = do
  -- Build a list of unique models...
  let uniqueModPaths = foldl' (\s i -> S.insert i s) S.empty $ concatMap (\(l, _, _) -> map fst l) c0
  uniqueMods <- mapM (\p -> liftM2 (,) (return p) (pathToModel m p)) (S.toList uniqueModPaths)
  -- Build a list of all encapsulation relationships...
  let allEnc = concatMap (\(p, m) ->
                           map (\(cn1, cn2) -> (p, cn1, cn2)) $
                             concatMap (\(WithCommon _ enc) ->
                                         concatMap (\(WithCommon _ cr) -> crToEncList cr) (encapsulationComponentRefs enc))
                                       (modelEncapsulation (withoutCommon $ iModel m))) uniqueMods
  -- Make a mapping from (path to model, component name) -> path to actual component
  let actualComponentPath = foldl' (\pn2acp (l, _, p) ->
                                     foldl' (\pn2acp' n -> M.insert n p pn2acp') pn2acp l) M.empty c0
  selectDescendents uniqueModPaths m c0 allEnc actualComponentPath

selectDescendents oldModPaths m clist allEnc acp = do
  -- Make a list of previously non-included components encapsulated under included ones.
  let toNew = mapMaybe (\(p, cn1, cn2) ->
                         maybe Nothing (\_ -> maybe (Just (p, cn2))
                                                      (const Nothing) (M.lookup (p, cn2) acp))
                               (M.lookup (p, cn1) acp)) allEnc
  clist' <- forM toNew $ \(p, n) -> do
    newm <- pathToModel m p
    findSpecificComponent p newm n
  let cnew = nub . sort $ clist'
  let newAcp = foldl' (\pn2acp (l, _, p) ->
                        foldl' (\pn2acp' n -> M.insert n p pn2acp') pn2acp l) acp cnew
  let newModPaths = (foldl' (\s i -> S.insert i s) S.empty $ concatMap (\(l, _, _) -> map fst l) cnew) `S.difference` oldModPaths
  newMods <- mapM (\p -> liftM2 (,) (return p) (pathToModel m p)) (S.toList newModPaths)
  let newEnc = concatMap (\(p, m) -> map (\(cn1, cn2) -> (p, cn1, cn2)) $
                                     concatMap (\(WithCommon _ enc) ->
                                                 concatMap (\(WithCommon _ cr) -> crToEncList cr) (encapsulationComponentRefs enc))
                                               (modelEncapsulation (withoutCommon $ iModel m))) newMods
  if null cnew
    then return clist
    else liftM2 (++) (return clist) (selectDescendents (newModPaths `S.union` oldModPaths) m cnew (newEnc ++ allEnc) newAcp)

crToEncList :: ComponentRef -> [(String, String)]
crToEncList (cr@ComponentRef{ componentRefComponent = n1, componentRefComponentRefs = l }) =
  concatMap (\(WithCommon _ (cr2@ComponentRef{ componentRefComponent = n2 })) -> (n1, n2):(crToEncList cr2)) l
