{-# LANGUAGE DeriveDataTypeable #-}
module Data.CellML12.IndexedModel
where
import qualified Data.Map as M
import Data.CellML12.Structure
import Data.Typeable
import Data.Data

data IndexedModel = IndexedModel {
  iModelComponents :: M.Map String (Either IndexedComponent (Import, ImportComponent)),
  iModelUnits :: M.Map String (Either Units (Import, ImportUnits)),
  iModelImports :: M.Map String Import,
  iModel :: WithCommon Model,
  iModelBase :: String
  } deriving (Eq, Ord, Show, Data, Typeable)

data IndexedComponent = IndexedComponent {
    iComponent :: WithCommon Component,
    iComponentVariables :: M.Map String Variable,
    iComponentUnits :: M.Map String Units
  } deriving (Eq, Ord, Show, Data, Typeable)

indexModel :: String -> WithCommon Model -> IndexedModel
indexModel b (m@(WithCommon _ Model { modelComponents = c, modelUnits = u, modelImports = i })) =
  IndexedModel {
    iModel = m,
    iModelBase = b,
    iModelComponents =
      M.fromList $
        (map (\vc@(WithCommon _ v) -> (componentName v, Left $ indexComponent vc)) c) ++
        (concatMap (\(WithCommon _ imp) ->
                    map (\(WithCommon _ ic) ->
                          (importComponentName ic, Right (imp, ic))) (importComponent imp)
                   ) i),
    iModelUnits = M.fromList $ (map (\(WithCommon _ v) -> (unitsName v, Left v)) u) ++
                  (concatMap (\(WithCommon _ imp) ->
                               map (\(WithCommon _ iu) ->
                                     (importUnitsName iu, Right (imp, iu))) (importUnits imp)
                             ) i),
    iModelImports = M.fromList $ map (\(WithCommon _ imp) -> (importName imp, imp)) i
               }

indexComponent :: WithCommon Component -> IndexedComponent
indexComponent (c@(WithCommon _ (Component { componentVariables = v, componentUnits = u }))) =
  IndexedComponent { iComponent = c,
                     iComponentVariables =
                       M.fromList $ map (\(WithCommon _ var) -> (variableName var, var)) v, 
                     iComponentUnits =
                       M.fromList $ map (\(WithCommon _ units) -> (unitsName units, units)) u
                   }
