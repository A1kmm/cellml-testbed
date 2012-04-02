module Data.CellML12.SimplifiedModel
where

newtype VariableID = VariableID Int
newtype BaseUnit = BaseUnit Int
data CanonicalUnits = CanoncialUnits {
  cuOffset :: Double,
  cuMultiplier :: Double,
  cuBases :: [(BaseUnit, Double)]
  }

newtype AssertionContext = AssertionContext {
  variableMap :: M.Map String (CanonicalUnits, VariableID)
  }

data ModelPath = ModelPathStop | ModelPathComponent String ComponentPath |
                 ModelPathUnits String |
                 ModelPathImport String ImportPath
data ComponentPath = ComponentPathStop | ComponentPathVariable String
data ImportPath = ImportPathStop | ImportPathComponent String ComponentPath | ImportPathUnits String

newtype VariableInfo = VariableInfo (M.Map VariableID [(ModelPath, CanoncialUnits)])

newtype Assertion = Assertion (M.ASTC, AssertionContext)

data SimplifiedModel = SimplifiedModel {
  variableInfo :: VariableInfo,
  assertions :: [Assertion]
  }
