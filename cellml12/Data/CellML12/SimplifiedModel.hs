{-# LANGUAGE DeriveDataTypeable #-}
module Data.CellML12.SimplifiedModel
where
import qualified Data.Map as M
import qualified Data.ContentMathML3.Structure as M
import Data.Typeable
import Data.Data
import Data.CellML12.DisjointSet

newtype VariableID = VariableID Int deriving (Eq, Ord, Typeable, Data, Show)
newtype BaseUnit = BaseUnit Int deriving (Eq, Ord, Typeable, Data, Show)
data CanonicalUnits = CanonicalUnits {
  cuOffset :: Double,
  cuMultiplier :: Double,
  cuBases :: M.Map BaseUnit Double
  } deriving (Eq, Ord, Typeable, Data, Show)

type TypeName = String

newtype AssertionContext = AssertionContext {
  variableMap :: M.Map String (TypeName, Maybe CanonicalUnits, VariableID)
  } deriving (Eq, Ord, Typeable, Data, Show)

data ModelPath = ModelPathStop | ModelPathComponent String ComponentPath |
                 ModelPathUnits String |
                 ModelPathImport String ModelPath deriving (Eq, Ord, Typeable, Data, Show)
data ComponentPath = ComponentPathStop | ComponentPathVariable String | ComponentPathUnits String deriving (Eq, Ord, Typeable, Data, Show)

newtype VariableInfo = VariableInfo (M.Map VariableID [(ModelPath, Maybe CanonicalUnits)]) deriving (Eq, Ord, Typeable, Data, Show)

newtype Assertion = Assertion (M.ASTC, AssertionContext) deriving (Eq, Ord, Typeable, Data, Show)

data SimplifiedModel = SimplifiedModel {
  variableInfo :: VariableInfo,
  assertions :: [Assertion]
  } deriving (Eq, Ord, Typeable, Data, Show)
