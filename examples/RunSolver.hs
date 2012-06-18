{-# LANGUAGE OverloadedStrings #-}
import Data.CellML12.ModelSolver
import Data.CellML12.ToSimplifiedModel
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Array.IArray as A

data SolverOpts { modelURL :: String, componentOfBoundVariable :: String, boundVariableName :: String, 
                  lowerLimit :: Double, upperLimit :: Double,
                  relativeTolerance :: Double, absoluteTolerance :: Double } deriving (Show, Data, Typeable)
solverOpts = SolverOpts { modelURL = def &= help "URL of the model to simulate",
                          componentOfBoundVariable = def &= help "The component containing the bound variable for the integral",
                          boundVariableName = def &= help "The name of the bound variable" &= opt "time",
                          lowerLimit = def &= help "The lower limit of the integral to compute" &= opt 0,
                          upperLimit = def &= help "The upper limit of the integral to compute" &= opt 10,
                          relativeTolerance = def &= help "The relative solver tolerance" &= opt 1E-6,
                          absoluteTolerance = def &= help "The absolute solver tolerance" &= opt 1E-6 }

rightOrFail _ (Right x) = return x
rightOrFail ef (Left e) = fail $ ef e
justOrFail _ (Just x) = return x
justOrFail m _ = fail m

findTime :: SimplifiedModel -> SolverOpts -> VariableID
findTime (SimplifiedModel { variableInfo = VariableInfo vmap}) (SolverOpts { boundVariableName = n,
                                                                             componentOfBoundVariable = c }) =
  find (any ( . snd) . snd) (M.toList vmap)

makeCSVHeading :: SimplifiedModel -> M.Map (VariableID, Int) Int -> LBS.ByteString

main = do
  cmd <- cmdArgs solverOpts
  m <- rightOrFail (\InvalidCellML e -> "Model load failed: " ++ e) =<<
       runLoadModels (buildSimplifiedModel (modelURL cmd))
  vt <- justOrFail "Cannot find specified bound variable for integration" $ findTime m cmd
  rightOrFail (\x -> "Error running model: " ++ x) =<<
    runSolverOnDAESimplifiedModel m (DAEIntegrationSetup { daeModel = m, daeBoundVariable = vt }) $ do
      DAEIntegrationResult idx rows <- solveModelWithParameters
             (DAEIntegrationProblem { daeParameterOverrides = [],
                                      daeBVarRange = (lowerLimit cmd, upperLimit cmd),
                                      daeRelativeTolerance = relativeTolerance cmd,
                                      daeAbsoluteTolerance = absoluteTolerance cmd })
      LBS.putStrLn $ makeCSVHeading m idx
      mapM (LBS.putStrLn . LBS.intersperse ',' . map (LBS.pack . show) . A.elems . snd) rows
