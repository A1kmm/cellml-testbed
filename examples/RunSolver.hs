{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import Data.CellML12.ModelSolver
import Data.CellML12.Parser (InvalidCellML(..))
import Data.CellML12.ToSimplifiedModel
import Data.CellML12.SimplifiedModel
import Data.CellML12.ModelLoader (runLoadModels)
import System.Console.CmdArgs
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Array.IArray as A

data SolverOpts = SolverOpts {
  modelURL :: String, componentOfBoundVariable :: String,
  boundVariableName :: String, lowerLimit :: Double, upperLimit :: Double,
  relativeTolerance :: Double, absoluteTolerance :: Double, 
  dumpCode :: Bool } deriving (Show, Data, Typeable)

solverOpts = SolverOpts { modelURL = def &= help "URL of the model to simulate",
                          componentOfBoundVariable = def &= help "The component containing the bound variable for the integral",
                          boundVariableName = def &= help "The name of the bound variable" &= opt ("time" :: String),
                          lowerLimit = def &= help "The lower limit of the integral to compute" &= opt (0 :: Double),
                          upperLimit = def &= help "The upper limit of the integral to compute" &= opt (10 :: Double),
                          relativeTolerance = def &= help "The relative solver tolerance" &= opt (1E-6 :: Double),
                          absoluteTolerance = def &= help "The absolute solver tolerance" &= opt (1E-6 :: Double), 
                          dumpCode = def &= help "Dump generated code" }

rightOrFail _ (Right x) = return x
rightOrFail ef (Left e) = fail $ ef e
justOrFail _ (Just x) = return x
justOrFail m _ = fail m

findTime :: SimplifiedModel -> SolverOpts -> Maybe VariableID
findTime (SimplifiedModel { variableInfo = VariableInfo vmap}) (SolverOpts { boundVariableName = n,
                                                                             componentOfBoundVariable = c }) =
  case find (any ((==(ModelPathComponent c (ComponentPathVariable n))) . fst) . snd) 
       (M.toList vmap)
  of
    Just (vid, _) -> return vid
    Nothing -> Nothing

makeCSVHeading :: SimplifiedModel -> M.Map (VariableID, Int) Int -> LBS.ByteString
makeCSVHeading (SimplifiedModel { variableInfo = VariableInfo vimap }) vToID =
  -- Reverse the variable map to yield index => variable path...
  let
    idxToPath = M.fromList . mapMaybe (\((v,deg), idx) -> liftM (((,)idx) . (flip (,) deg) . fst . head) (M.lookup v vimap)) . M.toList $ vToID
  in
   LBS.intercalate "," (map (\i -> case M.lookup i idxToPath of
                                Nothing -> "Internal variable"
                                Just (vpath, deg) ->
                                  displayPath vpath `LBS.append`
                                  (LBS.concat (take deg (repeat "'")))
                            ) [0..(M.size vToID)])

displayPath (ModelPathComponent s cp) = (LBS.pack s) `LBS.append` ("/" `LBS.append` displayCPath cp)
displayPath (ModelPathImport i mp) = (LBS.pack i) `LBS.append` ("/" `LBS.append` displayPath mp)
displayPath _ = ""
displayCPath (ComponentPathVariable s) = LBS.pack s
displayCPath _ = ""

main = do
  cmd <- cmdArgs solverOpts
  m <- rightOrFail (\(InvalidCellML e) -> "Model load failed: " ++ e) =<<
       runLoadModels (buildSimplifiedModel (modelURL cmd))
  vt <- justOrFail "Cannot find specified bound variable for integration" $ findTime m cmd
  rightOrFail (\x -> "Error running model: " ++ x) =<<
    (runSolverOnDAESimplifiedModel m (DAEIntegrationSetup { daeModel = m, daeBoundVariable = vt, daeWithGenCode = if dumpCode cmd then LBS.putStrLn else const (return ()) }) $ do
      DAEIntegrationResults idx rows <- solveModelWithParameters
             (DAEIntegrationProblem { daeParameterOverrides = [],
                                      daeBVarRange = (lowerLimit cmd, upperLimit cmd),
                                      daeRelativeTolerance = relativeTolerance cmd,
                                      daeAbsoluteTolerance = absoluteTolerance cmd })
      liftIO . LBS.putStrLn $ makeCSVHeading m idx
      mapM (liftIO . LBS.putStrLn . LBS.intercalate "," . map (LBS.pack . show) . A.elems . snd) rows
    )
