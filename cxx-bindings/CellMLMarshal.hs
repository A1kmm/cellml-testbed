{-# LANGUAGE ForeignFunctionInterface #-}
module CellMLMarshal where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.CellML12.ModelSolver
import Data.CellML12.ToSimplifiedModel
import Data.CellML12.SimplifiedModel
import Data.CellML12.ModelLoader
import Data.CellML12.Parser
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Array.Unboxed as U
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.Error

foreign export ccall "camarshal_generateSolveCode" camarshal_generateSolveCode
  :: CString -> StablePtr ModelPath -> IO (StablePtr (Either String ((VariableMap, LBS.ByteString), SimplifiedModel)))
foreign export ccall "camarshal_isErrorFail" camarshal_isErrorFail
  :: StablePtr (Either String a) -> IO Bool
foreign export ccall "camarshal_eitherUnsafeGetError" camarshal_eitherUnsafeGetError
  :: StablePtr (Either String a) -> IO CString
foreign export ccall "camarshal_eitherUnsafeGetStablePtr" camarshal_eitherUnsafeGetStablePtr
  :: StablePtr (Either String a) -> IO (StablePtr a)
foreign export ccall "camarshal_makeModelPathComponent" camarshal_makeModelPathComponent
  :: CString -> CString -> IO (StablePtr ModelPath)
foreign export ccall "camarshal_makeModelPathImport" camarshal_makeModelPathImport
  :: CString -> StablePtr ModelPath -> IO (StablePtr ModelPath)
foreign export ccall "camarshal_getCCode" camarshal_getCCode
  :: StablePtr ((VariableMap, LBS.ByteString), SimplifiedModel) -> IO CString
foreign export ccall "camarshal_startIteratingVariablePaths" camarshal_startIteratingVariablePaths
  :: StablePtr (a, SimplifiedModel) -> IO (StablePtr [(VariableID, [ModelPath])])
foreign export ccall "camarshal_startIteratingVariableStorage" camarshal_startIteratingVariableStorage
  :: StablePtr ((VariableMap, LBS.ByteString), SimplifiedModel) -> IO (StablePtr [((VariableID, Int), (Int, Int))])
foreign export ccall "camarshal_advanceIterator" camarshal_advanceIterator
  :: StablePtr [a] -> IO (StablePtr [a])
foreign export ccall "camarshal_isIteratorFinished" camarshal_isIteratorFinished
  :: StablePtr [a] -> IO Bool
foreign export ccall "camarshal_getVarPathVarID" camarshal_getVarPathVarID
  :: StablePtr [(VariableID, [ModelPath])] -> IO CInt
foreign export ccall "camarshal_getVarPathModelPaths" camarshal_getVarPathModelPaths
  :: StablePtr [(VariableID, [ModelPath])] -> IO (StablePtr [ModelPath])
foreign export ccall "camarshal_getNextModelPath" camarshal_getNextModelPath
  :: StablePtr [ModelPath] -> IO (StablePtr ModelPath)
foreign export ccall "camarshal_isModelPathImport" camarshal_isModelPathImport
  :: StablePtr ModelPath -> IO Bool
foreign export ccall "camarshal_modelPathImportGetName" camarshal_modelPathImportGetName
  :: StablePtr ModelPath -> IO CString
foreign export ccall "camarshal_modelPathImportPath" camarshal_modelPathImportGetPath
  :: StablePtr ModelPath -> IO (StablePtr ModelPath)
foreign export ccall "camarshal_modelPathComponentGetName" camarshal_modelPathComponentGetName
  :: StablePtr ModelPath -> IO CString
foreign export ccall "camarshal_modelPathComponentGetVariable" camarshal_modelPathComponentGetVariable
  :: StablePtr ModelPath -> IO CString
foreign export ccall "camarshal_varStorageGetDeriv" camarshal_varStorageGetDeriv
  :: StablePtr [((VariableID, Int), (Int, Int))] -> IO CInt
foreign export ccall "camarshal_varStorageGetConstIdx" camarshal_varStorageGetConstIdx
  :: StablePtr [((VariableID, Int), (Int, Int))] -> IO CInt
foreign export ccall "camarshal_varStorageGetVarIdx" camarshal_varStorageGetVarIdx
  :: StablePtr [((VariableID, Int), (Int, Int))] -> IO CInt
foreign export ccall "camarshal_solveModelWithParameters" camarshal_solveModelWithParameters
  :: CString ->                                -- ^ URL for the model to solve for.
     StablePtr ModelPath ->                    -- ^ The path to the bound variable to solve for (consumed).
     StablePtr [((ModelPath, Int), Double)] -> -- ^ Parameters to override (consumed).
     CDouble ->                                -- ^ Start bvar (e.g. time).
     CDouble ->                                -- ^ End bvar (e.g. time).
     CDouble ->                                -- ^ Relative tolerance.
     CDouble ->                                -- ^ Absolute tolerance.
     IO (StablePtr (Either String (DAEIntegrationResult, SimplifiedModel)))
foreign export ccall "camarshal_makeEmptyList" camarshal_makeEmptyList
  :: IO (StablePtr [a])
foreign export ccall "camarshal_addOverride" camarshal_addOverride
  :: StablePtr ModelPath -> -- ^ The model path to the variable (consumed).
     Int ->                 -- ^ The derivative degree.
     CDouble ->             -- ^ The new value.
     StablePtr [((ModelPath, Int), Double)] ->
     IO (StablePtr [((ModelPath, Int), Double)])
foreign export ccall "camarshal_startIteratingVariableResultIndex" camarshal_startIteratingVariableResultIndex
  :: StablePtr (DAEIntegrationResult, SimplifiedModel) -> IO (StablePtr [((VariableID, Int), Int)])
foreign export ccall "camarshal_getNextResultIndexVariableID" camarshal_getNextResultIndexVariableID
  :: StablePtr [((VariableID, Int), Int)] -> IO CInt
foreign export ccall "camarshal_getNextResultIndexDeriv" camarshal_getNextResultIndexDeriv
  :: StablePtr [((VariableID, Int), Int)] -> IO CInt
foreign export ccall "camarshal_getNextResultIndex" camarshal_getNextResultIndex
  :: StablePtr [((VariableID, Int), Int)] -> IO CInt
foreign export ccall "camarshal_beginResults" camarshal_beginResults
  :: StablePtr (DAEIntegrationResult, SimplifiedModel) ->
     IO (StablePtr [(Double, U.UArray Int Double)])
foreign export ccall "camarshal_getBvarValue" camarshal_getBvarValue
  :: StablePtr [(Double, U.UArray Int Double)] ->
     IO CDouble
foreign export ccall "camarshal_getResultLength" camarshal_getResultLength
  :: StablePtr [(Double, U.UArray Int Double)] ->
     IO CInt
foreign export ccall "camarshal_getResultRow" camarshal_getResultRow
  :: StablePtr [(Double, U.UArray Int Double)] ->
     IO (Ptr CDouble) -- ^ Must be free()d by caller.

dupCStringNullTerm :: CStringLen -> IO CString
dupCStringNullTerm (ptr, len) = do
  newPtr <- mallocBytes (len + 4)
  copyBytes newPtr ptr len
  forM [0..3] $ \i ->
    pokeByteOff newPtr (len + i) ((fromIntegral 0) :: CChar)
  return newPtr

-- | Converts a strict ByteString to a cloned null terminated CString that
--   must be released externally by free (for returning to C code).
byteStringToNullTerm :: BS.ByteString -> IO CString
byteStringToNullTerm bs =
  BSU.unsafeUseAsCStringLen bs dupCStringNullTerm

-- | Result must be released externally by free.
lazyByteStringToNullTerm :: LBS.ByteString -> IO CString
lazyByteStringToNullTerm =
  byteStringToNullTerm . LBS.toStrict

camarshal_generateSolveCode cstr spBvarPath = do
  bvarPath <- deRefStablePtr spBvarPath
  url <- peekCString cstr
  freeStablePtr spBvarPath
  r <- runErrorT $ do
    m <- ((ErrorT $ (either (\(InvalidCellML a) -> Left a) Right <$> runLoadModels (buildSimplifiedModel url))) :: ErrorT String IO SimplifiedModel)
    bvarId <- maybe (fail "Cannot find specified bound variable") (return . fst) $
                find (any ((==bvarPath) . fst) . snd) $ M.toList (unvariableInfo (variableInfo m))
    either fail (\v -> return (v, m)) $ generateSolveCode m (DAEIntegrationSetup m bvarId (const (return ())))
  newStablePtr r

camarshal_isErrorFail sp =
  either (const True) (const False) <$> deRefStablePtr sp

camarshal_eitherUnsafeGetError sp =
  (either newCString (error "called camarshal_eitherUnsafeGetError on a non-error!") =<< deRefStablePtr sp) <* (freeStablePtr sp)

camarshal_eitherUnsafeGetStablePtr sp =
  (either (error "called camarshal_eitherUnsafeGetStablePtr on an error!") newStablePtr =<< deRefStablePtr sp) <* (freeStablePtr sp)

camarshal_makeModelPathComponent compName varName =
  (newStablePtr =<< (ModelPathComponent <$> peekCString compName <*> (ComponentPathVariable <$> peekCString varName)))
  
camarshal_makeModelPathImport impName mptr =
  (newStablePtr =<< (ModelPathImport <$> peekCString impName <*> (deRefStablePtr mptr)))
     <* (freeStablePtr mptr)

camarshal_getCCode sp =
  lazyByteStringToNullTerm =<< ((snd . fst) <$> deRefStablePtr sp)

camarshal_startIteratingVariablePaths :: StablePtr (a, SimplifiedModel) -> IO (StablePtr [(VariableID, [ModelPath])])
camarshal_startIteratingVariablePaths sp = do
  (_, SimplifiedModel (VariableInfo m) _) <- deRefStablePtr sp
  newStablePtr $ map (\(f,s) -> (f, map fst s)) (M.toList m)

camarshal_startIteratingVariableStorage :: StablePtr ((VariableMap, LBS.ByteString), SimplifiedModel) -> IO (StablePtr [((VariableID, Int), (Int, Int))])
camarshal_startIteratingVariableStorage sp = do
  ((VariableMap { vmapMap = m }, _), _) <- deRefStablePtr sp
  newStablePtr $ map (\(vd, (mc, mv)) -> (vd, (fromMaybe (-1) mc, fromMaybe (-1) mv))) (M.toList m)

camarshal_advanceIterator :: StablePtr [a] -> IO (StablePtr [a])
camarshal_advanceIterator sp = do
  l <- deRefStablePtr sp
  freeStablePtr sp
  newStablePtr (tail l)

camarshal_isIteratorFinished :: StablePtr [a] -> IO Bool
camarshal_isIteratorFinished sp = null <$> deRefStablePtr sp

camarshal_getVarPathVarID :: StablePtr [(VariableID, [ModelPath])] -> IO CInt
camarshal_getVarPathVarID sp =
  (fromIntegral . (\(VariableID x) -> x) . fst . head) <$> deRefStablePtr sp
  
camarshal_getVarPathModelPaths :: StablePtr [(VariableID, [ModelPath])] -> IO (StablePtr [ModelPath])
camarshal_getVarPathModelPaths sp = do
  l <- deRefStablePtr sp
  newStablePtr (snd (head l))

camarshal_getNextModelPath :: StablePtr [ModelPath] -> IO (StablePtr ModelPath)
camarshal_getNextModelPath sp = do
  l <- deRefStablePtr sp
  newStablePtr (head l)

camarshal_isModelPathImport :: StablePtr ModelPath -> IO Bool
camarshal_isModelPathImport sp = do
  mp <- deRefStablePtr sp
  return $ case mp of
    ModelPathImport s mp -> True
    _ -> False
  
camarshal_modelPathImportGetName :: StablePtr ModelPath -> IO CString
camarshal_modelPathImportGetName sp = newCString =<< ((\(ModelPathImport n _) -> n) <$> deRefStablePtr sp)

camarshal_modelPathImportGetPath :: StablePtr ModelPath -> IO (StablePtr ModelPath)
camarshal_modelPathImportGetPath sp = newStablePtr =<< ((\(ModelPathImport _ p) -> p) <$> deRefStablePtr sp)

camarshal_modelPathComponentGetName :: StablePtr ModelPath -> IO CString
camarshal_modelPathComponentGetName sp =
  newCString =<< ((\(ModelPathComponent n _) -> n) <$> deRefStablePtr sp)

camarshal_modelPathComponentGetVariable :: StablePtr ModelPath -> IO CString
camarshal_modelPathComponentGetVariable sp =
  newCString =<< ((\(ModelPathComponent n _) -> n) <$> deRefStablePtr sp)

camarshal_varStorageGetDeriv :: StablePtr [((VariableID, Int), (Int, Int))] -> IO CInt
camarshal_varStorageGetDeriv sp =
  (fromIntegral . snd . fst . head) <$> (deRefStablePtr sp)

camarshal_varStorageGetConstIdx :: StablePtr [((VariableID, Int), (Int, Int))] -> IO CInt
camarshal_varStorageGetConstIdx sp =
  (fromIntegral . fst . snd . head) <$> (deRefStablePtr sp)

camarshal_varStorageGetVarIdx :: StablePtr [((VariableID, Int), (Int, Int))] -> IO CInt
camarshal_varStorageGetVarIdx sp =
  (fromIntegral . snd . snd . head) <$> (deRefStablePtr sp)

camarshal_solveModelWithParameters
  :: CString -> StablePtr ModelPath -> StablePtr [((ModelPath, Int), Double)] ->
     CDouble -> CDouble -> CDouble -> CDouble ->
     IO (StablePtr (Either String (DAEIntegrationResult, SimplifiedModel)))
camarshal_solveModelWithParameters modelURL' bvarPath' overrideParams' startTime endTime rtol atol = do
  modelURL <- peekCString modelURL'
  bvarPath <- deRefStablePtr bvarPath'
  freeStablePtr bvarPath'
  overrideParams <- deRefStablePtr overrideParams'
  freeStablePtr overrideParams'
  ret <- runErrorT $ do
    m <- ((ErrorT $ (either (\(InvalidCellML a) -> Left a) Right <$>
                     runLoadModels (buildSimplifiedModel modelURL))) :: ErrorT String IO SimplifiedModel)
    overrideParamsResolved <- forM overrideParams $ \((mp, i), val) ->
      case find (\(_, l) -> any (\x -> fst x == mp) l)
                (M.toList $ unvariableInfo (variableInfo m)) of
        Nothing -> fail "One of the overrides refers to a variable not found in the model."
        Just (v, _) -> return ((v, i), val)
    bvarId <- maybe (fail "Cannot find specified bound variable") (return . fst) $
                find (any ((==bvarPath) . fst) . snd) $ M.toList (unvariableInfo (variableInfo m))
    (\v -> (v, m)) <$> (ErrorT $
        runSolverOnDAESimplifiedModel m
          (DAEIntegrationSetup { daeModel = m, daeBoundVariable = bvarId,
                                 daeWithGenCode = const (return ()) })
          (solveModelWithParameters (DAEIntegrationProblem { daeParameterOverrides = overrideParamsResolved,
                                                             daeBVarRange = (realToFrac startTime, realToFrac endTime), 
                                                             daeRelativeTolerance = realToFrac rtol,
                                                             daeAbsoluteTolerance = realToFrac atol
                                                           })))
  newStablePtr ret

camarshal_makeEmptyList :: IO (StablePtr [a])
camarshal_makeEmptyList = newStablePtr []

camarshal_addOverride :: StablePtr ModelPath -> Int -> CDouble -> StablePtr [((ModelPath, Int), Double)] -> IO (StablePtr [((ModelPath, Int), Double)])
camarshal_addOverride spmp deg val spl = do
  mp <- deRefStablePtr spmp
  freeStablePtr spmp
  l <- deRefStablePtr spl
  freeStablePtr spl
  newStablePtr (((mp, fromIntegral deg), realToFrac val):l)

camarshal_startIteratingVariableResultIndex :: StablePtr (DAEIntegrationResult, SimplifiedModel) ->
                                               IO (StablePtr [((VariableID, Int), Int)])
camarshal_startIteratingVariableResultIndex sp = do
  (DAEIntegrationResults { daeResultIndices = resIdx }, _) <- deRefStablePtr sp
  newStablePtr (M.toList resIdx)

camarshal_getNextResultIndexVariableID :: StablePtr [((VariableID, Int), Int)] -> IO CInt
camarshal_getNextResultIndexVariableID spmp =
  (((\(VariableID v) -> fromIntegral v) . fst . fst . head) <$> deRefStablePtr spmp)

camarshal_getNextResultIndexDeriv :: StablePtr [((VariableID, Int), Int)] -> IO CInt
camarshal_getNextResultIndexDeriv spmp =
  (fromIntegral . snd . fst . head) <$> deRefStablePtr spmp

camarshal_getNextResultIndex :: StablePtr [((VariableID, Int), Int)] -> IO CInt
camarshal_getNextResultIndex spmp =
  (fromIntegral . snd . head) <$> deRefStablePtr spmp

camarshal_beginResults :: StablePtr (DAEIntegrationResult, SimplifiedModel) -> IO (StablePtr [(Double, U.UArray Int Double)])
camarshal_beginResults spr =
  newStablePtr =<< (daeResults . fst <$> deRefStablePtr spr)

camarshal_getBvarValue :: StablePtr [(Double, U.UArray Int Double)] -> IO CDouble
camarshal_getBvarValue spl =
  realToFrac . fst . head <$> deRefStablePtr spl

camarshal_getResultLength :: StablePtr [(Double, U.UArray Int Double)] -> IO CInt
camarshal_getResultLength spl =
  fromIntegral . (+1) . snd . U.bounds . snd . head <$> deRefStablePtr spl

camarshal_getResultRow :: StablePtr [(Double, U.UArray Int Double)] -> IO (Ptr CDouble)
camarshal_getResultRow spl =
  newArray =<< ((map (realToFrac :: Double -> CDouble) . U.elems . snd . head) <$> deRefStablePtr spl)
