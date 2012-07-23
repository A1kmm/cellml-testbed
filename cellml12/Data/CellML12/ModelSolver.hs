{-# LANGUAGE PatternGuards, Rank2Types, TypeFamilies, FlexibleContexts, UndecidableInstances, OverloadedStrings, ScopedTypeVariables #-}
module Data.CellML12.ModelSolver where

import Data.CellML12.SupportCode
import Data.CellML12.SimplifiedModel
import Data.CellML12.Parser
import Data.Serialize.Get
import Data.Serialize.Put
import Data.ContentMathML3.Structure
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unwrappable
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import System.IO
import Data.List hiding ((!!))
import Data.Char
import Control.Applicative
import Data.Array.Unboxed as U
import System.IO.Temp
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import System.FilePath
import System.Process
import System.Exit
import Control.Concurrent
import Data.Maybe
import Data.Generics.Uniplate.Operations
import Data.Serialize.IEEE754
import Data.Generics
import Prelude hiding ((!!))
import Data.CellML12.SystemDecomposer
import qualified Debug.Trace

data DAEIntegrationSetup = DAEIntegrationSetup {
    daeModel :: SimplifiedModel,
    daeBoundVariable :: VariableID,
    daeWithGenCode :: LBS.ByteString -> IO () -- const (return ()) to disable.
    }
data DAEIntegrationProblem = DAEIntegrationProblem {
    daeParameterOverrides :: [((VariableID, Int), Double)],
    daeBVarRange :: (Double, Double),
    daeRelativeTolerance :: Double,
    daeAbsoluteTolerance :: Double
  } deriving (Eq, Ord, Show)

data DAEIntegrationResult = DAEIntegrationResults {
  daeResultIndices :: M.Map (VariableID, Int) Int,
  daeResults :: [(Double, U.UArray Int Double)]
                                                  } deriving (Eq, Ord, Show)
data EvaluationPoint = 
  -- | Evaluate using variables available prior to constant evaluation.
  ConstantEvaluation |
  -- | Evaluate using 'time' varying variables.
  -- | Note: 'time' refers to the independent variable over which the problem is
  -- | being integrated, which might not actually be time.
  TimeVaryingEvaluation deriving (Eq, Ord, Show)

data VariableMap = VariableMap { vmapMap :: M.Map (VariableID, Int) (Maybe Int, Maybe Int),
                                 vmapNConsts :: Int,
                                 vmapNVars :: Int
                               } deriving (Eq, Ord, Show)

-- | A classification of a model into a set of time-varying variables, non-time-varying variables,
-- | equations to solve the non-time-varying system, equations to solve the time-varying system,
-- | and inequalities applicable to the non-time-varying and time-varying parts of the model.
type ModelClassification = (S.Set (VariableID, Int, Bool),
                            S.Set (VariableID, Int, Bool),
                            [[Assertion]], [Assertion], [Assertion], [Assertion])

data NumericalEntry = NumericalError String | NumericalSuccess | NumericalData (U.UArray Int Double)
                                                                 deriving (Eq, Ord, Show)

class Monad m => MonadSolver m where
  liftSolver :: Monad m2 => SolverT m2 a -> m a

newtype CodeGen a = CodeGen { runCodeGen :: (LBS.ByteString, LBS.ByteString, Int) ->
                                            Either String (LBS.ByteString, LBS.ByteString, Int, a) }
instance Monad CodeGen where
  return x = CodeGen (\(a,b,c) -> Right (a,b,c,x))
  CodeGen cg1 >>= cg2 = CodeGen (\(a,b,c) ->
                                  case cg1 (a, b, c) of
                                    Right (a', b', c', x) -> (runCodeGen $ cg2 x) (a', b', c')
                                    Left e -> Left e)
  fail e = CodeGen (const $ Left e)

cgAllocateIndex :: CodeGen Int
cgAllocateIndex = CodeGen $ \(s1, s2, idx) -> Right (s1, s2, idx + 1, idx)

cgFunction :: CodeGen a -> CodeGen a
cgFunction (CodeGen cg) = CodeGen $ \(s1, s2, idx) ->
  case (cg ("", s2, idx)) of
    Right (s1', s2', idx', ret) -> Right (s1, s2 `LBS.append` s2' `LBS.append` s1', idx', ret)
    e -> e

cgAppend :: LBS.ByteString -> CodeGen ()
cgAppend bs = CodeGen $ \(s1, s2, idx) -> Right (s1 `LBS.append` bs, s2, idx, ())

type SolverInternalsT m a = ReaderT (Handle, VariableMap, DAEIntegrationSetup) (StateT LBS.ByteString m) a
newtype SolverT m a = SolverT {unsolver :: SolverInternalsT m a }

instance Monad m => Monad (SolverT m) where
  return x = SolverT $ return x
  (SolverT x) >>= f = SolverT $ x >>= (\v -> unsolver (f v))
instance MonadTrans SolverT where
  lift x = SolverT (lift . lift $ x)
instance MonadIO m => MonadIO (SolverT m) where
  liftIO = lift . liftIO
instance (MonadIO m, MonadIOUnwrappable (ReaderT (Handle, VariableMap, DAEIntegrationSetup) (StateT LBS.ByteString m))) =>
         MonadIOUnwrappable (SolverT m) where
  type MonadIOWrapType (SolverT m) = MonadIOWrapType (ReaderT (Handle, VariableMap, DAEIntegrationSetup) (StateT LBS.ByteString m))
  type MonadIOStateType (SolverT m) = MonadIOStateType (ReaderT (Handle, VariableMap, DAEIntegrationSetup) (StateT LBS.ByteString m))
  unwrapState = SolverT unwrapState
  unwrapMonadIO s (SolverT m) = unwrapMonadIO s m
  rewrapMonadIO s v = SolverT (rewrapMonadIO s v)
instance Monad m => MonadReader (SolverT m) where
  type EnvType (SolverT m) = EnvType (ReaderT (Handle, VariableMap, DAEIntegrationSetup) (StateT LBS.ByteString m))
  ask = SolverT ask
  local f (SolverT m) = SolverT (local f m)
instance Monad m => MonadState (SolverT m) where
  type StateType (SolverT m) = StateType (ReaderT (Handle, VariableMap, DAEIntegrationSetup) (StateT LBS.ByteString m))
  get = SolverT get
  put v = SolverT (put v)

solveModelWithParameters :: DAEIntegrationProblem -> SolverT (ErrorT String IO) DAEIntegrationResult
solveModelWithParameters p = do
  (hOut, vm@(VariableMap (vmap :: M.Map (VariableID, Int) (Maybe Int, Maybe Int)) _ _), setup) <- ask
  let iBound = fromJust . fst $ vmap !! (daeBoundVariable setup, 0)
  
  -- Send the request for the problem to be solved...
  liftIO . BS.hPutStr hOut $ runPut $ do
    -- Command...
    putWord8 0
    -- Overrides...
    let pOverrides = mapMaybe (\(vardeg, newv) -> liftM (\v -> (v, newv)) $ M.lookup vardeg vmap) . daeParameterOverrides $ p
    flip putListOf pOverrides $ putTwoOf (putWord32host . fromIntegral . (\(a, b) -> fromJust $ a `mplus` b)) (putFloat64le)
    -- Bvar range...
    putTwoOf putFloat64le putFloat64le . daeBVarRange $ p
    putFloat64le . daeRelativeTolerance $ p
    putFloat64le . daeAbsoluteTolerance $ p
  liftIO (hFlush hOut)
  
  -- Now retrieve the response from the solver...
  s0 <- get
  (numericalResult, rest) <- lift . ErrorT . return $ flip runGetLazyState s0 $ getOutput $ M.size vmap
  put rest
  case last numericalResult of
    NumericalError str -> fail $ "Numerical problem: " ++ str
    NumericalSuccess -> do
      let r = (map (\x -> (x!iBound, x)) . mapMaybe onlyData $ numericalResult)
      return $ DAEIntegrationResults { daeResultIndices = M.map (\(a, b) -> fromJust $ a `mplus` b) vmap, daeResults = r }
    _ -> fail "Incomplete results - solver program may have crashed"

-- | Runs a solver monad over a particular model with a particular setup.
runSolverOnDAESimplifiedModel :: SimplifiedModel -> DAEIntegrationSetup -> SolverT (ErrorT String IO) a -> IO (Either String a)
runSolverOnDAESimplifiedModel m' setup solver = runErrorT $ do
  mSimp1 <- simplifyModel m'
  case fixHighers mSimp1 setup of
    Left e -> fail e
    Right mFull -> do
      let mUnits = substituteUnits mFull
      -- Simplify again after putting units conversions in. We need to do it
      -- twice, because we need to simplify derivative degrees to get the units, but
      -- the units conversions might be able to be simplified out.
      mSimp2 <- simplifyModel mUnits
      c@(constVars, varVars, eqnConst, eqnVary, ieqConst, ieqVary) <- classifyVariablesAndAssertions setup mSimp2
      when (any (\(_, _, isIV) -> isIV) . S.toList $ varVars) $
        fail "Model contains initial values that can't be computed independently of time"
      let vmap = assignIndicesToVariables setup mSimp2 constVars varVars
      code <- ErrorT (return (writeDAECode vmap mSimp2 setup c))
      liftIO $ daeWithGenCode setup code
      withSystemTempDirectory' "daesolveXXX" $ \fn -> do
        liftIO $ LBS.writeFile (fn </> "solve.c") code
        ec <- liftIO $ system $ showString "gcc -ggdb -O0 -Wall " . showString (fn </> "solve.c") . showString " -lsundials_ida -lsundials_kinsol -lsundials_nvecserial -lblas -llapack " . showString " -o " $ fn </> "solve"
        if ec /= ExitSuccess then fail "Cannot find C compiler" else do
        (Just i, Just o, _, p) <-
          liftIO $ createProcess $
            CreateProcess { cmdspec = RawCommand (fn </> "solve") [], cwd = Nothing,
                            env = Nothing, std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit,
                            close_fds = False, create_group = False }
        ecMvar <- liftIO newEmptyMVar
        liftIO $ forkOS $ do
          ec' <- waitForProcess p
          putMVar ecMvar ec'
        lStr <- liftIO $ LBS.hGetContents o
        ret <- evalStateT (runReaderT (unsolver solver) (i, vmap, setup)) lStr
        liftIO $ BS.hPutStr i (BS.pack "\x01") -- Send exit command.
        liftIO $ hFlush i
        ec' <- liftIO $ takeMVar ecMvar
        when (ec' /= ExitSuccess) $ fail "Problem executing integrator program"
        return ret

extractPiecewisePiece (Apply op [arg1, arg2])
  | opIs "piece1" "piece" op = Just (stripSemCom arg1, stripSemCom arg2)
extractPiecewisePiece _ = Nothing
extractPiecewiseOtherwise (Apply op [arg])
  | opIs "piece1" "otherwise" op = Just arg
extractPiecewiseOtherwise _ = Nothing

-- | Convert a mathematical expression into a ByteString containing C code for
-- | evaluating that expression.
putExpression :: EvaluationPoint -> VariableMap -> M.Map String (TypeName, Maybe CanonicalUnits, VariableID) -> AST -> CodeGen ()

putExpression ep vmap ctx x
  | Just (varname, deg, isIV) <- tryGetVariableRef x = do
    (_, _, var) <-
      maybe (fail $ "Variable " ++ varname ++ " not found but named in ci") return $
        M.lookup varname ctx
    str <- maybe (fail "Cannot find C name for variable") return $
             getVariableString (if isIV then TimeVaryingEvaluation else ep) vmap (var, deg)
    cgAppend . LBS.pack $ str

putExpression ep vmap ctx (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just cd) name))) operands)
  | cd == "logic1" = putLogicExpression
  | cd == "piece1" = putPieceExpression
  | cd == "relation1" = putRelationExpression
  | cd == "arith1" = putArithExpression
  | cd == "transc1" = putTranscExpression
  | cd == "rounding1" = putRoundingExpression
  | otherwise = fail $ "Unrecognised content dictiorary " ++ cd ++ "in apply operator csymbol"
  where
    putLogicExpression
      | name == "equivalent" = binaryOperator "((" ")==(" "))"
      | name == "not" = unaryOperator "(!(" "))"
      | name == "and" = naryOperator "((" ")&&(" "))"
      | name == "xor" = naryOperator "(((" ")?1:0) ^ ((" ")?1:0))"
      | name == "or" = naryOperator "((" ") || (" "))"
      | name == "implies" = binaryOperator "((!(" ")) || (" "))"
      | otherwise = fail $ "Unexpected logic1 operator" ++ name
    putPieceExpression
      | name == "piecewise" = do
        (pieces, otherwise) <-
          flip (flip foldM ([], Nothing)) operands $ \(pieces, otherwise) operand ->
            case extractPiecewisePiece (stripSemCom operand) of
              Just p -> return $ (p:pieces, otherwise)
              Nothing -> case extractPiecewiseOtherwise (stripSemCom operand) of
                o@(Just _) -> return $ (pieces, otherwise `mplus` o)
                Nothing -> fail "Piecewise contains operands other than pieces and otherwise"
        cgAppend "("
        forM_ pieces $ \(val, cond) -> do
          cgAppend "("
          putExpression ep vmap ctx cond
          cgAppend ")?("
          putExpression ep vmap ctx val
          cgAppend "):"
        cgAppend "("
        case otherwise of
          Just o -> putExpression ep vmap ctx (stripSemCom o)
          Nothing -> cgAppend "0.0/0.0"
        cgAppend "))"
      | otherwise = fail $ "Unknown piecewise expression " ++ name
    putRelationExpression
      | name == "eq" = binaryOperator "((" ")==(" "))"
      | name == "lt" = binaryOperator "((" ")<(" "))"
      | name == "gt" = binaryOperator "((" ")>(" "))"
      | name == "neq" = binaryOperator "((" ")!=(" "))"
      | name == "leq" = binaryOperator "((" ")<=(" "))"
      | name == "geq" = binaryOperator "((" ")>=(" "))"
      | otherwise = fail $ "Unknown relation1 symbol " ++ name
    putArithExpression
      | name == "lcm" = naryOperator ("lcm(" `LBS.append` (LBS.pack . show . length $ operands)
                                      `LBS.append` ", ") "," ")"
      | name == "gcd" = naryOperator ("gcd(" `LBS.append` (LBS.pack . show . length $ operands)
                                      `LBS.append` ", ") "," ")"
      | name == "plus" = naryOperator "((" ")+(" "))"
      | name == "unary_minus" = unaryOperator "(-(" "))"
      | name == "minus" = binaryOperator "((" ")-(" "))"
      | name == "times" = naryOperator "((" ")*(" "))"
      | name == "divide" = binaryOperator "((" ")/(" "))"
      | name == "power" = binaryOperator "pow(" "," ")"
      | name == "abs" = unaryOperator "abs(" ")"
      | name == "root" = binaryOperator "pow(" ", 1.0/(" "))"
      | otherwise = fail $ "Unknown arith1 operator " ++ name
    basicTrig = ["sin", "cos", "tan"]
    invTrig = ["csc", "sec", "cot"]
    putTranscExpression
      -- XXX we could simplify the constant base case
      | name == "log" = binaryOperatorFlipped "(log(" ")/log(" "))"
      | name == "ln" = unaryOperator "log(" ")"
      | name == "exp" = unaryOperator "exp(" ")"
      | name `S.member` (S.fromList basicTrig) = unaryOperator (LBS.pack $ name ++ "(") ")"
      | name `S.member` (S.fromList $ map (++"h") basicTrig) = unaryOperator (LBS.pack $ name ++ "h(") ")"
      | name `S.member` (S.fromList invTrig) =
        unaryOperator (LBS.pack $ "(1.0 / " ++ ((M.fromList $ zip invTrig basicTrig)!!name) ++ "(") "))"
      | name `S.member` (S.fromList $ map (++"h") invTrig) =
        unaryOperator (LBS.pack $ "(1.0 / " ++ (((M.fromList $ zip invTrig basicTrig)!!name) ++ "h") ++ "(") "))"
      | name `S.member` (S.fromList $ map ("arc"++) basicTrig) =
        unaryOperator (LBS.pack $ "a" ++ name ++ "(") ")"
      | name `S.member` (S.fromList $ map (\x -> "arc" ++ x ++ "h") basicTrig) =
        unaryOperator (LBS.pack $ "arc" ++ name ++ "h(") ")"
      | name `S.member` (S.fromList $ map ("arc"++) invTrig) =
        unaryOperator (LBS.pack $ "a" ++ ((M.fromList $ zip invTrig basicTrig)!!name) ++ "(1.0 / (") "))"
      | name `S.member` (S.fromList $ map (\x -> "arc" ++ x ++"h") invTrig) =
        unaryOperator (LBS.pack $ ("a" ++ ((M.fromList $ zip invTrig basicTrig)!!name) ++ "h") ++ "(1.0 / (") "))"
      | otherwise = fail $ "Unknown transc1 operator " ++ name
    putRoundingExpression
      | name == "ceiling" = unaryOperator "ceil(" ")"
      | name == "floor" = unaryOperator "floor(" ")"
      | name == "trunc" = unaryOperator "trunc(" ")"
      | name == "round" = unaryOperator "round(" ")"
      | otherwise = fail $ "Unknown rounding1 operator " ++ name
    unaryOperator a b
      | [operand] <- operands = do
        cgAppend a
        putExpression ep vmap ctx (stripSemCom operand)
        cgAppend b
      | otherwise = fail "Unary operator must have exactly one argument"
    binaryOperator a b c
      | [op1, op2] <- operands = do
        cgAppend a
        putExpression ep vmap ctx (stripSemCom op1)
        cgAppend b
        putExpression ep vmap ctx (stripSemCom op2)
        cgAppend c
      | otherwise = fail "Binary operator must have exactly one argument"
    binaryOperatorFlipped a b c
      | [op2, op1] <- operands = do
        cgAppend a
        putExpression ep vmap ctx (stripSemCom op1)
        cgAppend b
        putExpression ep vmap ctx (stripSemCom op2)
        cgAppend c
      | otherwise = fail "Binary operator must have exactly one argument"
    naryOperator a b c = do
        cgAppend a
        forM (zip [0..] operands) $ \(i, operand) -> do
          when (i /= 0) $ cgAppend b
          putExpression ep vmap ctx (stripSemCom operand)
        cgAppend c

putExpression _ _ _ (Apply op _) = fail $ "Apply where operator is not csymbol or other recognised form but is " ++ show op

putExpression _ _ _ (Bind _ _ _) = fail "Unexpected bind in expression"
  
-- We don't handle e, gamma, pi from nums1 since the simplifier has already
-- converted them to numerical constants.
putExpression _ _ _ (Csymbol cd name)
  | Just "logic1" <- cd, name == "true" = cgAppend "1"
  | Just "logic1" <- cd, name == "false" = cgAppend "0"
  | otherwise =
  fail $ "Encountered csymbol with content dictionary" ++ (show cd) ++
         " and name " ++ name ++ "."

putExpression _ _ _ (Cs str) =
  fail $ "Unexpected MathML cs element with contents " ++ str

putExpression ep vmap ctx (Error{}) = fail "Unexpected MathML <error> element"

putExpression _ _ _ (CBytes str) =
  fail $ "Unexpected MathML cbytes element with contents " ++ str

putExpression _ _ _ (Cn (CnInteger i)) = cgAppend . LBS.pack $ show i
putExpression _ _ _ (Cn (CnReal r)) = cgAppend . LBS.pack $ show r
putExpression _ _ _ (Cn (CnDouble r)) = cgAppend . LBS.pack $ show r
putExpression _ _ _ (Cn (CnHexDouble r)) = cgAppend . LBS.pack $ show r

putExpression _ _ _ ex = fail ("Unhandled MathML expression " ++ show ex)

-- | Assign a variable to each index in the model. Note that in the case where
-- | a derivative is constant, it actually gets two indices, one as a constant,
-- | and one as the rate corresponding to the state variable.
-- | Produces a map from (variable, degree) to
-- | (Maybe constantIndex, Maybe timeVaryingIndex)
assignIndicesToVariables :: DAEIntegrationSetup -> SimplifiedModel ->
                            S.Set (VariableID, Int, Bool) -> S.Set (VariableID, Int, Bool) ->
                            VariableMap
assignIndicesToVariables setup m constVars varVars =
  let
    bvarVarying = S.map (\(v, _, _) -> v) varVars
    constVarsExcludingIVs = S.map (\(a,b,_) -> (a,b)) $
                            S.filter (\(_, _, isConst) -> not isConst) constVars
    nConsts = S.size constVarsExcludingIVs
    otherVars = M.fromList $ (((daeBoundVariable setup, 0), (Nothing, Just 0)):
                              (zip (S.toList constVarsExcludingIVs) [(Just i, Nothing) | i <- [1..nConsts]]))
    varsPair' = (S.map (\(a, b, _) -> (a, b)) varVars)
    -- Every state corresponding to a rate becomes a variable...
    varsPair = (S.map (\(a, b) -> (a, b - 1)) . S.filter (\(_, b) -> b > 0) $ varsPair' `S.union` constVarsExcludingIVs)
               `S.union` varsPair'
    nVars = S.size . S.map fst $ varsPair
    varsRates = (M.fromList (zip (S.toList varsPair) [(Nothing, Just i) | i <- [(nConsts + 1)..(nConsts + nVars)]])) `M.union`
                (M.fromList (zip (S.toList $ S.map (\(a, b) -> (a, b + 1)) varsPair)
                                 [(Nothing, Just i) | i <- [(nConsts + nVars + 1)..(nConsts + nVars * 2)]]))
  in
   VariableMap { vmapMap = M.unionWith (\(a1, b1) (a2, b2) -> (a1 `mplus` a2, b2 `mplus` b1)) otherVars varsRates,
                 vmapNConsts = nConsts, vmapNVars = nVars }

-- | Gets the index in the work array appropriate for a given stage of the evaluation.
getWorkArrayIndex :: EvaluationPoint -> VariableMap -> (VariableID, Int) -> Maybe Int
getWorkArrayIndex ConstantEvaluation (VariableMap { vmapMap = m }) v =
  M.lookup v m >>= fst
getWorkArrayIndex TimeVaryingEvaluation (VariableMap { vmapMap = m }) v =
  M.lookup v m >>= (\(a, b) -> b `mplus` a)

getVariableString :: EvaluationPoint -> VariableMap -> (VariableID, Int) -> Maybe String
getVariableString ep vm@(VariableMap { vmapNConsts = nc, vmapNVars = nv }) v = do
  varIndex <- getWorkArrayIndex ep vm v
  case varIndex of
    0 -> return "t"
    x | x <= nc -> return $ "CONSTANTS[" ++ (show (varIndex - 1) ++ "]")
      | x <= (nc + nv) -> return $ "STATES[" ++ (show (varIndex - 1 - nc) ++ "]")
      | otherwise -> return $ "RATES[" ++ (show (varIndex - 1 - nc - nv) ++ "]")

conversionFactor convertFrom@(CanonicalUnits fromOffs fromMup _) convertTo@(CanonicalUnits toOffs toMup _) =
  ((fromMup / toMup) * fromOffs - toOffs, fromMup / toMup)

convertReal from to v = let (a,m) = conversionFactor from to in
  v * m + a

-- | Put all variables into consistent units
substituteUnits :: SimplifiedModel -> SimplifiedModel
substituteUnits sm@(SimplifiedModel { variableInfo = variableInfo, assertions = assertions }) =
  SimplifiedModel { assertions = assertions', variableInfo = VariableInfo variableInfo' }
  where
    mapSnd g = map (\(f,s) -> (f, g s))
    liftSndMaybe (_, Nothing) = Nothing
    liftSndMaybe (x, Just y) = Just (x, y)
    chosenUnitsForVar :: M.Map VariableID CanonicalUnits
    chosenUnitsForVar = M.fromList . mapMaybe liftSndMaybe . mapSnd snd . mapMaybe liftSndMaybe .
                                     mapSnd listToMaybe . M.toList . unvariableInfo $ variableInfo
    variableInfo' = M.mapWithKey (\v muList -> case M.lookup v chosenUnitsForVar of
                                     Nothing -> muList
                                     Just newu -> mapSnd (liftM (const newu)) muList) . unvariableInfo $ variableInfo
    assertions' = map substituteUnitsInAssertion assertions
    substituteUnitsInAssertion (Assertion (expr, AssertionContext origMap)) =
      let
        newMap = M.map (\(typeName, mcu, varid) -> (typeName, mcu >>= (\cu -> M.lookup varid chosenUnitsForVar), varid)) origMap
        conversionFactors = M.fromList .
                            mapMaybe (\(n, (_, mcu, varid)) ->
                                       mcu >>= (\cu -> Just (n, conversionFactor cu (chosenUnitsForVar !! varid)))) .
                            M.toList $ newMap
        considerApply cd op ignoreVal actualVal expr
          | abs (actualVal - ignoreVal) < 1E-6 = expr
          | otherwise = Apply (noSemCom (Csymbol (Just cd) op)) [noSemCom expr, noSemCom . Cn . CnReal $ actualVal]
        convertOneVariable expr (offs, mup) =
          considerApply "arith1" "plus" 0 offs . considerApply "arith1" "times" 1 mup $ expr
        ignoringSemCom f (WithMaybeSemantics s (WithCommon c x)) = WithMaybeSemantics s (WithCommon c (f x))
        applyConversions bvar x@(ASTCi (Ci str))
          | not (str `S.member` bvar) = maybe x (convertOneVariable x) (M.lookup str conversionFactors)
        applyConversions bvar expr@(Apply (WithMaybeSemantics _ (WithCommon _ (
                                         Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "calculus1") "diff")))
                                           [WithMaybeSemantics _ (WithCommon _ (
                                            Bind (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "fns1") "lambda")))
                                                 _ (WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci varName))))))]
                                                                              )))
                                         [WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci bvarName)))])
          | Just (varOffs, varMup) <- M.lookup varName conversionFactors,
            Just (_, bvarMup) <- M.lookup bvarName conversionFactors = convertOneVariable expr (varOffs, varMup / bvarMup)
        applyConversions bvar (Apply op operands) = Apply op (map (ignoringSemCom (applyConversions bvar)) operands)
        applyConversions bvar (Bind op newBvar operand) =
          let
            bvar' = foldl' (\m (WithMaybeSemantics _ (WithCommon _ (Ci str))) -> S.insert str m) bvar newBvar
          in
           Bind op newBvar (ignoringSemCom (applyConversions bvar') operand)
        applyConversions _ x = x
      in
       Assertion (ignoringSemCom (applyConversions S.empty) expr, AssertionContext newMap)

writeDAECode :: VariableMap -> SimplifiedModel -> DAEIntegrationSetup -> ModelClassification -> Either String LBS.ByteString
writeDAECode vmap model problem classification =
  let
    (nresidInit, residInitialisation) = dofsAffectingInitialResidual vmap classification
    lenDefines = LBS.pack ((showString "#define NCONSTS " . shows (vmapNConsts vmap) . showString"\n#define NVARS " .
                            shows (vmapNVars vmap) . showString "\n#define NRESIDINITVARS " . shows nresidInit) "\n")
    ret =
      flip runCodeGen ("", "", 0) $ do
        cgAppend daeInitialPrefix
        putModelSolveConsts vmap model problem classification
        cgAppend daeInitialSuffix
        
        cgAppend daeResidualPrefix
        putModelResiduals vmap model problem classification
        cgAppend daeResidualSuffix
        
        cgAppend paramToStatePrefix
        cgAppend residInitialisation
        cgAppend paramToStateSuffix
        
        cgAppend daeJacobianPrefix
        putModelJacobian vmap model problem classification
        cgAppend daeJacobianSuffix
        -- To do: uncertain starting parameters.
  in
   case ret of
     Right (mainCode, preFuncs, _, _) ->
       Right $ lenDefines `LBS.append` daeBoilerplateCode `LBS.append` preFuncs `LBS.append` mainCode
     Left e -> Left e

dofsAffectingInitialResidual vm@(VariableMap { vmapMap = m }) (constv, varying, _, _, _, _) =
  let vDeg = 
        foldl' (\m (v,deg,_) -> M.alter (\deg0 -> Just $ max (fromMaybe 0 deg0) deg) v m)
          M.empty (S.toList $ S.filter (\(v,d,_) -> isJust . snd $ m!!(v,d)) $ constv `S.union` varying)
  in
   (M.size vDeg,
    LBS.fromChunks $ map (\(i, vv) -> BS.pack $
                                        (fromMaybe "" $ getVariableString TimeVaryingEvaluation vm vv) ++
                                        " = params[" ++ show i ++ "];\n")
                         (zip [0..] $ M.toList vDeg))

putModelSolveConsts vmap model@(SimplifiedModel { assertions = assertions }) p
                    (constVars, varVars, eqnConst, eqnVary, ieqConst, ieqVary) = do
  -- Solve the system...
  foldM_ (solveSystem ConstantEvaluation vmap) S.empty eqnConst
  -- Check the inequalities hold. XXX should split them up and use them as solver
  -- constraints as soon as all variables in them are known instead.
  mapM_ (checkInequality ConstantEvaluation vmap) ieqConst
  cgAppend "return 0;\n"

-- | Solve a system of one or more equations.
-- | We have two cases: Solve by assignment of the variable on one side to the
-- | evaluation of the expression on the other, and solve using the DoSolve
-- | non-linear solver.
solveSystem ep vmap known [Assertion (WithMaybeSemantics _ (WithCommon _ (Apply _ [a, b])), AssertionContext ctx)]
  | Just (vname, deg, iv) <- tryGetVariableRef (stripSemCom a),
    Just (_, _, v) <- M.lookup vname ctx,
    not (S.member (v, deg, iv) known),
    Just vCode <- getVariableString (if iv then TimeVaryingEvaluation else ep) vmap (v, deg) = do
      cgAppend $ LBS.pack vCode
      cgAppend " = "
      putExpression ep vmap ctx (stripSemCom b)
      cgAppend ";\n"
      return $ S.insert (v, deg, iv) known
  | Just (vname, deg, iv) <- tryGetVariableRef (stripSemCom b),
    Just (_, _, v) <- M.lookup vname ctx,
    not (S.member (v, deg, iv) known), 
    Just vCode <- getVariableString (if iv then TimeVaryingEvaluation else ep) vmap (v, deg) = do
      cgAppend $ LBS.pack vCode
      cgAppend " = "
      putExpression ep vmap ctx (stripSemCom a)
      cgAppend ";\n"
      return $ S.insert (v, deg, iv) known
solveSystem ep vmap known eqns = do
  fIdx <- cgAllocateIndex
  let fnName = "SolveSystem" `LBS.append` (LBS.pack . show $ fIdx)
  let allVars = S.unions $ map (S.fromList . findAssertionVariableUsage) eqns
  let newVars = allVars `S.difference` known
  let copyParams =
        forM_ (zip [0..] (S.toList newVars)) $ \(param, (v, deg, iv)) -> do
          vName <- maybe (fail $ "Variable " ++ show (v, deg) ++ " in equation not in variable map " ++ (show vmap)) return $
                     getVariableString (if iv then TimeVaryingEvaluation else ep) vmap (v, deg)
          cgAppend (LBS.pack vName)
          cgAppend " = params["
          cgAppend . LBS.pack . show $ param
          cgAppend "];\n"
  cgFunction $ do
    cgAppend "int "
    cgAppend fnName
    cgAppend "(void* allState, double* params, double* residuals)\n{\n"
    cgAppend "UNBUNDLE_ARRAYS\n"
    copyParams
    forM_ (zip [0..] eqns) $ \(i, Assertion (WithMaybeSemantics _ (WithCommon _ (Apply _ [a, b])), AssertionContext ctx)) -> do
      cgAppend "residuals["
      cgAppend . LBS.pack . show $ i
      cgAppend "] = "
      putExpression ep vmap ctx (Apply (noSemCom (Csymbol (Just "arith1") "minus")) [a, b])
      cgAppend ";\n"
      cgAppend "return 0;\n"
    cgAppend "}\n"
  cgAppend "{"
  cgAppend "struct ArrayBundle b = { CONSTANTS, STATES, RATES };"
  cgAppend "params = DoSolve(&b, "
  cgAppend . LBS.pack . show $ S.size newVars
  cgAppend ", "
  cgAppend fnName
  cgAppend ");"
  copyParams
  cgAppend "free(params);\n"
  cgAppend "}"
  return (allVars `S.union` known)

checkInequality ep vmap (Assertion (ieq, AssertionContext ctx)) = do
  cgAppend "if (!("
  putExpression ep vmap ctx (stripSemCom ieq)
  cgAppend ")) return 1;\n"

putModelResiduals vmap@(VariableMap { vmapMap = m }) model problem (constVars, varVars, eqnConst, eqnVary, ieqConst, ieqVary) = do
  forM_ (zip [0..] eqnVary) $ \(i, Assertion (WithMaybeSemantics _ (WithCommon _ (Apply _ [a, b])), AssertionContext ctx)) -> do
    cgAppend "residuals["
    cgAppend . LBS.pack . show $ i
    cgAppend "] = "
    putExpression TimeVaryingEvaluation vmap ctx (Apply (noSemCom (Csymbol (Just "arith1") "minus")) [a, b])
    cgAppend ";\n"
  let needsCopy x
        | Just (Just _, Just _) <- mx = True
        | otherwise = False
        where mx = M.lookup x m
  forM_ (zip [length eqnVary..] (filter (\((v, deg,iv)) -> not iv && needsCopy (v, deg)) . S.toList $ constVars `S.union` varVars)) $
    \(i, (v, deg, _)) -> do
      cgAppend "residuals["
      cgAppend . LBS.pack . show $ i
      cgAppend "] = ("
      cgAppend . LBS.pack . fromJust $ getVariableString TimeVaryingEvaluation vmap (v, deg)
      cgAppend ") - ("
      cgAppend . LBS.pack . fromJust $ getVariableString ConstantEvaluation vmap (v, deg)
      cgAppend ")"
  if null ieqVary then cgAppend "return 0;\n"
    else do
      cgAppend "return (("
      forM_ (zip [0..] ieqVary) $ \(i, Assertion (ieq, AssertionContext ctx)) -> do
        when (i /= 0) (cgAppend ") && (")
        putExpression TimeVaryingEvaluation vmap ctx (stripSemCom ieq)
      cgAppend ")) ? 0 : 1;\n"

putModelJacobian vmap@(VariableMap { vmapNConsts = nconsts, vmapMap = m }) model problem (constVars, varVars, eqnConst, eqnVary, ieqConst, ieqVary) = do
  forM_ (zip [0..] eqnVary) $ \(i, Assertion (WithMaybeSemantics _ (WithCommon _ (Apply _ [a, b])), AssertionContext ctx)) -> do
    cgAppend . LBS.pack $ "Jv[" ++ show i ++ "] = "
    forM_ (zip [0..] $ filter (\(v, d, iv) -> d == 0 && not iv) $ S.toList varVars) $ \(j, (v, d, _)) -> do
      ex <- either fail return . runIdentity . runErrorT $
            (repeatedlySimplify ctx) =<< (takePartialDerivativeEx ctx (v,d)
                                          (Apply (noSemCom (Csymbol (Just "arith1") "minus")) [a, b]))
      when (j /= 0) $ cgAppend " + "
      let Just (_, Just jVar) = M.lookup (v, d) m
      cgAppend . LBS.pack $ "v[" ++ show (jVar - 1 - nconsts) ++ "] * ("
      putExpression TimeVaryingEvaluation vmap ctx ex
      cgAppend ")"
    cgAppend ";\n"
  let needsCopy x
        | Just (Just _, Just _) <- mx = True
        | otherwise = False
        where mx = M.lookup x m
  forM_ (zip [length eqnVary..] (filter (\((v, deg, iv)) -> not iv && needsCopy (v, deg)) . S.toList $ constVars `S.union` varVars)) $
    \(i, (v, d, _)) -> do
      cgAppend . LBS.pack $ "Jv[" ++ show i ++ "] = "
      case M.lookup (v, d) m of
        Just (_, Just jVar) -> cgAppend . LBS.pack $ "v[" ++ show jVar ++ "];\n"
        _ -> cgAppend "0;\n"

repeatedlySimplify :: (Monad m, Functor m) => M.Map String (TypeName, Maybe CanonicalUnits, VariableID) -> AST -> ErrorT String m AST
repeatedlySimplify m ex = fst <$> (flip (untilM (return . snd)) (ex, False) $ \(x, _) -> do
  let x' = transformBi (simplificationRules m) x
  x'' <- simplifyMathAST m x'
  return (x'', x''==x))

-- | A getter for parsing the output from the simulator.
getOutput :: Int -> Get [NumericalEntry]
getOutput l = do
  eType <- getWord8
  case eType of
    0 -> liftM (\x -> [NumericalError x]) $ do
      eLength <- getWord8
      eStr <- getBytes (fromIntegral eLength)
      return $ BS.unpack eStr
    1 -> return [NumericalSuccess]
    2 -> (liftM NumericalData $ do
             liftM (listArray (0, (l-1))) (mapM (const $ getFloat64le)
                                           [1..l])) >>=
         (\v -> getOutput l >>= \l -> return (v:l))

-- TODO boolean variable handling.
-- | Classify all assertions and model variables[*] into four categories:
-- |   1. Equations that include no 'time' dependent variables (including i.v.s).
-- |   2. Equations that include 'time' dependent variables.
-- |   3. Inequalities that include no 'time' dependent variables.
-- |   4. Inequalities that include 'time' dependent variables.
-- | Also returns the set of variables that don't vary with 'time'.
-- |  * Model variables here include the derivative degree, and treat
-- |    connected CellML variables as the same - not to be confused with
-- |    CellML variables.
classifyVariablesAndAssertions :: Monad m => DAEIntegrationSetup -> SimplifiedModel -> ErrorT String m ModelClassification
classifyVariablesAndAssertions (DAEIntegrationSetup { daeBoundVariable = bv }) (SimplifiedModel { assertions = assertions }) =
  let
    (equations, inequalities) = partition isAssertionEquation assertions
    usageList = map (S.fromList . findAssertionVariableUsage) equations
    allVars = S.unions usageList
    allVarsNoTime = S.delete (bv, 0, False) allVars
    allVarsNoStates = S.filter (\(v, d, iv) -> iv || d > 0 ||
                                               not ((v, d + 1, False) `S.member` allVarsNoTime)) allVarsNoTime
    -- Note: for the purposes of the usage list, IVs and rates do not depend on
    -- 'time', we are only looking for explicit time dependencies.
    equationsNoTime = filter (\(_, vu) -> not $ (bv, 0, False) `S.member` vu) $
                      zip equations usageList
    allVarsNoStatesL = S.toList allVarsNoStates
    nEqns = length equations
    nVarsNoTime = S.size allVarsNoTime
    nVarsNoStates = S.size allVarsNoStates
    nEqnsNoTime = length equationsNoTime
    eqnStructure :: U.UArray (Equation, Variable) Bool
    eqnStructure = U.array ((0,0), (nEqnsNoTime - 1, nVarsNoStates - 1))
                           (concatMap (\(eqn, (_, s)) -> [((eqn, varID), S.member var s) | (var, varID) <- zip allVarsNoStatesL [0..]])
                                      (zip [0..] equationsNoTime))
    allEqnNos = S.fromList [0..(nEqnsNoTime - 1)]
    (constEqns, constVars) = 
      foldl' (\(l1, l2) (a, b) -> (a:l1, b:l2)) ([], []) $
        smallestDecompose eqnStructure allEqnNos (S.fromList [0..(nVarsNoStates - 1)])
    varMap = M.fromList (zip [0..] allVarsNoStatesL)
    eqnMap = M.fromList (zip [0..] (map fst equationsNoTime))
    constVarSet = S.fromList . map (varMap!!) . concat $ constVars
    varSet = allVarsNoTime `S.difference` constVarSet
    constEqns' = map (map (eqnMap!!)) constEqns
    varEqns = S.toList (S.fromList equations `S.difference` (S.fromList . concat $ constEqns'))
    partitionByUsage =
      partition (all (\var@(_, _, isIV) -> isIV || S.member var constVarSet) . findAssertionVariableUsage)
    (ieqConst, ieqVary) = partitionByUsage inequalities
  in
   if nEqns == nVarsNoStates then
     return (constVarSet, varSet, reverse constEqns', varEqns, ieqConst, ieqVary)
   else fail $ "Trying to solve a model with " ++ (show nEqns) ++ " equations but " ++ (show nVarsNoTime) ++ " unknowns."

isAssertionEquation (Assertion (WithMaybeSemantics _ (WithCommon _
                      (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "relation1") "eq"))) _)), _)) = True
isAssertionEquation _ = False

-- | Finds, within an assertion, all variables, classified by whether they are an
-- | initial value or not, and their degree.
findAssertionVariableUsage :: Assertion -> [(VariableID, Int, Bool)]
findAssertionVariableUsage (Assertion (ex, AssertionContext vmap)) =
  findExpressionVariableUsage (M.map (\(_, _, v) -> v) vmap) (stripSemCom ex)

findExpressionVariableUsage nameToVar (ASTCi (Ci varName))
  | Just varid <- M.lookup varName nameToVar = [(varid, 0, False)]

findExpressionVariableUsage nameToVar (Apply op [expr])
  | opIs "calculus1" "diff" op,
    Just varName <- tryGetLambdaVariable (stripSemCom expr),
    Just varid <- M.lookup varName nameToVar = [(varid, 1, False)]

findExpressionVariableUsage nameToVar (Apply op [boundE, whenE, varE])
  | opIs "cellml1" "evaluatedAt" op,
    -- XXX we really should check all whenE values are consistent, and that
    -- boundE is the bound variable the problem is over.
    Just whenc <- tryGetConstant whenE,
    ASTCi (Ci varName) <- stripSemCom varE,
    Just varid <- M.lookup varName nameToVar = [(varid, 0, True)]

findExpressionVariableUsage nameToVar (Apply op [boundE, whenE, WithMaybeSemantics _ (WithCommon _ (Apply op2 [expr]))])
  | opIs "cellml1" "evaluatedAt" op,
    opIs "calculus1" "diff" op2,
    -- XXX we really should check all whenE values are consistent, and that
    -- boundE is the bound variable the problem is over.
    Just whenc <- tryGetConstant whenE,
    ASTCi (Ci varName) <- stripSemCom expr,
    Just varid <- M.lookup varName nameToVar = [(varid, 1, True)]

findExpressionVariableUsage nameToVar (Bind op bvars expression) =
  let
    nameToVar' = foldl' (\nameToVar' (WithMaybeSemantics _ (WithCommon _ (Ci bvarName))) 
                              -> M.delete bvarName nameToVar') nameToVar bvars
  in
   findExpressionVariableUsage nameToVar (stripSemCom op) ++
   findExpressionVariableUsage nameToVar' (stripSemCom op)

findExpressionVariableUsage nameToVar (Apply op exprs) =
  findExpressionVariableUsage nameToVar (stripSemCom op) ++
  (concat $ map (findExpressionVariableUsage nameToVar . stripSemCom) exprs)

findExpressionVariableUsage _ _ = []

-- TODO: Nothing in the secondary spec says that derivatives can only be taken
-- on variables. Need a rule to substitute e.g. d(2x^2)/dt with y=2x^2, dy/dt
fixHighers :: SimplifiedModel -> DAEIntegrationSetup -> Either String SimplifiedModel
fixHighers m p =
  let
    (_, cuBvar):_ = (unvariableInfo . variableInfo $ m) !! (daeBoundVariable p)
    varDeg = findMaximalDiffVariableDegreeWRT m (daeBoundVariable p)
    (_, allRates) = partition (\(v, d) -> d==0) $ M.toList varDeg
    (basicRates, higherRates) = partition (\(v, d) -> d==1) allRates
    origMaxVar = maximum (map (\(VariableID i) -> i) (M.keys (unvariableInfo . variableInfo $ m)))
    (higherMap, m'', _) =
      flip (flip foldl' (M.empty, m, origMaxVar)) higherRates $
      \(hMap, m'', vMax) (v, d) ->
        let (mp, cu):_ = (unvariableInfo $ variableInfo m'') !! v
        in
         flip (flip foldl' (M.insert (v, 0) v hMap, m'', vMax)) [1..(d - 1)] $ \(hMap, m'', vMax) newDeg ->
           let derivu = tryMakeUnitsForDeriv cu newDeg cuBvar
               vu = tryMakeUnitsForDeriv cu (newDeg - 1) cuBvar
               newv = VariableID (vMax + 1)
           in
            (M.insert (v, newDeg) newv hMap,
             m'' { variableInfo = VariableInfo $ M.insert newv [(mp, derivu)] (unvariableInfo $ variableInfo m''),
                   assertions = (Assertion
                                   (noSemCom $ Apply (noSemCom $ Csymbol (Just "relation1") "eq")
                                      [noSemCom $ ASTCi (Ci "higher"),
                                       noSemCom $ Apply (noSemCom $ Apply (noSemCom $ Csymbol (Just "calculus1") "diff")
                                                         [noSemCom $ Bind (noSemCom $ Csymbol (Just "fns1") "lambda")
                                                                          [noSemCom $ Ci "t"]
                                                                          (noSemCom $ ASTCi (Ci "lower"))])
                                       [noSemCom $ ASTCi (Ci "t")]],
                                    AssertionContext $ M.fromList [("higher", ("real", derivu, newv)),
                                                                   ("lower", ("real", derivu, hMap !! (v, newDeg - 1))),
                                                                   ("t", ("real", cuBvar, daeBoundVariable p))
                                                                  ])):
                                (map (changeDerivativesToVariable v newDeg newv derivu (newDeg == d-1)) (assertions m'')) },
             vMax + 1)
  in
   Right m''

changeDerivativesToVariable origVar matchDegree newVar newVarU oneHigherAsDeriv
                            (Assertion (WithMaybeSemantics s (WithCommon c ex), AssertionContext ctx)) =
  let
    -- Find all variable names that correspond to origVar...
    nameSet = S.fromList . map fst . filter (\(_, (_, _, var)) -> var == origVar) . M.toList $ ctx
    makeUnusedName i | M.member istr ctx = makeUnusedName (i + 1)
                     | otherwise = "var" ++ istr
      where istr = "var" ++ show i
    newName = makeUnusedName 0
    ctx' = M.insert newName ("real", newVarU, newVar) ctx
  in
   Assertion (WithMaybeSemantics s (WithCommon c
                               (changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv ex)), AssertionContext ctx')

changeDerivativesToVariableEx _ _ _ _ c@(Cn _) = c
changeDerivativesToVariableEx _ _ _ _ c@(ASTCi _) = c
changeDerivativesToVariableEx _ _ _ _ c@(Csymbol _ _) = c
changeDerivativesToVariableEx _ _ _ _ c@(Cs _) = c
changeDerivativesToVariableEx _ _ _ _ e@(Error _ _) = e
changeDerivativesToVariableEx _ _ _ _ cb@(CBytes _) = cb
changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv (Apply op [dege, expre])
  | opIs "calculus1" "nthdiff" op,
    Just deg <- tryGetConstant dege,
    degi <- round deg,
    degi == matchDegree || (oneHigherAsDeriv && (degi == matchDegree + 1)),
    isLambdaVariableNamedInSet nameSet (stripSemCom expre) =
      if degi == matchDegree then
        ASTCi (Ci newName)
      else
        Apply (noSemCom $ Csymbol (Just "calculus1") "diff") [noSemCom $ ASTCi (Ci newName)]

changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv (Apply op [expre])
  | opIs "calculus1" "diff" op,
    matchDegree == 1,
    isLambdaVariableNamedInSet nameSet (stripSemCom expre) = ASTCi (Ci newName)
changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv (Apply op operands) =
  Apply (applyIgnoringSemCom (changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv) op)
        (mapIgnoringSemCom (changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv) operands)
changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv (Bind op bvars expr) =
  let
    nameSet' = foldl' (\nameSet' (WithMaybeSemantics _ (WithCommon _ (Ci bvarname))) ->
                        S.delete bvarname nameSet') nameSet bvars
  in
   Bind (applyIgnoringSemCom (changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv) op)
        bvars 
        (applyIgnoringSemCom (changeDerivativesToVariableEx nameSet matchDegree newName oneHigherAsDeriv) expr)

opIs cd cn (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just cd1) cn1)))
  | cd == cd1 && cn == cn1 = True
opIs _ _ _ = False

isLambdaVariableNamedInSet nameSet (Bind op _ (WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci name))))) 
  | opIs "fns1" "lambda" op,
    S.member name nameSet = True
isLambdaVariableNamedInSet _ _ = False

tryGetLambdaVariable (Bind op _ (WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci name))))) 
  | opIs "fns1" "lambda" op = Just name
tryGetLambdaVariable _ = Nothing

tryGetVariableRef (ASTCi (Ci name)) = Just (name, 0, False)
tryGetVariableRef (Apply (WithMaybeSemantics _ (WithCommon _ (Apply op [expr]))) [bvex])
  | opIs "calculus1" "diff" op,
    ASTCi (Ci bvName) <- stripSemCom bvex, -- We should check bvName is our overall bvar
    Just varName <- tryGetLambdaVariable (stripSemCom expr) = Just (varName, 1, False)
tryGetVariableRef (Apply op [boundE, whenE, varE])
  | opIs "cellml1" "evaluatedAt" op,
    Just whenc <- tryGetConstant whenE,
    ASTCi (Ci varName) <- stripSemCom varE = Just (varName, 0, True)
tryGetVariableRef (Apply op [boundE, whenE, WithMaybeSemantics _ (WithCommon _ (Apply op2 [expr]))])
  | opIs "cellml1" "evaluatedAt" op,
    opIs "calculus1" "diff" op2,
    -- XXX we really should check all whenE values are consistent, and that
    -- boundE is the bound variable the problem is over.
    Just whenc <- tryGetConstant whenE,
    ASTCi (Ci varName) <- stripSemCom expr = Just (varName, 1, True)
tryGetVariableRef _ = Nothing

tryGetConstant (WithMaybeSemantics _ (WithCommon _ (Cn cp))) =
  Just (cpToDouble cp)
tryGetConstant _ = Nothing

stripSemCom (WithMaybeSemantics _ (WithCommon _ x)) = x
             
applyIgnoringSemCom :: (a -> b) -> WithMaybeSemantics (WithCommon a) -> WithMaybeSemantics (WithCommon b)
applyIgnoringSemCom f (WithMaybeSemantics s (WithCommon c a)) = WithMaybeSemantics s (WithCommon c (f a))
mapIgnoringSemCom f = map (applyIgnoringSemCom f)

-- | Zip the first list with items from one of the two following lists, depending on a predicate. True
-- | means to take the next value from the second list, while False means to take the next value from the
-- | third
zipSelect :: (a -> Bool) -> [a] -> [b] -> [b] -> [(a, b)]
zipSelect f (a:l1) l2 l3
  | fa, (b:l2') <- l2 = (a, b):(zipSelect f l1 l2' l3)
  | not fa, (b:l3') <- l3 = (a, b):(zipSelect f l1 l2 l3')
  where
    fa = f a
zipSelect _ _ _ _ = []

tryMakeUnitsForDeriv :: Maybe CanonicalUnits -> Int -> Maybe CanonicalUnits -> Maybe CanonicalUnits
tryMakeUnitsForDeriv (Just u) derivDeg (Just bvaru) = Just $
  CanonicalUnits {
     -- XXX spec should clarify this. Is the derivative of a variable in celsius in celsius per second, or in
     -- kelvin per second?
    cuOffset = if derivDeg == 0 then cuOffset u else 0,
    cuMultiplier = cuMultiplier u / (cuMultiplier bvaru ** (fromIntegral derivDeg)),
    cuBases = M.unionWith (+) (M.map (\v -> -v * (fromIntegral derivDeg)) (cuBases bvaru)) (cuBases u)
    }
tryMakeUnitsForDeriv _ _ _ = Nothing

findMaximalDiffVariableDegreeWRT :: SimplifiedModel -> VariableID -> M.Map VariableID Int
findMaximalDiffVariableDegreeWRT m t =
  let
    allVars = M.fromList [(v, 0) |
                          Assertion (ex, AssertionContext ctx) <-
                            assertions m,
                          vname <- S.toList $ globalVariableNames ex,
                          Just (_, _, v) <- [M.lookup vname ctx]
                          ]
    allDerivs = M.fromList [(v, 1) |
                            Assertion (ex, AssertionContext ctx) <- assertions m,
                            Apply (WithMaybeSemantics _
                                   (WithCommon _
                                    (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "calculus1") "diff")))
                                     [WithMaybeSemantics _
                                      (WithCommon _
                                       (
                                         Bind (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "fns1") "lambda")))
                                              _
                                              (WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci vname))))
                                       ))]
                                    ))) [WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci vnameBvar)))] <- universeBi ex,
                            Just (_, _, vbvar) <- [M.lookup vnameBvar ctx],
                            vbvar == t,
                            Just (_, _, v) <- [M.lookup vname ctx]
                           ]
    allNthDerivs = M.fromList [(v, round . cpToDouble $ x) |
                                Assertion (ex, AssertionContext ctx) <- assertions m,
                                Apply (WithMaybeSemantics _ (WithCommon _
                                                             (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "calculus1") "nthdiff")))
                                                              [
                                                                WithMaybeSemantics _ (WithCommon _ (Cn x)),
                                                                WithMaybeSemantics _
                                                                (WithCommon _
                                                                 (
                                                                   Bind (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "fns1") "lambda")))
                                                                        _
                                                                        (WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci vname))))
                                                                 ))]
                                                             ))) [WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci vnameBvar)))] <- universeBi ex,
                                Just (_, _, vbvar) <- [M.lookup vnameBvar ctx],
                                vbvar == t,
                                Just (_, _, v) <- [M.lookup vname ctx]]
  in
   M.unionsWith max [allVars, allDerivs, allNthDerivs]

globalVariableNames =
  -- Pick out all variable names in a mathematical expression, but exclude bound variable names.
  everythingWithContext S.empty S.union
    (((\binds -> (S.empty, binds)) `mkQ`
      (\(Ci name) binds -> (if (name `S.member` binds) then S.empty else S.singleton name, binds))) `extQ`
     bindCase)
  where
    bindCase (Bind _ l _) binds = 
      (S.empty, foldl' updateBinds binds l)
    bindCase _ binds = (S.empty, binds)
    updateBinds binds (WithMaybeSemantics _ (WithCommon _ (Ci name))) = S.insert name binds

assertionVariables :: Assertion -> [VariableID]
assertionVariables (Assertion (ex, AssertionContext ac)) = 
  [v | Ci vname <- universeBi ex, Just (_, _, v) <- [M.lookup vname ac]]

onlyData (NumericalData d) = Just d
onlyData _ = Nothing

dimensionallyCompatibleUnits :: CanonicalUnits -> CanonicalUnits -> Bool
dimensionallyCompatibleUnits (CanonicalUnits { cuBases = u1 }) (CanonicalUnits { cuBases = u2 })
  | length u1e /= length u2e = False
  | all (\((bu1, v1), (bu2, v2)) -> bu1 == bu2 && abs (v1-v2) <= 1E-5) (zip u1e u2e) = True
  | otherwise = False
  where
    u1e = M.toList u1
    u2e = M.toList u2

untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
untilM termcond ex v0 = do
  term <- termcond v0
  if term
    then return v0
    else ex v0 >>= untilM termcond ex

simplifyModel (sm@SimplifiedModel { assertions = a }) =
  (\x -> sm { assertions = fst $ x }) <$>
    (untilM (return . not . snd) tryFurtherSimplifications (a, True))

tryFurtherSimplifications (assertions, _) = do
  assertions' <- mapM (\(Assertion (ex, ac@(AssertionContext m))) -> (\x -> Assertion (x, ac)) <$> simplifyMaths m ex) assertions
  let substitutions = M.fromList $ mapMaybe toMaybeSubst assertions'
  return $ foldl' (\(l, anyChange) (Assertion (ex, ac@(AssertionContext m))) ->
                    let ex' = transformBi (simplificationRules m) (substituteInMaths m substitutions ex)
                    in
                     ((Assertion (ex', ac)):l, anyChange || (ex /= ex')))
                  ([], False) assertions'

simplificationRules :: M.Map String (TypeName, Maybe CanonicalUnits, VariableID) -> AST -> AST
simplificationRules m (Apply op args)
  | opIs "arith1" "times" op, [arg] <- args = stripSemCom arg
  | opIs "arith1" "times" op, [] <- args = Cn (CnInteger 1)
  | opIs "arith1" "plus" op, [arg] <- args = stripSemCom arg
  | opIs "arith1" "plus" op, [] <- args = Cn (CnInteger 0)
  | opIs "arith1" "minus" op, [arg1, WithMaybeSemantics _ (WithCommon _ (Cn v))] <-
      args, cp <- cpToDouble v, cp == 0 = stripSemCom arg1
  | opIs "arith1" "minus" op, [arg1, arg2] <-
      args, arg1 == arg2 = Cn (CnInteger 0)
  | opIs "arith1" "times" op, any (isParticularConstant 0 . stripSemCom) args = Cn (CnInteger 0)
  | opIs "arith1" "divide" op, [arg1, arg2] <- args, isParticularConstant 0 (stripSemCom arg1) = Cn (CnInteger 0)
  | opIs "arith1" "divide" op, [arg1, arg2] <- args, isParticularConstant 1 (stripSemCom arg2) = stripSemCom arg1
  | opIs "arith1" "power" op, [arg1, arg2] <- args, isParticularConstant 1 (stripSemCom arg2) = stripSemCom arg1
  | opIs "arith1" "power" op, [arg1, arg2] <- args, isParticularConstant 0 (stripSemCom arg1) = Cn (CnInteger 0)
  | opIs "arith1" "power" op, [arg1, arg2] <- args, isParticularConstant 0 (stripSemCom arg2) = Cn (CnInteger 1)
  | opIs "arith1" "power" op, [arg1, arg2] <- args, isParticularConstant 1 (stripSemCom arg1) = Cn (CnInteger 1)
  | opIs "arith1" "plus" op = Apply op (filter (not . isParticularConstant 0 . stripSemCom) args)
  | opIs "arith1" "times" op = Apply op (filter (not . isParticularConstant 1 . stripSemCom) args)
simplificationRules m ex = ex

isParticularConstant :: Double -> AST -> Bool
isParticularConstant n (Cn p) | cpToDouble p == n = True
isParticularConstant _ _ = False

toMaybeSubst (Assertion
              ((WithMaybeSemantics _ (WithCommon _ (Apply eq [arg1, arg2]))),
               AssertionContext m))
  | opIs "relation1" "eq" eq = 
    case (extractCI (stripSemCom arg1), extractConstant (stripSemCom arg2),
          extractCI (stripSemCom arg2), extractConstant (stripSemCom arg1)) of
      (Just n, Just c, _, _) |
        Just (_, Just u, v) <- M.lookup n m -> Just (v, (c, u))
      (_, _, Just n, Just c) |
        Just (_, Just u, v) <- M.lookup n m -> Just (v, (c, u))
      _ -> Nothing
toMaybeSubst _ = Nothing

extractCI (ASTCi (Ci n)) = Just n
extractCI _ = Nothing
extractConstant (Cn cp) = Just (cpToDouble cp)
extractConstant _ = Nothing

substituteInMaths m sm ex = transformBi (substituteVariable m sm) ex
substituteVariable m sm ex@(ASTCi (Ci n))
  | Just (_, Just ucontext, v)  <- M.lookup n m,
    Just (c, uvar) <- M.lookup v sm =
      Cn (CnReal (convertReal uvar ucontext c))
substituteVariable _ _ ex = ex

simplifyMaths :: Monad m => M.Map String (TypeName, Maybe CanonicalUnits, VariableID) -> ASTC -> ErrorT String m ASTC
simplifyMaths m (WithMaybeSemantics _ (WithCommon _ x)) =
  liftM noSemCom $ (simplifyMathAST m x)
  
isConstantReal (WithMaybeSemantics _ (WithCommon _ (Cn _))) = True
isConstantReal _ = False
isConstantBool (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "logic1") "true"))) = True
isConstantBool (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "logic1") "false"))) = True
isConstantBool _ = False

toConstantBool (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "logic1") "true"))) = True
toConstantBool (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "logic1") "false"))) = False
toConstantBool _ = error "toConstantBool given an expression which isn't MathML for a constant boolean"

constantBoolAsMathML True = Csymbol (Just "logic1") "true"
constantBoolAsMathML False = Csymbol (Just "logic1") "false"

simplifyMathAST :: Monad m => M.Map String (TypeName, Maybe CanonicalUnits, VariableID) -> AST -> ErrorT String m AST
simplifyMathAST m (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "nums1") "based_integer")))
                         [ex, WithMaybeSemantics _ (WithCommon _ (Cs bs))]) = do
  ex' <- simplifyMaths m ex
  case ex' of
    WithMaybeSemantics _ (WithCommon _ (Cn cp)) -> basedIntegerStringToAST (round $ cpToDouble cp) bs
    _ -> fail "based_integer with non-constant base not supported"
  
simplifyMathAST m (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "nums1") "based_float")))
                         [ex, WithMaybeSemantics _ (WithCommon _ (Cs bs))]) = do
  ex' <- simplifyMaths m ex
  case ex' of
    WithMaybeSemantics _ (WithCommon _ (Cn cp)) -> basedFloatStringToAST (cpToDouble cp) bs
    _ -> fail "based_float with non-constant base not supported"

simplifyMathAST m (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "fns2") "predicate_on_list")))
                   [op,
                    WithMaybeSemantics _ (WithCommon _ (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "list1") "list")))
                                                              l))]) = do
  l' <- mapM (simplifyMaths m) l
  if null l' then return $ constantBoolAsMathML True
    else
    return $ Apply (noSemCom $ Csymbol (Just "logic1") "and")
      (map (\(v1, v2) -> noSemCom $ Apply op [v1, v2]) $ zip l' (tail l'))

simplifyMathAST m (Apply op@(WithMaybeSemantics _ (WithCommon _ (Csymbol (Just cd) expr))) ops)
  | cd == "piece1"  && expr == "piecewise" = do
    ops' <- mapM (simplifyMaths m) ops
    let pieces = mapMaybe (extractPiecewisePiece . stripSemCom) ops'
    let otherwise = listToMaybe . mapMaybe (extractPiecewiseOtherwise . stripSemCom) $ ops'
    let pieces' = filter (\(_, x) -> not (isConstantBool (noSemCom x)) || toConstantBool (noSemCom x)) pieces
    let defpieces = filter (isConstantBool . noSemCom . snd) pieces'
    return $ case (defpieces, pieces', otherwise) of
      -- If there is a piece that is always true, use it.
      ((ex, _):_, _, _) -> ex
      -- If all pieces are false, and there is an otherwise, use it.
      (_, [], Just ow) -> stripSemCom ow
      -- If only one piece isn't provably false, and there's no otherwise,
      -- use it always...
      (_, (ex, _):[], Nothing) -> ex
      -- Otherwise re-assemble the piecewise from the parts...
      (_, l, mo) -> Apply op (map (noSemCom . Apply (noSemCom $ Csymbol (Just "piece1") "piece") . (\(a,b) -> [noSemCom a, noSemCom b])) l ++ map (noSemCom . Apply (noSemCom $ Csymbol (Just "piece1") "otherwise") . (:[])) (maybeToList mo))

  | otherwise = do
    ops' <- mapM (simplifyMaths m) ops
    if cd == "piece1" then
      tryPartialApplyCombine cd expr ops'
      else 
      if all isConstantReal ops'
        then tryConstantApply cd expr (map (\(WithMaybeSemantics _ (WithCommon _ (Cn p))) -> cpToDouble p) ops')
        else
          if all isConstantBool ops'
            then tryConstantApplyBool cd expr (map toConstantBool ops')
            else tryPartialApplyCombine cd expr ops'

simplifyMathAST m cs@(Csymbol (Just "nums1") s)
  | Just v <- tryNums1Constant s = return v
  | otherwise = return cs

simplifyMathAST m (Apply op ops) =
  Apply `liftM` (simplifyMaths m op) `ap` (mapM (simplifyMaths m) ops)
  
simplifyMathAST m (Bind op bvars operand) =
  Bind `liftM` (simplifyMaths m op) `ap` (return bvars) `ap` (simplifyMaths m operand)

simplifyMathAST _ v = return v

tryPartialApplyCombine cd sym ops
  | (cd == "relation1" && (sym == "and" || sym == "or")) = do
    let (sconst, svar) = partition isConstantBool $ recursivelyFlattenApply cd sym ops
    cconst <- tryConstantApplyBool cd sym (map toConstantBool sconst)
    if ((cconst==constantBoolAsMathML True) /= (sym == "and"))
       -- anything and false, or anything or true is equal to false or true respectively
      then return cconst
      else -- otherwise keep the non-constant part.
        if null (tail svar) then return ((\(WithMaybeSemantics _ (WithCommon _ v)) -> v) $ head svar)
          else return $ Apply (noSemCom $ Csymbol (Just cd) sym) svar
  | (cd == "arith1" && (sym == "plus" || sym == "times" || sym == "gcd" || sym == "lcm")) = do
    let (sconst, svar) = partition isConstantReal $ recursivelyFlattenApply cd sym ops
    cconst <- tryConstantApply cd sym (map (\(WithMaybeSemantics _ (WithCommon _ (Cn p))) -> cpToDouble p) sconst)
    case () of
      () | cconst == Cn (CnReal 0) && sym == "times" -> return (Cn . CnReal $ 0)
         | null svar -> return cconst
         | null (tail svar) &&
           ((cconst == Cn (CnReal 1) && sym == "times") || 
            (cconst == Cn (CnReal 0) && sym == "plus")) ->
             return ((\(WithMaybeSemantics _ (WithCommon _ v)) -> v) $ head svar)
         | ((cconst == Cn (CnReal 1) && sym == "times") || 
            (cconst == Cn (CnReal 0) && sym == "plus")) ->
               return $ Apply (noSemCom $ Csymbol (Just cd) sym) svar
         | otherwise ->
           return $ Apply (noSemCom $ Csymbol (Just cd) sym) ((noSemCom cconst):svar)

tryPartialApplyCombine cd sym ops = return $ Apply (noSemCom $ Csymbol (Just cd) sym) ops

recursivelyFlattenApply cd sym ops =
  concatMap (tryApplySymbol cd sym) ops

tryApplySymbol cd1 sym1 (WithMaybeSemantics _ (WithCommon _ (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just cd2) sym2))) args)))
  | cd1 == cd2 && sym2 == sym1 =
    recursivelyFlattenApply cd1 sym1 args
tryApplySymbol _ _ s = [s]

cpToDouble :: ConstantPart -> Double
cpToDouble (CnInteger i) = fromIntegral i
cpToDouble (CnReal v) = v
cpToDouble (CnDouble v) = v
cpToDouble (CnHexDouble v) = v

tryConstantApply :: Monad m => String -> String -> [Double] -> ErrorT String m AST
tryConstantApply "arith1" o l = tryArith1ConstantApply o l
tryConstantApply "transc1" o l = tryTransc1ConstantApply o l
tryConstantApply "relation1" o l = tryRelation1ConstantApply o l
tryConstantApply "rounding1" o l = tryRounding1ConstantApply o l
tryConstantApply "integer1" o l = tryInteger1ConstantApply o l
tryConstantApply "nums1" "rational" [v1, v2] = return . Cn . CnReal $ v1 / v2
tryConstantApply cd o _ = fail ("Unrecognised content dictionary: " ++ cd)

tryConstantApplyBool :: Monad m => String -> String -> [Bool] -> ErrorT String m AST
tryConstantApplyBool "logic1" o l = tryLogic1ConstantApply o l
tryConstantApplyBool "relation1" o l = tryRelation1ConstantApplyBool o l
tryConstantApplyBool cd o _ = fail ("Unrecognised content dictionary for boolean expression: " ++ cd)

tryArith1ConstantApply "plus" ops = return . Cn . CnReal . sum $ ops
tryArith1ConstantApply "times" ops = return . Cn . CnReal . product $ ops
tryArith1ConstantApply "minus" [v1,v2] = return . Cn . CnReal $ v1-v2
tryArith1ConstantApply "unary_minus" [v] = return . Cn . CnReal $ -v
tryArith1ConstantApply "divide" [v1,v2] = return . Cn . CnReal $ v1/v2
tryArith1ConstantApply "power" [v1,v2] = return . Cn . CnReal $ v1**v2
tryArith1ConstantApply "abs" [v] = return . Cn . CnReal $ abs v
tryArith1ConstantApply "root" [v1, v2] = return . Cn . CnReal $ v1**(1/v2)
tryArith1ConstantApply "lcm" ops@(_:_) = return . Cn . CnReal $ foldl' (\a b -> fromIntegral (lcm (round a) (round b))) (head ops) (tail ops)
tryArith1ConstantApply "gcd" ops@(_:_) = return . Cn . CnReal $ foldl' (\a b -> fromIntegral (gcd (round a) (round b))) (head ops) (tail ops)
tryArith1ConstantApply "sum" _ = fail "sum operator in dictionary arith1 is not supported"
tryArith1ConstantApply "product" _ = fail "product operator in dictionary arith1 is not supported"
tryArith1ConstantApply "minus" _ = fail "Minus operator must have two arguments"
tryArith1ConstantApply "unary_minus" _ = fail "Unary minus operator must have one argument"
tryArith1ConstantApply "divide" _ = fail "Unary minus operator must have one argument"
tryArith1ConstantApply "power" _ = fail "Power operator must have two arguments"
tryArith1ConstantApply "abs" _ = fail "Abs operator must have one argument"
tryArith1ConstantApply "root" _ = fail "Root operator must have two arguments"
tryArith1ConstantApply "lcm" _ = fail "Lcm operator must have at least one argument"
tryArith1ConstantApply "gcd" _ = fail "Gcd operator must have at least one argument"
tryArith1ConstantApply n _ = fail ("Content Dictionary arith1 has no operator " ++ n)

tryLogic1ConstantApply "true" _ = return . constantBoolAsMathML $ True
tryLogic1ConstantApply "false" _ = return . constantBoolAsMathML $ False
tryLogic1ConstantApply "equivalent" [v1, v2] = return . constantBoolAsMathML $ v1 == v2
tryLogic1ConstantApply "not" [v1] = return . constantBoolAsMathML $ not v1
tryLogic1ConstantApply "and" l = return . constantBoolAsMathML $ foldl' (&&) True l
tryLogic1ConstantApply "or" l = return . constantBoolAsMathML $ foldl' (||) True l
tryLogic1ConstantApply "xor" l = return . constantBoolAsMathML $ foldl' (/=) True l
tryLogic1ConstantApply "equivalent" _ = fail "Equivalent operator must have two arguments"
tryLogic1ConstantApply "not" _ = fail "Not operator must have one argument"
tryLogic1ConstantApply n _ = fail ("Content Dictionary logic1 has no operator " ++ n)

tryTransc1ConstantApply "log" [v1, v2] = return . Cn . CnReal $ logBase v1 v2
tryTransc1ConstantApply "ln" [v] = return . Cn . CnReal $ log v
tryTransc1ConstantApply "exp" [v] = return . Cn . CnReal $ exp v
tryTransc1ConstantApply "sin" [v] = return . Cn . CnReal $ sin v
tryTransc1ConstantApply "cos" [v] = return . Cn . CnReal $ cos v
tryTransc1ConstantApply "tan" [v] = return . Cn . CnReal $ tan v
tryTransc1ConstantApply "sec" [v] = return . Cn . CnReal $ 1 / cos v
tryTransc1ConstantApply "csc" [v] = return . Cn . CnReal $ 1 / sin v
tryTransc1ConstantApply "cot" [v] = return . Cn . CnReal $ 1 / tan v
tryTransc1ConstantApply "sinh" [v] = return . Cn . CnReal $ sinh v
tryTransc1ConstantApply "cosh" [v] = return . Cn . CnReal $ cosh v
tryTransc1ConstantApply "tanh" [v] = return . Cn . CnReal $ tanh v
tryTransc1ConstantApply "sech" [v] = return . Cn . CnReal $ 1 / cosh v
tryTransc1ConstantApply "csch" [v] = return . Cn . CnReal $ 1 / sinh v
tryTransc1ConstantApply "coth" [v] = return . Cn . CnReal $ 1 / tanh v
tryTransc1ConstantApply "arcsin" [v] = return . Cn . CnReal $ asin v
tryTransc1ConstantApply "arccos" [v] = return . Cn . CnReal $ acos v
tryTransc1ConstantApply "arctan" [v] = return . Cn . CnReal $ atan v
tryTransc1ConstantApply "arcsec" [v] = return . Cn . CnReal $ acos (1 / v)
tryTransc1ConstantApply "arccsc" [v] = return . Cn . CnReal $ asin (1 / v)
tryTransc1ConstantApply "arccot" [v] = return . Cn . CnReal $ atan (1 / v)
tryTransc1ConstantApply "arcsinh" [v] = return . Cn . CnReal $ asinh v
tryTransc1ConstantApply "arccosh" [v] = return . Cn . CnReal $ acosh v
tryTransc1ConstantApply "arctanh" [v] = return . Cn . CnReal $ atanh v
tryTransc1ConstantApply "arcsech" [v] = return . Cn . CnReal $ acosh (1 / v)
tryTransc1ConstantApply "arccsch" [v] = return . Cn . CnReal $ asinh (1 / v)
tryTransc1ConstantApply "arccoth" [v] = return . Cn . CnReal $ atanh (1 / v)
tryTransc1ConstantApply "sin" _ = fail $ "sin operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "cos" _ = fail $ "cos operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "tan" _ = fail $ "tan operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "sec" _ = fail $ "sec operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "csc" _ = fail $ "csc operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "cot" _ = fail $ "cot operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "sinh" _ = fail $ "sinh operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "cosh" _ = fail $ "cosh operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "tanh" _ = fail $ "tanh operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "sech" _ = fail $ "sech operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "csch" _ = fail $ "csch operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "coth" _ = fail $ "coth operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arcsin" _ = fail $ "arcsin operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arccos" _ = fail $ "arccos operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arctan" _ = fail $ "arctan operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arcsec" _ = fail $ "arcsec operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arccsc" _ = fail $ "arccsc operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arccot" _ = fail $ "arccot operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arcsinh" _ = fail $ "arcsinh operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arccosh" _ = fail $ "arccosh operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arctanh" _ = fail $ "arctanh operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arcsech" _ = fail $ "arcsech operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arccsch" _ = fail $ "arccsch operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "arccoth" _ = fail $ "arccoth operator in cd transc1 requires exactly one argument"
tryTransc1ConstantApply "log" _ = fail "Log operator must have two arguments"
tryTransc1ConstantApply "ln" _ = fail "Ln operator must have one argument"
tryTransc1ConstantApply "exp" _ = fail "Exp operator must have one argument"
tryTransc1ConstantApply n _ = fail ("Content dictionary transc1 has no operator " ++ n)

tryRelation1ConstantApply "eq" [v1, v2]  = return . constantBoolAsMathML $ v1 == v2
tryRelation1ConstantApply "lt" [v1, v2]  = return . constantBoolAsMathML $ v1 < v2
tryRelation1ConstantApply "gt" [v1, v2]  = return . constantBoolAsMathML $ v1 > v2
tryRelation1ConstantApply "neq" [v1, v2]  = return . constantBoolAsMathML $ v1 /= v2
tryRelation1ConstantApply "leq" [v1, v2]  = return . constantBoolAsMathML $ v1 <= v2
tryRelation1ConstantApply "geq" [v1, v2]  = return . constantBoolAsMathML $ v1 >= v2
tryRelation1ConstantApply "eq" _  = fail "eq operator in cd relation1 requires exactly two arguments"
tryRelation1ConstantApply "lt" _  = fail "lt operator in cd relation1 requires exactly two arguments"
tryRelation1ConstantApply "gt" _  = fail "gt operator in cd relation1 requires exactly two arguments"
tryRelation1ConstantApply "neq" _  = fail "neq operator in cd relation1 requires exactly two arguments"
tryRelation1ConstantApply "leq" _  = fail "leq operator in cd relation1 requires exactly two arguments"
tryRelation1ConstantApply "geq" _  = fail "geq operator in cd relation1 requires exactly two arguments"
tryRelation1ConstantApply n _  = fail ("Content dictionary relation1 has no operator " ++ n)

tryRelation1ConstantApplyBool "eq" [v1, v2] = return . constantBoolAsMathML $ v1 == v2
tryRelation1ConstantApplyBool "eq" _ = fail ("Operator eq in content dictionary relation1 requires exactly two arguments")
tryRelation1ConstantApplyBool n _ = fail ("Operator " ++ n ++ " in content dictionary relation1 is not valid on boolean arguments")

tryRounding1ConstantApply "ceiling" [v] = return . Cn . CnReal . fromIntegral . ceiling $ v
tryRounding1ConstantApply "floor" [v] = return . Cn . CnReal . fromIntegral . floor $ v
tryRounding1ConstantApply "trunc" [v] = return . Cn . CnReal . fromIntegral . truncate $ v
tryRounding1ConstantApply "round" [v] = return . Cn . CnReal . fromIntegral . round $ v
tryRounding1ConstantApply "ceiling" _ = fail ("ceiling operator in the content dictionary rounding1 requires exactly one argument")
tryRounding1ConstantApply "floor" _ = fail ("floor operator in the content dictionary rounding1 requires exactly one argument")
tryRounding1ConstantApply "trunc" _ = fail ("trunc operator in the content dictionary rounding1 requires exactly one argument")
tryRounding1ConstantApply "round" _ = fail ("round operator in the content dictionary rounding1 requires exactly one argument")
tryRounding1ConstantApply n _ = fail ("Content dictionary rounding1 has no operator " ++ n)

tryInteger1ConstantApply "factorof" [v1, v2] = return . constantBoolAsMathML $ (round v2) `mod` (round v1) == 0
tryInteger1ConstantApply "factorial" [v] = return . Cn . CnReal . fromIntegral . fact . round $ v
tryInteger1ConstantApply "quotient" [v1, v2] = return . Cn . CnReal . fromIntegral $ (round v1) `div` (round v2)
tryInteger1ConstantApply "remainder" [v1, v2] = return . Cn . CnReal . fromIntegral $ (round v1) `rem` (round v2)
tryInteger1ConstantApply "factorof" _ = fail "factorof requires exactly two arguments"
tryInteger1ConstantApply "factorial" _ = fail "factorial requires exactly one argument"
tryInteger1ConstantApply "quotient" _ = fail "quotient requires exactly two arguments"
tryInteger1ConstantApply "remainder" _ = fail "remainder requires exactly two arguments"
tryInteger1ConstantApply n _ = fail $ "Content dictionary integer1 has no operator " ++ n

tryNums1Constant "infinity" = Just . Cn . CnReal $ 1.0/0.0
tryNums1Constant "e" = Just . Cn . CnReal . exp $ 1
tryNums1Constant "gamma" = Just . Cn . CnReal $ 0.577215664
tryNums1Constant "pi" = Just . Cn . CnReal $ pi
tryNums1Constant "NaN" = Just . Cn . CnReal $ 0.0/0.0
tryNums1Constant _ = Nothing

fact n | n <= 1 = 1
       | otherwise = n * (fact (n-1))

basedIntegerStringToAST b s = liftM (Cn . CnDouble . fromIntegral) $ parseBased b 0 1 (reverse s)
basedFloatStringToAST b s =
  liftM (Cn . CnDouble) $
    let
      (dec, fl') = break (=='.') s
      fl = if null fl' then "" else tail fl'
    in
     liftM2 (+) (parseBased b 0.0 1.0 (reverse dec)) (parseBased (1/b) 0 0.1 fl)
parseBased _ c _ [] = return c
parseBased b c m s@(s1:s') = do
  d <- liftM fromIntegral (basedDigit s1)
  when (d >= b) . fail $ "Based number in base " ++ (show d) ++ " has invalid digit: " ++ s
  parseBased b (c + m * d) (m * b) s'

basedDigit c
  | c >= '0' && c <= '9' = return (digitToInt c)
  | c >= 'a' && c <= 'z' = return (ord c - ord 'a' + 10)
  | c >= 'A' && c <= 'Z' = return (ord c - ord 'A' + 10)
  | otherwise = fail $ "Invalid 'digit' " ++ c:(" in based digit string")

takePartialDerivative (Assertion (ex, AssertionContext m)) wrt = repeatedlySimplify m (stripSemCom ex) >>= \ex' -> takePartialDerivativeEx m wrt ex'

sTakePartialDerivativeEx :: (Functor m, Monad m) => M.Map String (TypeName, Maybe CanonicalUnits, VariableID) -> (VariableID, Int) -> AST -> ErrorT String m AST
sTakePartialDerivativeEx m wrt a = repeatedlySimplify m a >>= \a' -> takePartialDerivativeEx m wrt a'

takePartialDerivativeEx :: (Functor m, Monad m) => M.Map String (TypeName, Maybe CanonicalUnits, VariableID) -> (VariableID, Int) -> AST -> ErrorT String m AST
takePartialDerivativeEx m _ (Cn _) = return $ Cn (CnInteger 0)
takePartialDerivativeEx m (wrtv, wrtn) (ASTCi (Ci n))
  | Just (_, _, v) <- M.lookup n m, v == wrtv, wrtn == 0 = return $ Cn (CnInteger 1)
  | otherwise = return $ Cn (CnInteger 0)

takePartialDerivativeEx m wrt (Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just cd) name))) args)
  | cd == "piece1" = takePartialDerivativePiece
  | cd == "arith1" = takePartialDerivativeArith
  | cd == "transc1" = takePartialDerivativeTransc
  | cd == "rounding1" = takePartialDerivativeRounding
  | otherwise = fail $ "Don't know how to differentiate symbols from content dictionary " ++ cd
  where
    takePartialDerivativePiece
      | name == "piecewise" =
        (Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "piece1") "piecewise")))) `liftM`
          (mapM (\x -> noSemCom `liftM` (takePartialDerivativePiecewisePart . stripSemCom $ x)) args)
      | otherwise = fail $ "Can't differentiate piece1 symbol " ++ cd
    takePartialDerivativePiecewisePart :: (Functor m, Monad m) => AST -> ErrorT String m AST
    takePartialDerivativePiecewisePart (Apply op [arg1, arg2])
      | opIs "piece1" "piece" op = sTakePartialDerivativeEx m wrt (stripSemCom arg1) >>= \x -> return (Apply op [noSemCom x, arg2])
    takePartialDerivativePiecewisePart (Apply op [arg1])
      | opIs "piece1" "otherwise" op = (Apply op . (:[]) . noSemCom) `liftM` sTakePartialDerivativeEx m wrt (stripSemCom arg1)
    takePartialDerivativePiecewisePart _ = fail "Tried to take derivative of invalid piecewise part"
    takePartialDerivativeArith :: (Functor m, Monad m) => ErrorT String m AST
    takePartialDerivativeArith
      | name == "lcm" = return $ Cn (CnInteger 0) -- It is not differentiable over R, but is 0 at all R \ N
      | name == "gcd" = return $ Cn (CnInteger 0)
      | name == "plus" = (Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "plus")))) `liftM`
                           (mapM (\x -> liftM noSemCom (sTakePartialDerivativeEx m wrt . stripSemCom $ x)) args)
      | name == "unary_minus" =
        Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "unary_minus"))) `liftM`
          mapM (\x -> liftM noSemCom (sTakePartialDerivativeEx m wrt (stripSemCom x))) args
      | name == "minus" =
        Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "minus"))) `liftM`
          mapM (\x -> liftM noSemCom (sTakePartialDerivativeEx m wrt (stripSemCom x))) args
      | name == "times", f:[] <- args = do
        sTakePartialDerivativeEx m wrt (stripSemCom f)
      | name == "times", g:h:[] <- args = do
        g' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom g)
        h' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom h)
        return $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "plus"))) [
          noSemCom $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "times"))) [g, g'],
          noSemCom $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "times"))) [h, h']
          ]
      | name == "times", l <- args = do
        let ll = length l
            ls = ll `div` 2
            l1 = take ls l
            l2 = drop ls l
        sTakePartialDerivativeEx m wrt (Apply (noSemCom $ Csymbol (Just "arith1") "times") [
                                          noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "times") l1,
                                          noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "times") l2])
      | name == "divide", [g, h] <- args = do
        g' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom g)
        h' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom h)
        return $
          Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "divide")))
            [noSemCom $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "minus")))
             [noSemCom $ Apply (noSemCom (Csymbol (Just "arith1") "times")) [g', h],
              noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [g, h']],
             noSemCom $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "power")))
                          [h,
                           noSemCom $ Cn (CnInteger 2)]]
      | name == "power", [arg1, WithMaybeSemantics _ (WithCommon _ (Cn arg2))] <- args = do
        -- This f(x)^n rule is only needed because our simplifier is not good enough to recover
        -- from the generality of the f(x)^g(x) rule yet.
        -- cpToDouble arg2
        arg1' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom arg1)
        return $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "times"))) [
          noSemCom $ Cn (CnDouble (cpToDouble arg2 - 1)),
            noSemCom $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "power"))) [
              arg1,
              noSemCom $ Cn (CnDouble (cpToDouble arg2 - 1))
                                                                                                     ],
            arg1'
          ]
      | name == "power", [f, g] <- args = do
        f' <- noSemCom `liftM` sTakePartialDerivativeEx m wrt (stripSemCom f)
        g' <- noSemCom `liftM` sTakePartialDerivativeEx m wrt (stripSemCom g)
        return $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "times"))) [noSemCom $
          Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "plus"))) [noSemCom $
             Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "times"))) [noSemCom $
                Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "transc1") "ln"))) [f], g'
                ],
             noSemCom $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "divide"))) [noSemCom $
               Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "times"))) [g, f'],
               f
                                                                                            ]
                                                                                       ],
          noSemCom $ Apply (WithMaybeSemantics s (WithCommon c (Csymbol (Just "arith1") "power"))) [f, g]
                                                                                               ]
      | name == "abs", [arg] <- args =
        sTakePartialDerivativeEx m wrt (Apply (noSemCom $ Csymbol (Just "piece1") "piecewise") [
          noSemCom $ Apply (noSemCom $ Csymbol (Just "piece1") "piece") [
             arg,
             noSemCom $ Apply (noSemCom $ Csymbol (Just "relation1") "geq") [arg, noSemCom $ Cn (CnInteger 0)]
             ],
          noSemCom $ Apply (noSemCom $ Csymbol (Just "piece1") "otherwise") [
             noSemCom $ Apply (noSemCom $ Csymbol (Just "piece1") "unary_minus") [arg]
             ]
          ])
      | name == "root", [WithMaybeSemantics _ (WithCommon _ arg1), WithMaybeSemantics _ (WithCommon _ arg2)] <- args = do
          -- Doing this simplify first works around limitations of the simplifier that hit later.
          rootPow <- {- repeatedlySimplify m $ -} return $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
            noSemCom $ Cn (CnInteger 1), noSemCom arg2]
          sTakePartialDerivativeEx m wrt
            (Apply (noSemCom $ Csymbol (Just "arith1") "power") [noSemCom arg1, noSemCom $ rootPow])
      | otherwise = fail $ "Could not differentiate arith1 operator with name " ++ name
    takePartialDerivativeTransc
      | name == "log", [g, h] <- args =
        sTakePartialDerivativeEx m wrt (Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
          noSemCom (Apply (noSemCom $ Csymbol (Just "transc1") "ln") [g]),
          noSemCom (Apply (noSemCom $ Csymbol (Just "transc1") "ln") [h])
          ])
      | name == "ln", [f] <- args = do
          f' <- sTakePartialDerivativeEx m wrt (stripSemCom f)
          return (
            Apply (noSemCom $ Csymbol (Just "arith1") "times") [noSemCom f', noSemCom $
              Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
                noSemCom $ Cn (CnInteger 1), f]
                                                               ]
            )
      | name == "exp", [f] <- args = do
          f' <- sTakePartialDerivativeEx m wrt (stripSemCom f)
          return (
            Apply (noSemCom $ Csymbol (Just "arith1") "times") [
               noSemCom f',
               noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") "exp") [f]
               ]
            )
      | name == "csc" = inverseTrigDeriv "sin" args
      | name == "sec" = inverseTrigDeriv "cos" args
      | name == "cot" = inverseTrigDeriv "tan" args
      | name == "csch" = inverseTrigDeriv "sinh" args
      | name == "sech" = inverseTrigDeriv "cosh" args
      | name == "coth" = inverseTrigDeriv "tanh" args
      | name == "arccsc" = inverseArcTrigDeriv "arcsin" args
      | name == "arcsec" = inverseArcTrigDeriv "arccos" args
      | name == "arccot" = inverseArcTrigDeriv "arctan" args
      | name == "arccsch" = inverseArcTrigDeriv "arcsinh" args
      | name == "arcsech" = inverseArcTrigDeriv "arccosh" args
      | name == "arccoth" = inverseArcTrigDeriv "arctanh" args
      | name == "sin", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") "cos") [f]
          ]
      | name == "cos", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Cn (CnInteger (-1)),
            noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") "sin") [f]
          ]
      | name == "tan", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger 1),
              noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") "sec") [f],
                noSemCom $ Cn (CnInteger 2)
                ]]
          ]
      | name == "sinh", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") "cosh") [f]
          ]
      | name == "cosh", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") "sinh") [f]
          ]
      | name == "tanh", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger 1),
              noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") "sech") [f],
                noSemCom $ Cn (CnInteger 2)
                ]]
          ]
      | name == "arcsin", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger 1),
              noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "root") [
                noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "minus") [
                   noSemCom $ Cn (CnInteger 1),
                   noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                     f, noSemCom $ Cn (CnInteger 2)
                                                                                 ]
                                                                              ],
                noSemCom $ Cn (CnInteger 2)
                                                                             ]
          ]]
      | name == "arccos", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger (-1)),
              noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "root") [
                noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "minus") [
                   noSemCom $ Cn (CnInteger 1),
                   noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                     f, noSemCom $ Cn (CnInteger 2)
                                                                                 ]
                                                                              ],
                noSemCom $ Cn (CnInteger 2)
                                                                             ]
          ]]
      | name == "arctan", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger 1),
                noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "plus") [
                   noSemCom $ Cn (CnInteger 1),
                   noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                     f, noSemCom $ Cn (CnInteger 2)
                                                                                 ]
                                                                              ]
          ]]


      | name == "arcsinh", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger 1),
              noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "root") [
                noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "plus") [
                   noSemCom $ Cn (CnInteger 1),
                   noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                     f, noSemCom $ Cn (CnInteger 2)
                                                                                 ]
                                                                              ],
                noSemCom $ Cn (CnInteger 2)
                                                                             ]
          ]]
      | name == "arccosh", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger (-1)),
              noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "root") [
                noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "minus") [
                   noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                     f, noSemCom $ Cn (CnInteger 2)
                                                                                 ],
                   noSemCom $ Cn (CnInteger 1)                   
                                                                              ],
                noSemCom $ Cn (CnInteger 2)
                                                                             ]
          ]]
      | name == "arctanh", [f] <- args = do
        f' <- liftM noSemCom $ sTakePartialDerivativeEx m wrt (stripSemCom f)
        return $ Apply (noSemCom $ Csymbol (Just "arith1") "times") [
            f',
            noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
              noSemCom $ Cn (CnInteger 1),
                noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "minus") [
                   noSemCom $ Cn (CnInteger 1),
                   noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "power") [
                     f, noSemCom $ Cn (CnInteger 2)
                                                                                 ]
                                                                              ]
          ]]
    inverseTrigDeriv n l =
      sTakePartialDerivativeEx m wrt (Apply (noSemCom $ Csymbol (Just "arith1") "divide") [
                                        noSemCom $ Cn (CnInteger 1),
                                        noSemCom $ Apply (noSemCom $ Csymbol (Just "transc1") n) l])
    inverseArcTrigDeriv n (v:_) =
      sTakePartialDerivativeEx m wrt (Apply (noSemCom $ Csymbol (Just "transc1") n) [
         noSemCom $ Apply (noSemCom $ Csymbol (Just "arith1") "divide") [noSemCom $ Cn (CnInteger 1), v]
                                        ])
    inverseArcTrigDeriv n [] = fail "Attempt to take derivative of inverse arc trig function with no arguments"
    takePartialDerivativeRounding
      -- It is not differentiable over R, but is 0 at afm points in any finite range.
      | name == "ceiling" = return $ Cn (CnInteger 0)
      | name == "floor" = return $ Cn (CnInteger 0)
      | name == "round" = return $ Cn (CnInteger 0)
      | name == "trunc" = return $ Cn (CnInteger 0)
      | otherwise = fail $ "Don't know how to take derivative of rounding1 function " ++ name

takePartialDerivativeEx m (wrtv, wrtd) (Apply (WithMaybeSemantics _ (WithCommon _
                                 (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "calculus1") "diff"))) [f]
                                 ))) [t])
  | Just n <- tryGetLambdaVariable (stripSemCom f), Just (_, _, v) <- M.lookup n m =
    if wrtd == 1 && wrtv == v then return (Cn (CnInteger 1)) else return (Cn (CnInteger 0))

takePartialDerivativeEx _ _ ex = fail $ "Don't know how to take a partial derivative of " ++ show ex

daeBoilerplateCode =
  "int solveForIVs(double t, double* CONSTANTS, double* STATES, double* RATES);\n\
  \int solveResiduals(double t, double* CONSTANTS, double* STATES, double* RATES, double* residuals);\n\
  \int paramsToState(double* params, double* STATES, double* RATES);\n\
  \#include \"cellml12-solver-protocol.h\"\n"
daeInitialPrefix = "int solveForIVs(double t, double* CONSTANTS, double* STATES, double* RATES)\n{\n"
daeInitialSuffix = "}\n"
daeResidualPrefix = "int solveResiduals(double t, double* CONSTANTS, double* STATES, double* RATES, double* residuals)\n{\n"
daeResidualSuffix = "}\n"
paramToStatePrefix = "int paramsToState(double* params, double* STATES, double* RATES)\n{\n"
paramToStateSuffix = "  return 0;\n}\n"

daeJacobianPrefix = "void solveJacobianxVec(double t, double* CONSTANTS, double* STATES, double* RATES, double* v, double* Jv)\n{\n"
daeJacobianSuffix = "}\n"