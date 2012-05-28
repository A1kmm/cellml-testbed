{-# LANGUAGE PatternGuards, Rank2Types, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
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

data DAEIntegrationSetup = DAEIntegrationSetup {
    daeModel :: SimplifiedModel,
    daeBoundVariable :: VariableID
    }
data DAEIntegrationProblem = DAEIntegrationProblem {
    daeParameterOverrides :: [((VariableID, Int), Double)],
    daeBVarRange :: (Double, Double),
    daeRelativeTolerance :: Double,
    daeAbsoluteTolerance :: Double
  }

data DAEIntegrationResult = DAEIntegrationResults {
  daeResultIndices :: M.Map (VariableID, Int) Int,
  daeResults :: [(Double, U.UArray Int Double)]
                                                  }

data Expression = ConstantDoubleExpression Double | ConstantBooleanExpression Bool |
                  CodeString String | DependentExpression (M.Map String Expression -> Expression)

data NumericalEntry = NumericalError String | NumericalSuccess | NumericalData (U.UArray Int Double)

class Monad m => MonadSolver m where
  liftSolver :: Monad m2 => SolverT m2 a -> m a

type SolverInternalsT m a = ReaderT (Handle, M.Map (VariableID, Int) Int, DAEIntegrationSetup) (StateT LBS.ByteString m) a
newtype SolverT m a = SolverT {unsolver :: SolverInternalsT m a }

instance Monad m => Monad (SolverT m) where
  return x = SolverT $ return x
  (SolverT x) >>= f = SolverT $ x >>= (\v -> unsolver (f v))
instance MonadTrans SolverT where
  lift x = SolverT (lift . lift $ x)
instance MonadIO m => MonadIO (SolverT m) where
  liftIO = lift . liftIO
instance (MonadIO m, MonadIOUnwrappable (ReaderT (Handle, M.Map (VariableID, Int) Int, DAEIntegrationSetup) (StateT LBS.ByteString m))) =>
         MonadIOUnwrappable (SolverT m) where
  type MonadIOWrapType (SolverT m) = MonadIOWrapType (ReaderT (Handle, M.Map (VariableID, Int) Int, DAEIntegrationSetup) (StateT LBS.ByteString m))
  type MonadIOStateType (SolverT m) = MonadIOStateType (ReaderT (Handle, M.Map (VariableID, Int) Int, DAEIntegrationSetup) (StateT LBS.ByteString m))
  unwrapState = SolverT unwrapState
  unwrapMonadIO s (SolverT m) = unwrapMonadIO s m
  rewrapMonadIO s v = SolverT (rewrapMonadIO s v)
instance Monad m => MonadReader (SolverT m) where
  type EnvType (SolverT m) = EnvType (ReaderT (Handle, M.Map (VariableID, Int) Int, DAEIntegrationSetup) (StateT LBS.ByteString m))
  ask = SolverT ask
  local f (SolverT m) = SolverT (local f m)
instance Monad m => MonadState (SolverT m) where
  type StateType (SolverT m) = StateType (ReaderT (Handle, M.Map (VariableID, Int) Int, DAEIntegrationSetup) (StateT LBS.ByteString m))
  get = SolverT get
  put v = SolverT (put v)

solveModelWithParameters :: DAEIntegrationProblem -> SolverT (ErrorT String IO) DAEIntegrationResult
solveModelWithParameters p = do
  (hOut, vmap, setup) <- ask
  let iBound = vmap !! (daeBoundVariable setup, 0)
  
  -- Send the request for the problem to be solved...
  liftIO . BS.hPutStr hOut $ runPut $ do
    -- Overrides...
    let pOverrides = mapMaybe (\(vardeg, newv) -> liftM (\v -> (v, newv)) $ M.lookup vardeg vmap) . daeParameterOverrides $ p
    flip putListOf pOverrides $ putTwoOf (putWord32host . fromIntegral) (putFloat64le)
    -- Bvar range...
    putTwoOf putFloat64le putFloat64le . daeBVarRange $ p
    putFloat64le . daeRelativeTolerance $ p
    putFloat64le . daeAbsoluteTolerance $ p
  liftIO (hFlush hOut)
  
  -- Now retrieve the response from the solver...
  s0 <- get
  (numericalResult, rest) <- lift . ErrorT . return $ flip runGetLazyState s0 $ getOutput $ M.size vmap
  put rest
  case numericalResult of
    (NumericalError str):d -> fail $ "Numerical problem: " ++ str
    NumericalSuccess:d -> do
      let r = (map (\x -> (x!iBound, x)) . mapMaybe onlyData $ d)
      return $ DAEIntegrationResults { daeResultIndices = vmap, daeResults = r }
    _ -> fail "Incomplete results - solver program may have crashed"

-- | Runs a solver monad over a particular model with a particular setup.
runSolverOnDAESimplifiedModel :: SimplifiedModel -> DAEIntegrationSetup -> SolverT (ErrorT String IO) a -> ErrorT String IO a
runSolverOnDAESimplifiedModel m' setup solver =
  case makeVarMapFixingHighers m' setup of
    Left e -> fail e
    Right (m, vmap) ->
      let
        -- TODO: Simplify & partially evaluate model first.
        mUnits = substituteUnits m
        -- TODO: Simplify again after putting units conversions in. We need to do it
        -- twice, because we need to simplify derivative degrees to get the units, but
        -- the units conversions might be able to be simplified out.
        code = writeDAECode vmap mUnits setup
      in
        withSystemTempDirectory' "daesolveXXX" $ \fn -> do
          liftIO $ LBS.writeFile (fn </> "solve.c") code
          ec <- liftIO $ system $ showString "gcc -O3 " . showString (fn </> "solve.c") . showString " -o " $ fn </> "solve"
          if ec /= ExitSuccess then fail "Cannot find C compiler" else do
          (Just i, Just o, _, p) <-
            liftIO $ createProcess $
            CreateProcess { cmdspec = RawCommand (fn </> "solve") [], cwd = Nothing,
                            env = Nothing, std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit,
                            close_fds = False, create_group = False }
          ecMvar <- liftIO newEmptyMVar
          liftIO $ forkIO $ do
            ec' <- waitForProcess p
            putMVar ecMvar ec'
          lStr <- liftIO $ LBS.hGetContents o
          ret <- evalStateT (runReaderT (unsolver solver) (i, vmap, setup)) lStr
          ec' <- liftIO $ takeMVar ecMvar
          when (ec' /= ExitSuccess) $ fail "Problem executing integrator program"
          return ret

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
        conversionFactor convertFrom@(CanonicalUnits fromOffs fromMup _) convertTo@(CanonicalUnits toOffs toMup _) =
          ((toMup / fromMup) * toOffs - fromOffs, toMup / fromMup)
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
                                         [WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci bvarName)))]
                                      )
          | Just (varOffs, varMup) <- M.lookup varName conversionFactors,
            Just (_, bvarMup) <- M.lookup bvarName conversionFactors = convertOneVariable expr (varOffs, varMup / bvarMup)
        applyConversions bvar expr@(Apply (WithMaybeSemantics _ (WithCommon _ (
                                         Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "calculus1") "nthdiff")))
                                           [WithMaybeSemantics _ (WithCommon _ (Cn cpart)),
                                            WithMaybeSemantics _ (WithCommon _ (
                                            Bind (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "fns1") "lambda")))
                                                 _ (WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci varName))))))]
                                                                              )))
                                         [WithMaybeSemantics _ (WithCommon _ (ASTCi (Ci bvarName)))]
                                      )
          | Just (varOffs, varMup) <- M.lookup varName conversionFactors,
            Just (_, bvarMup) <- M.lookup bvarName conversionFactors = convertOneVariable expr (varOffs, varMup / (bvarMup ** (cpToDouble cpart)))
        applyConversions bvar (Apply op operands) = Apply op (map (ignoringSemCom (applyConversions bvar)) operands)
        applyConversions bvar (Bind op newBvar operand) =
          let
            bvar' = foldl' (\m (WithMaybeSemantics _ (WithCommon _ (Ci str))) -> S.insert str m) bvar newBvar
          in
           Bind op newBvar (ignoringSemCom (applyConversions bvar') operand)
        applyConversions _ x = x
      in
       Assertion (ignoringSemCom (applyConversions S.empty) expr, AssertionContext newMap)

writeDAECode :: M.Map (VariableID, Int) Int -> SimplifiedModel -> DAEIntegrationSetup -> LBS.ByteString
writeDAECode vars model problem =
  runPutLazy $ do
    putByteString (BS.pack daeBoilerplateCode)
    putByteString (BS.pack daeResidualPrefix)
    putModelResiduals vars model
    putByteString (BS.pack daeResidualSuffix)
    -- To do: analytic Jacobian?
    -- To do: uncertain starting parameters.

stripSemCom (WithMaybeSemantics _ (WithCommon _ v)) = v

putModelResiduals vars (SimplifiedModel { assertions = assertions }) =
  let
    isRecoverableError = 
      flip concatMap assertions $ \(Assertion (expr, AssertionContext varMap)) ->
      case (stripSemCom expr) of
        -- Apply ()
        _ -> ""
      

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

eraseConstantVariables :: SimplifiedModel -> SimplifiedModel
eraseConstantVariables m =
  let
    isConstant a@(Assertion (WithMaybeSemantics _ (WithCommon _ (Apply (WithMaybeSemantics _ (WithCommon _ (Csymbol (Just "relation1") "eq")))
                                                                 _)), _)) =
      case assertionVariables a of
        _:_:_ -> True
        _ -> False
    isConstant _ = False
    newAssertions = filter (not . isConstant) (assertions m)
    vset = S.fromList $ concatMap assertionVariables newAssertions
    vi = M.filterWithKey (\k a -> not (k `S.member` vset)) (unvariableInfo $ variableInfo m)
  in
   SimplifiedModel { variableInfo = VariableInfo vi, assertions = newAssertions }

makeVarMapFixingHighers :: SimplifiedModel -> DAEIntegrationSetup -> Either String (SimplifiedModel, M.Map (VariableID, Int) Int)
makeVarMapFixingHighers m p =
  let
    (_, cuBvar):_ = (unvariableInfo . variableInfo $ m) !! (daeBoundVariable p)
    m' = eraseConstantVariables m
    varDeg = findMaximalDiffVariableDegreeWRT m' (daeBoundVariable p)
    (_, allRates) = partition (\(v, d) -> d==0) $ M.toList varDeg
    (basicRates, higherRates) = partition (\(v, d) -> d==1) allRates
    origMaxVar = maximum (map (\(VariableID i) -> i) (M.keys (unvariableInfo . variableInfo $ m)))
    (higherMap, m'', _) =
      flip (flip foldl' (M.empty, m', origMaxVar)) higherRates $
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
                                                                  ])):(assertions m'') },
             vMax + 1)
    allDegs = (S.fromList (M.keys higherMap)) `S.union`
              (S.fromList (map (\v -> (v, 0)) (M.keys . unvariableInfo . variableInfo $ m)))
  in
   Right (m'', M.fromList $ zip (S.toList allDegs) [0..])

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

convertUnits :: Expression -> CanonicalUnits -> CanonicalUnits -> Expression
convertUnits (ConstantDoubleExpression v) src dst =
  ConstantDoubleExpression (v * (cuMultiplier src / cuMultiplier dst) + cuOffset src - cuOffset dst)
convertUnits (CodeString s) src dst =
  let
    mupStr = if cuMultiplier src == cuMultiplier dst then "" else " * " ++ show (cuMultiplier src / cuMultiplier dst)
    offStr = if cuOffset src == cuOffset dst then "" else " + " ++ show (cuOffset src - cuOffset dst)
  in
   CodeString (s ++ mupStr ++ offStr)
convertUnits (DependentExpression f) src dst = DependentExpression $ \m -> convertUnits (f m) src dst
convertUnits ex _ _ = ex

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

simplifyMathAST m (Apply op@(WithMaybeSemantics _ (WithCommon _ (Csymbol (Just cd) expr))) ops) = do
  ops' <- mapM (simplifyMaths m) ops
  if all isConstantReal ops
    then tryConstantApply cd expr (map (\(WithMaybeSemantics _ (WithCommon _ (Cn p))) -> cpToDouble p) ops')
    else
      if all isConstantBool ops
        then tryConstantApplyBool cd expr (map toConstantBool ops')
        else tryPartialApplyCombine cd expr ops'
  -- To do: piecewise functions.

simplifyMathAST m cs@(Csymbol (Just "nums1") s)
  | Just v <- tryNums1Constant s = return v
  | otherwise = return cs

simplifyMathAST m (ASTCi (Ci s))
  | Just v <- M.lookup s m = v

simplifyMathAST m (Apply op ops) =
  Apply <$> (simplifyMaths m op) <*> mapM (simplifyMaths m) ops
  
simplifyMathAST m (Bind op bvars operand) =
  Bind <$> (simplifyMaths m op) <*> (return bvars) <*> (simplifyMaths m operand)

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

tryConstantApply :: Monad m => String -> String -> [Double] -> ErrorT InvalidCellML m AST
tryConstantApply "arith1" o l = tryArith1ConstantApply o l
tryConstantApply "transc1" o l = tryTransc1ConstantApply o l
tryConstantApply "relation1" o l = tryRelation1ConstantApply o l
tryConstantApply "rounding1" o l = tryRounding1ConstantApply o l
tryConstantApply "integer1" o l = tryInteger1ConstantApply o l
tryConstantApply "nums1" "rational" [v1, v2] = return . Cn . CnReal $ v1 / v2
tryConstantApply cd o _ = fail ("Unrecognised content dictionary: " ++ cd)

tryConstantApplyBool :: Monad m => String -> String -> [Bool] -> ErrorT InvalidCellML m AST
tryConstantApplyBool "logic1" o l = tryLogic1ConstantApply o l
tryConstantApplyBool "relation1" o l = tryRelation1ConstantApplyBool o l
tryConstantApplyBool cd o _ = fail ("Unrecognised content dictionary for boolean expression: " ++ cd)

tryArith1ConstantApply "plus" ops = return . Cn . CnReal . sum $ ops
tryArith1ConstantApply "times" ops = return . Cn . CnReal . sum $ ops
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

daeBoilerplateCode = "DAE boilerplate goes here"
daeResidualPrefix = "int daeSignature() {"
daeResidualSuffix = "}"
