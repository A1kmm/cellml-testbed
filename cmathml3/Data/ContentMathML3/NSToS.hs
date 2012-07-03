{-# LANGUAGE RankNTypes,PatternGuards #-}

-- | A module for the translation of non-strict MathML 3 to strict form
module Data.ContentMathML3.NSToS (nsToStrict)
where

import Data.ContentMathML3.Structure
import Network.URL
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Array.Unboxed as U
import Data.List
import Control.Monad
import Data.ContentMathML3.Parser

import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.QualifiedName as XN

import Data.Generics.Uniplate.Data

-- | Take a non-strict structure, and make a strict structure, possibly giving an error:
nsToStrict :: NSASTC -> Either String ASTC
nsToStrict = strictNSToS . nsToStrictNS

-- | Take a non-strict structure, and make another non-strict structure that only uses
-- | strict features.
nsToStrictNS :: NSASTC -> NSASTC
nsToStrictNS inp = let
  p1r = transformBi pass1 inp
  p2r = transformBi pass2 p1r
  p3r = transformBi pass3 p2r
  p4r = transformBi pass4 p3r
  p5r = transformBi pass5 p4r
  p6r = transformBi pass6 p5r
  p6br = transformBi pass6b p5r
  p7r = transformBi pass7 p6br
  p8r = transformBi pass8 p7r
  in
   transformBi pass9 p8r
  
-- | Take a non-strict structure that only uses strict features, and convert to a
-- | strict structure, giving an error if any non-strict features are found.
strictNSToS :: NSASTC -> Either String ASTC
strictNSToS (WithMaybeSemantics s (WithNSCommon c v)) = do
  common <- strictNSCommonToS c
  expr <- strictNSExprToS v
  return $ WithMaybeSemantics s (WithCommon common expr)

strictNSCommonToS :: NSCommon -> Either String Common
strictNSCommonToS (NSCommon { nsCommon = c, nsCommonDefinitionURL = u, nsCommonEncoding = e }) =
  if u /= Nothing then
    Left ("Found a residual definitionURL: " ++ (fromJust u))
  else
    if e /= Nothing then
      Left ("Found a residual encoding attribute: " ++ (fromJust e))
    else
      return c

strictNSExprToS :: NSAST -> Either String AST
strictNSExprToS cn@(NSCn (Just _) _) = Left ("Found a cn with a base: " ++ show cn)
strictNSExprToS (NSCn _ (NSCnInteger v)) = return $ Cn (CnInteger v)
strictNSExprToS (NSCn _ (NSCnReal v)) = return $ Cn (CnReal v)
strictNSExprToS (NSCn _ (NSCnDouble v)) = return $ Cn (CnDouble v)
strictNSExprToS (NSCn _ (NSCnHexDouble v)) = return $ Cn (CnHexDouble v)
strictNSExprToS cn@(NSCn _ _) = Left ("Found a cn of a type invalid for strict MathML: " ++ show cn)
strictNSExprToS (NSASTCi ci) = liftM ASTCi (strictNSCiToS ci)
strictNSExprToS cs@(NSCsymbol _ (Just _) _) = Left ("Found a csymbol with a type attribute: " ++ show cs)
strictNSExprToS cs@(NSCsymbol _ _ (NSCiMGlyph s)) = Left ("Found a csymbol with mglyph content: " ++ show cs)
strictNSExprToS cs@(NSCsymbol _ _ (NSCiPresentationExpression s)) = Left ("Found a csymbol with presentation content: " ++ show cs)
strictNSExprToS (NSCsymbol cd _ (NSCiText name)) = return $ Csymbol cd name
strictNSExprToS (NSCs s) = return $ Cs s
strictNSExprToS a@(NSApply { nsApplyBvar = _:_ }) = Left ("Found an apply with bvar children: " ++ show a)
strictNSExprToS a@(NSApply { nsApplyQualifier = _:_ }) = Left ("Found an apply with qualifier children: " ++ show a)
strictNSExprToS a@(NSApply { nsApplyOperator = nsop, nsApplyOperands = nsoperands }) = do
  op <- strictNSToS nsop
  operands <- sequence . map strictNSToS $ nsoperands
  return $ Apply op operands
strictNSExprToS a@(NSBind { nsBindQualifiers = _:_ }) = Left ("Found a bind with qualifier children: " ++ show a)
strictNSExprToS a@(NSBind { nsBindOperands = [] }) = Left ("Found a bind with no operands: " ++ show a)
strictNSExprToS a@(NSBind { nsBindOperands = _:(_:_) }) = Left ("Found a bind with more than one operand: " ++ show a)
strictNSExprToS a@(NSBind { nsBindOperator = nsop, nsBindBvar = nsbvar, nsBindOperands = nsoperand:_ }) = do
  op <- strictNSToS nsop
  bvars <- sequence . map strictNSBvarToS $ nsbvar
  operand <- strictNSToS nsoperand
  return $ Bind op bvars operand
strictNSExprToS a@(NSError nset nsea) = do
  et <- strictNSToS nset
  ea <- mapM strictNSToS nsea
  return $ Error et ea
strictNSExprToS a@(NSCBytes s) = return $ CBytes s
strictNSExprToS x = Left ("Symbol found that is not present in strict MathML 3: " ++ show x)

strictNSCiToS ci@(NSCi (Just _) _) = Left ("ci element has a type attribute" ++ show ci)
strictNSCiToS ci@(NSCi _ (NSCiMGlyph _)) = Left ("Found mglyph content in a ci element: " ++ show ci)
strictNSCiToS ci@(NSCi _ (NSCiPresentationExpression _)) = Left ("Found presentation content in a ci element: " ++ show ci)
strictNSCiToS (NSCi _ (NSCiText t)) = return $ Ci t

strictNSBvarToS bv@(NSBvar _ (Just _)) = Left ("Found a bvar with degree attribute: " ++ show bv)
strictNSBvarToS bv@(NSBvar (WithMaybeSemantics s (WithNSCommon nsc nsci)) _) = do
  c <- strictNSCommonToS nsc
  ci <- strictNSCiToS nsci
  return $ WithMaybeSemantics s (WithCommon c ci)

pass1 :: NSAST -> NSAST
pass1 = normaliseNSBind

normaliseNSBind (NSBind op bvar qual operands)
  | not (null bvar) || not (null qual) || length (take 2 operands) > 1 =
    NSApply op bvar qual operands
normaliseNSBind (NSRelation op l) = NSApply op [] [] l
normaliseNSBind (NSFunction (WithMaybeSemantics _ (WithNSCommon _ ex))) = ex
normaliseNSBind x = x

simpleCsymbol cd cn = (NSCsymbol (Just cd) Nothing (NSCiText cn))

isQualDegree (NSQualDegree _) = True
isQualDegree _ = False
tryGetQualDegree l = do
  qd <- find isQualDegree l
  let NSQualDegree deg = qd
  return deg

pass2 :: NSAST -> NSAST
pass2 = exprPass2
exprPass2 (NSApply (WithMaybeSemantics s (WithNSCommon c NSDiff)) bv@[NSBvar (WithMaybeSemantics sx (WithNSCommon cx x)) Nothing] [] e@[expr]) =
  NSApply (noNSSemCom $
             NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "calculus1" "diff") [] []
                     [
                       noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] e
                     ]) [] [] [WithMaybeSemantics sx (WithNSCommon cx $ NSASTCi x)]
exprPass2 (NSApply (WithMaybeSemantics s (WithNSCommon c NSDiff)) bv@[NSBvar (WithMaybeSemantics sx (WithNSCommon cx x)) (Just d)] [] e@[expr]) =
  NSApply (noNSSemCom $
             NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "calculus1" "nthdiff") [] []
                     [
                       d,
                       noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] e
                     ]) [] [] [WithMaybeSemantics sx (WithNSCommon cx $ NSASTCi x)]
-- Don't touch partialdiff with no bound variables...
exprPass2 x@(NSApply (WithMaybeSemantics _ (WithNSCommon _ NSPartialdiff)) [] _ _) = x
-- Otherwise, transform it...
exprPass2 (NSApply (WithMaybeSemantics s (WithNSCommon c NSPartialdiff)) bvars quals e@[expr]) =
  let
    bvarExps = map (\(NSBvar (WithMaybeSemantics _ (WithNSCommon _ ci)) _) -> noNSSemCom $ NSASTCi ci) bvars
    bvarDegs = map (\(NSBvar _ deg) -> fromMaybe (noNSSemCom $ NSCn Nothing (NSCnInteger 1)) deg) bvars
    totalDegree = fromMaybe (noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "arith1" "plus") [] [] bvarDegs) $ tryGetQualDegree quals
  in
   NSApply (noNSSemCom $
            NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "calculus1" "partialdiffdegree") [] []
            [
              noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "list1" "list") [] [] bvarExps,
              totalDegree,
              noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bvars [] e
            ]) [] [] bvarExps

exprPass2 (NSApply (WithMaybeSemantics s (WithNSCommon c NSLimit))
                   bv@[NSBvar (WithMaybeSemantics _ (WithNSCommon _ x)) _]
                   [NSQualDomain (NSCondition (WithMaybeSemantics _
                                               (WithNSCommon _
                                                (NSApply (WithMaybeSemantics _ (WithNSCommon _ (NSTendsto _)))
                                                         [] [] [WithMaybeSemantics _ (WithNSCommon _ (NSASTCi x2)), approach]))))]
                   ex@[_]
          )
  | x == x2 =
    NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "limit1" "limit") [] []
            [approach, noNSSemCom $ simpleCsymbol "limit1" "null",
             noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] ex]

exprPass2 (NSTendsto _) = NSASTCi (NSCi Nothing (NSCiText "tendsto"))

exprPass2 (NSApply (WithMaybeSemantics s (WithNSCommon c NSRoot))
                   []
                   quals
                   [ex]) =
  let
    isDegree (NSQualDegree _) = True
    isDegree _ = False
    degr = maybe (noNSSemCom $ NSCn Nothing $ NSCnInteger 2) (\(NSQualDegree v) -> v) $ find isDegree quals
  in
   NSApply (WithMaybeSemantics s (WithNSCommon c (simpleCsymbol "arith1" "root")))
           [] []
           [ex, degr]

exprPass2 (NSApply (WithMaybeSemantics s (WithNSCommon c NSLog))
                   []
                   quals
                   [ex]) =
  let
    isLogbase (NSQualLogbase _) = True
    isLogbase _ = False
    base = maybe (noNSSemCom $ NSCn Nothing $ NSCnInteger 10) (\(NSQualLogbase v) -> v) $ find isLogbase quals
  in
   NSApply (WithMaybeSemantics s (WithNSCommon c (simpleCsymbol "transc1" "log")))
           [] []
           [base, ex]

exprPass2 (NSApply (WithMaybeSemantics s (WithNSCommon c NSMoment))
                   []
                   quals
                   exl) =
  let
    isMomentabout (NSQualMomentabout _) = True
    isMomentabout _ = False
    momentabout = maybe (noNSSemCom $ NSCn Nothing $ NSCnReal 0) (\(NSQualMomentabout v) -> v) $ find isMomentabout quals
    isDegree (NSQualDegree _) = True
    isDegree _ = False
    degr = maybe (noNSSemCom $ NSCn Nothing $ NSCnInteger 1) (\(NSQualDegree v) -> v) $ find isDegree quals
    isData = length exl /= 1
    cs = if isData then simpleCsymbol "s_data1" "moment" else simpleCsymbol "s_dist1" "moment"
  in
   NSApply (WithMaybeSemantics s (WithNSCommon c cs))
           [] []
           (degr:momentabout:exl)

exprPass2 x = domainedRewrite x

pass3 :: NSAST -> NSAST
pass3 = exprPass3a . exprPass3

exprPass3 :: forall a . CanHaveDomain a => a -> a
exprPass3 a
  | Just l <- lowlimit, Just u <- uplimit =
    setDomains a (NSDomainOfApplication
                    (noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "interval1" "interval") [] [] [l, u]):
                  excludingLimits)
  | _:_ <- conditions =
      setDomains a ((NSDomainOfApplication . noNSSemCom $
                       NSApply (noNSSemCom $ simpleCsymbol "set1" "suchthat") [] []
                               [rexp, noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] [conjCondition]]):
                    excludingConditionsAndDOA
                   )
  where
    bv = getBvars a
    quals = getDomains a
    isLowlimit (NSLowlimit _) = True
    isLowlimit _ = False
    lowlimit = liftM (\(NSLowlimit x) -> x) $ find isLowlimit quals
    isUplimit (NSUplimit _) = True
    isUplimit _ = False
    uplimit = liftM (\(NSUplimit x) -> x) $ find isUplimit quals
    excludingLimits = filter (\x -> not (isLowlimit x || isUplimit x)) quals
    isCondition (NSCondition _) = True
    isCondition _ = False
    conditions = map (\(NSCondition c) -> c) $ filter isCondition quals
    conjCondition = case conditions of
      cond:[] -> cond
      _ -> noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "logic1" "and") [] [] conditions
    excludingConditionsAndDOA = filter (\x -> not (isCondition x || isDomainQualifierDOA x)) quals
    mdoa = listToMaybe $ mapMaybe domainQualifierToMaybeDOA quals
    mvartype | (NSBvar (WithMaybeSemantics _ (WithNSCommon _ (NSCi (Just (NSStrictVariableType vt)) _))) _):_ <- bv
         = Just vt
             | otherwise = Nothing
    rexp | Just doa <- mdoa = doa
         | mvartype == Just CiInteger  = noNSSemCom $ simpleCsymbol "setname1" "Z"
         | mvartype == Just CiReal     = noNSSemCom $ simpleCsymbol "setname1" "R"
         | mvartype == Just CiRational = noNSSemCom $ simpleCsymbol "setname1" "Q"
         | mvartype == Just CiComplex  = noNSSemCom $ simpleCsymbol "setname1" "C"
         | mvartype == Just CiComplexPolar = noNSSemCom $ simpleCsymbol "setname1" "C"
         | mvartype == Just CiComplexCartesian = noNSSemCom $ simpleCsymbol "setname1" "C"
         | otherwise = noNSSemCom $ NSASTCi $ NSCi Nothing (NSCiText "R")

exprPass3 x = x

exprPass3a (NSApply op bvs quals exl)
  | _:(_:_) <- doas = NSApply op bvs ((NSQualDomain . NSDomainOfApplication $ singleDOA):excludingDOAs) exl
  where
    doas = mapMaybe qualifierToMaybeDOA quals
    excludingDOAs = filter (not . isQualifierDOA) quals
    singleDOA = noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "set1" "intersect") [] [] doas
exprPass3a x = x

pass4 :: NSAST -> NSAST
pass4 = exprPass4a {- `extT` (mkT exprPass4b) -}

domainQualifierToMaybeDOA (NSDomainOfApplication doa) = Just doa
domainQualifierToMaybeDOA _ = Nothing
qualifierToMaybeDOA (NSQualDomain (NSDomainOfApplication doa)) = Just doa
qualifierToMaybeDOA _ = Nothing
isQualifierDOA (NSQualDomain (NSDomainOfApplication _)) = True
isQualifierDOA _ = False
isDomainQualifierDOA (NSDomainOfApplication _) = True
isDomainQualifierDOA _ = False

exprPass4a (NSSet bvars quals [expr])
  | not (null doaQuals) =
    foldl' (\inner qual -> NSApply (noNSSemCom $ simpleCsymbol "set1" "map") [] [] [noNSSemCom inner, qual])
           (NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bvars [] [expr])
           doaQuals
  where
    doaQuals = mapMaybe domainQualifierToMaybeDOA quals
    
-- To do: Specification refers to rule for vector which doesn't exist.
-- To do: Specification refers to rule for matrix which doesn't exist.
-- To do: Specification refers to rule for matrixrow which doesn't exist.
-- exprPass4a (NSVector bvars dom expr) = 
    
exprPass4a (NSLambda bvars domain expr)
  | null doas = NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bvars [] [expr]
  | otherwise = foldl' (\inner doa -> NSApply (noNSSemCom $ simpleCsymbol "fns1" "restrict") [] [] [noNSSemCom inner, doa])
                       (NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bvars [] [expr])
                       doas
  where
    doas = mapMaybe domainQualifierToMaybeDOA domain

exprPass4a (NSPiecewise (cases, motherwise)) =
  let
    lotherwise = maybeToList $
                   liftM (\(WithNSCommon c ow) ->
                             WithMaybeSemantics Nothing . WithNSCommon c $ NSApply (noNSSemCom $ simpleCsymbol "piece1" "otherwise") [] [] [ow])
                         motherwise
    lcases = map (\(WithNSCommon c (val, cond)) -> WithMaybeSemantics Nothing $
                                                   WithNSCommon c (NSApply (noNSSemCom $ simpleCsymbol "piece1" "piece") [] [] [val, cond])) cases
    lchild = lcases ++ lotherwise
  in
   NSApply (noNSSemCom $ simpleCsymbol "piece1" "piecewise") [] [] lchild

exprPass4a x = x

{- This rule only makes sense for intervals that aren't qualifiers, which our
   structure doesn't allow for...

closureToOperator (Just "open") = simpleCsymbol "interval1" "interval_oo"
closureToOperator (Just "open-closed") = simpleCsymbol "interval1" "interval_oc"
closureToOperator (Just "closed-open") = simpleCsymbol "interval1" "interval_co"
closureToOperator _ = simpleCsymbol "interval1" "interval_cc"

exprPass4b (NSInterval closure l h) =
  noNSSemCom $
    NSApply (noNSSemCom $ closureToOperator closure) [] [] [l, h]
-}

pass5 :: NSAST -> NSAST
pass5 = exprPass5

statsOrMinMaxToSym NSMin = Just $ simpleCsymbol "minmax1" "min"
statsOrMinMaxToSym NSMax = Just $ simpleCsymbol "minmax1" "max"
statsOrMinMaxToSym NSMean = Just $ simpleCsymbol "s_data1" "mean"
statsOrMinMaxToSym NSSdev = Just $ simpleCsymbol "s_data1" "sdev"
statsOrMinMaxToSym NSVariance = Just $ simpleCsymbol "s_data1" "variance"
statsOrMinMaxToSym NSMedian = Just $ simpleCsymbol "s_data1" "median"
statsOrMinMaxToSym NSMode = Just $ simpleCsymbol "s_data1" "mode"
statsOrMinMaxToSym _ = Nothing

statsOrMinMaxToDistSym NSMean = Just $ simpleCsymbol "s_dist1" "mean"
statsOrMinMaxToDistSym NSSdev = Just $ simpleCsymbol "s_dist1" "sdev"
statsOrMinMaxToDistSym NSVariance = Just $ simpleCsymbol "s_dist1" "variance"
statsOrMinMaxToDistSym NSMedian = Just $ simpleCsymbol "s_dist1" "median"
statsOrMinMaxToDistSym NSMode = Just $ simpleCsymbol "s_dist1" "mode"
statsOrMinMaxToDistSym x = statsOrMinMaxToSym x

exprPass5 (NSApply (WithMaybeSemantics _ (WithNSCommon _ op)) bv quals v)
  | Just sym <- statsOrMinMaxToSym op =
    case (doas, v) of
      ([], el:[]) ->
        NSApply (noNSSemCom . fromJust $ statsOrMinMaxToDistSym op) [] [] [el]
      (doa:_, el:_) ->
        NSApply (noNSSemCom sym) [] []
                [noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "set1" "map") [] []
                               [noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] (el:doa:[])]]
      ([], ell) ->
        NSApply (noNSSemCom sym) [] [] [noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "set1" "set") [] [] ell]
  where
    doas = mapMaybe qualifierToMaybeDOA quals

exprPass5 (NSApply (WithMaybeSemantics _ (WithNSCommon _ NSExists)) bv@((NSBvar (WithMaybeSemantics s (WithNSCommon c bvar1)) _):_) quals (v:_))
  | not (null doas) = NSBind (noNSSemCom $ simpleCsymbol "quant1" "exists") bv [] [
    noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "logic1" "and") [] [] [
       noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "set1" "in") [] [] [WithMaybeSemantics s $ WithNSCommon c $ NSASTCi bvar1, head doas],
       v
                                                                           ]
    ]
  where
    doas = mapMaybe qualifierToMaybeDOA quals

exprPass5 (NSApply (WithMaybeSemantics _ (WithNSCommon _ NSForall)) bv@((NSBvar (WithMaybeSemantics s (WithNSCommon c bvar1)) _):_) quals (v:_))
  | not (null doas) = NSBind (noNSSemCom $ simpleCsymbol "quant1" "forall") bv [] [
    noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "logic1" "implies") [] [] [
       noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "set1" "in") [] [] [WithMaybeSemantics s $ WithNSCommon c $ NSASTCi bvar1, head doas],
       v
                                                                           ]
    ]
  where
    doas = mapMaybe qualifierToMaybeDOA quals

exprPass5 x = domainedRewrite x

domainedRewrite (NSApply (WithMaybeSemantics s (WithNSCommon c NSInt)) bv@[NSBvar (WithMaybeSemantics sx (WithNSCommon cx x)) Nothing] [] e@[expr]) =
  NSApply (noNSSemCom $
             NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "calculus1" "int") [] []
                     [
                       noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] e
                     ]) [] [] [WithMaybeSemantics sx (WithNSCommon cx $ NSASTCi x)]

domainedRewrite (NSApply (WithMaybeSemantics s (WithNSCommon c NSInt))
           bv@[_] quals e@[expr])
  | doa:_ <- doas =
    NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "calculus1" "defint") [] []
            [
              doa,
              noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] e
            ]
  where
    doas = mapMaybe qualifierToMaybeDOA quals

domainedRewrite (NSApply (WithMaybeSemantics s (WithNSCommon c NSInt))
                   bv@[_] quals e@[expr])
  | Just (l,u) <- luMaybe =
    NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "calculus1" "defint") [] []
              [
                noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "interval1" "orientated_interval") [] [] [l, u],
                noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] e
              ]
  where
    toMaybeLowlimit (NSQualDomain (NSLowlimit l)) = Just l
    toMaybeLowlimit _ = Nothing
    toMaybeUplimit (NSQualDomain (NSUplimit u)) = Just u
    toMaybeUplimit _ = Nothing
    lMaybe = listToMaybe $ mapMaybe toMaybeLowlimit quals
    uMaybe = listToMaybe $ mapMaybe toMaybeUplimit quals
    luMaybe = liftM2 (,) lMaybe uMaybe
    
domainedRewrite (NSApply (WithMaybeSemantics s (WithNSCommon c NSSum))
                   bv@[_]
                   quals
                   ex@[_]
                )
  | Just (l, u) <- luMaybe =
    NSApply (WithMaybeSemantics s (WithNSCommon c $ simpleCsymbol "arith1" "sum")) [] []
            [noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "interval1" "integer_interval") [] [] [l, u],
             noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] ex]
  where
    toMaybeLowlimit (NSQualDomain (NSLowlimit l)) = Just l
    toMaybeLowlimit _ = Nothing
    toMaybeUplimit (NSQualDomain (NSUplimit u)) = Just u
    toMaybeUplimit _ = Nothing
    lMaybe = listToMaybe $ mapMaybe toMaybeLowlimit quals
    uMaybe = listToMaybe $ mapMaybe toMaybeUplimit quals
    luMaybe = liftM2 (,) lMaybe uMaybe

domainedRewrite (NSApply (WithMaybeSemantics s (WithNSCommon c NSProduct))
                   bv@[_]
                   [NSQualDomain (NSLowlimit l), NSQualDomain (NSUplimit u)]
                   ex@[_]
                ) =
  NSApply (WithMaybeSemantics s (WithNSCommon c $ simpleCsymbol "arith1" "product")) [] []
    [noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "interval1" "integer_interval") [] [] [l, u],
     noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] ex
    ]
domainedRewrite x = x

naryRelnToSym NSEq = Just $ simpleCsymbol "relation1" "eq"
naryRelnToSym NSGt = Just $ simpleCsymbol "relation1" "gt"
naryRelnToSym NSLt = Just $ simpleCsymbol "relation1" "lt"
naryRelnToSym NSGeq = Just $ simpleCsymbol "relation1" "geq"
naryRelnToSym NSLeq = Just $ simpleCsymbol "relation1" "leq"
naryRelnToSym NSSubset = Just $ simpleCsymbol "set1" "subset"
naryRelnToSym NSPrSubset = Just $ simpleCsymbol "set1" "prsubset"
naryRelnToSym _ = Nothing

naryGeneralToSym NSPlus = Just $ simpleCsymbol "arith1" "plus"
naryGeneralToSym NSTimes = Just $ simpleCsymbol "arith1" "times"
naryGeneralToSym NSGcd = Just  $ simpleCsymbol "arith1" "gcd"
naryGeneralToSym NSLcm = Just $ simpleCsymbol "arith1" "lcm"
naryGeneralToSym NSCompose = Just $ simpleCsymbol "fns1" "left_compose"
naryGeneralToSym NSAnd = Just $ simpleCsymbol "logic1" "and"
naryGeneralToSym NSOr = Just $ simpleCsymbol "logic1" "or"
naryGeneralToSym NSXor = Just $ simpleCsymbol "logic1" "xor"
naryGeneralToSym NSSelector = Just $ simpleCsymbol "linalg1" "matrix_selector"
naryGeneralToSym NSUnion = Just $ simpleCsymbol "set1" "union"
naryGeneralToSym NSIntersect = Just $ simpleCsymbol "set1" "intersect"
naryGeneralToSym NSCartesianProduct = Just $ simpleCsymbol "set1" "cartesian_product"

pass6 :: NSAST -> NSAST
pass6 = exprPass6
exprPass6 (NSApply opc@(WithMaybeSemantics _ (WithNSCommon _ op)) bv quals exs)
  | Just doa <- mdoa, [] <- bv =
    (NSApply (noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "fns1" "restriction") [] [] [opc, doa])
             [] [] exs)
  | Just sym <- mrsym, Just doa <- mdoa, ex:_ <- exs = 
      NSApply (noNSSemCom $ simpleCsymbol "fns2" "predicate_on_list") [] []
              [noNSSemCom sym,
               noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "list1" "map") [] []
                              [doa, noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] exs]
              ]
  | Just sym <- mrsym, ex1:ex2:[] <- exs = NSApply (noNSSemCom sym) [] [] exs
  | Just sym <- mrsym = NSApply (noNSSemCom $ simpleCsymbol "fns2" "predicate_on_list") [] []
                               [noNSSemCom sym, noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "list1" "list") [] [] exs]
  | Just sym <- mgsym, Just doa <- mdoa, ex1:[] <- exs =
    NSApply (noNSSemCom $ simpleCsymbol "fns2" "apply_to_list") [] [] [
      noNSSemCom sym,
      noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "list1" "map") [] [] [
        noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] exs
                                                               ]
                                                                      ]
 | Just sym <- mrsym = NSApply (noNSSemCom sym) [] [] exs
  where
    mdoa = listToMaybe $ mapMaybe qualifierToMaybeDOA quals
    mrsym = naryRelnToSym op
    mgsym = naryGeneralToSym op

exprPass6 (NSVector bv quals expr)
  | Just doa <- mdoa =
    NSApply (noNSSemCom $ simpleCsymbol "fns2" "apply_to_list") [] [] [
      noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "linalg2" "vector") [] [] [
         noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "list1" "map") [] [] [
            noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") [] [] expr,
            doa
            ]
         ]
      ]
  where
    mdoa = listToMaybe $ mapMaybe domainQualifierToMaybeDOA quals

exprPass6 (NSMatrixByFunction bv quals expr)
  | Just doa <- mdoa =
    NSApply (noNSSemCom $ simpleCsymbol "fns2" "apply_to_list") [] [] [
      noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "linalg2" "matrix") [] [] [
         noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "list1" "map") [] [] [
            noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") [] [] [expr],
            doa
            ]
         ]
      ]
  where
    mdoa = listToMaybe $ mapMaybe domainQualifierToMaybeDOA quals

exprPass6 (NSMatrixByRow rs) = NSApply (noNSSemCom $ simpleCsymbol "linalg2" "matrix") [] [] (map exprPass6MR rs)

exprPass6MR (WithNSCommon c (NSMatrixRow bv quals (expr:_)))
  | Just doa <- mdoa =
    WithMaybeSemantics Nothing $ WithNSCommon c $
      NSApply (noNSSemCom $ simpleCsymbol "fns2" "apply_to_list") [] [] [
        noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "linalg2" "matrixrow") [] [] [
           noNSSemCom $ NSApply (noNSSemCom $ simpleCsymbol "list1" "map") [] [] [
              noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") [] [] [expr],
              doa
              ]
           ]
        ]
  where
    mdoa = listToMaybe $ mapMaybe domainQualifierToMaybeDOA quals

pass6b :: NSAST -> NSAST
pass6b = exprPass6b
exprPass6b (NSApply op bv@(_:_) quals args) =
  let
    doas = mapMaybe qualifierToMaybeDOA quals
    lambdaArgs = flip map args $ \arg ->
      noNSSemCom $ NSBind (noNSSemCom $ simpleCsymbol "fns1" "lambda") bv [] [arg]
  in
   NSApply op [] [] (doas ++ lambdaArgs)
exprPass6b x = x

pass7 :: NSAST -> NSAST
pass7 = exprPass7

exprPass7 (NSCn base (NSCnENotation a b)) =
  NSApply (noNSSemCom $ simpleCsymbol "bigfloat1" "bigfloat") [] [] [
      noNSSemCom $ NSCn base (NSCnReal a),
      noNSSemCom $ NSCn base (NSCnReal b)
    ]
exprPass7 (NSCn base (NSCnRational a b)) =
  NSApply (noNSSemCom $ simpleCsymbol "nums1" "rational") [] [] [
      noNSSemCom $ NSCn base (NSCnInteger a),
      noNSSemCom $ NSCn base (NSCnInteger b)
    ]
exprPass7 (NSCn base (NSCnComplexCartesian a b)) =
  NSApply (noNSSemCom $ simpleCsymbol "complex1" "complex_cartesian") [] [] [
      noNSSemCom $ NSCn base (NSCnReal a),
      noNSSemCom $ NSCn base (NSCnReal b)
    ]
exprPass7 (NSCn base (NSCnComplexPolar a b)) =
  NSApply (noNSSemCom $ simpleCsymbol "complex1" "complex_polar") [] [] [
      noNSSemCom $ NSCn base (NSCnReal a),
      noNSSemCom $ NSCn base (NSCnReal b)
    ]
exprPass7 (NSCn _ (NSCnOther v1 v2)) = simpleCsymbol v1 v2
exprPass7 (NSCn _ (NSCnConstant "π")) = simpleCsymbol "nums1" "pi"
exprPass7 (NSCn _ (NSCnConstant "ⅇ")) = simpleCsymbol "nums1" "e"
exprPass7 (NSCn _ (NSCnConstant "ⅈ")) = simpleCsymbol "nums1" "i"
exprPass7 (NSCn _ (NSCnConstant "γ")) = simpleCsymbol "nums1" "gamma"
exprPass7 (NSCn _ (NSCnConstant "∞")) = simpleCsymbol "nums1" "infinity"
exprPass7 (NSCn _ (NSCnConstant v)) = simpleCsymbol "other" v
exprPass7 (NSCn (Just 10) v) = NSCn Nothing v
exprPass7 (NSCn (Just base) v) =
  NSApply (noNSSemCom $ simpleCsymbol "nums1" "based_integer") [] [] [
      noNSSemCom $ NSCn Nothing (NSCnInteger base),
      noNSSemCom $ NSCn Nothing v
    ]
-- To do: Remove presentation MathML and name identifiers...
exprPass7 (NSApply (WithMaybeSemantics s (WithNSCommon c NSMinus)) _ _ exs@(ex:[])) =
  NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "arith1" "unary_minus") [] [] exs
exprPass7 (NSApply (WithMaybeSemantics s (WithNSCommon c NSMinus)) _ _ exs) = 
  NSApply (WithMaybeSemantics s $ WithNSCommon c $ simpleCsymbol "arith1" "minus") [] [] exs
exprPass7 x = x

symbolMap = M.fromList symbolTable
symbolTable =
  [(NSInverse, ("fns1", "inverse")), (NSIdent, ("fns1", "identity")),
   (NSDomain, ("fns1", "domain")),   (NSCodomain, ("fns1", "range")),
   (NSImage, ("fns1", "image")),     (NSLn, ("transc1", "ln")),
   (NSQuotient, ("integer1", "quotient")), (NSDivide, ("arith1", "divide")),
   (NSPower, ("arith1", "power")),   (NSRem, ("integer1", "remainder")),
   (NSFactorial, ("integer1", "factorial")), (NSAbs, ("arith1", "abs")),
   (NSConjugate, ("complex1", "conjugate")),
   (NSArg, ("complex1", "argument")),
   (NSReal, ("complex1", "real")), (NSImaginary, ("complex1", "imaginary")),
   (NSFloor, ("rounding1", "floor")), (NSCeiling, ("rounding1", "ceiling")),
   (NSExp, ("transc1", "exp")),      (NSNot, ("logic1", "not")),
   (NSImplies, ("logic1", "implies")),
   (NSEquivalent, ("logic1", "equivalent")), (NSNeq, ("relation1", "neq")),
   (NSApprox, ("relation1", "approx")),
   (NSFactorof, ("integer1", "factorof")),
   (NSDivergence, ("veccalc1", "divergence")),
   (NSGrad, ("veccalc1", "grad")), (NSCurl, ("veccalc1", "curl")),
   (NSLaplacian, ("veccalc1", "laplacian")), (NSIn, ("set1", "in")),
   (NSNotIn, ("set1", "notin")), (NSNotSubset, ("set1", "notsubset")),
   (NSNotPrSubset, ("set1", "notprsubset")), (NSSubset, ("set1", "subset")),
   (NSPrSubset, ("set1", "prsubset")), (NSSin, ("transc1", "sin")),
   (NSCos, ("transc1", "cos")), (NSTan, ("transc1", "tan")),
   (NSSec, ("transc1", "sec")), (NSCsc, ("transc1", "csc")),
   (NSCot, ("transc1", "cot")), (NSSinh, ("transc1", "sinh")),
   (NSCosh, ("transc1", "cosh")), (NSTanh, ("transc1", "tanh")),
   (NSSech, ("transc1", "sech")), (NSCsch, ("transc1", "csch")),
   (NSCoth, ("transc1", "coth")), (NSArcsin, ("transc1", "arcsin")),
   (NSArccos, ("transc1", "arccos")), (NSArctan, ("transc1", "arctan")),
   (NSArccosh, ("transc1", "arccosh")), (NSArccot, ("transc1", "arccot")),
   (NSArccoth, ("transc1", "arccoth")), (NSArccsc, ("transc1", "arccsc")),
   (NSArccsch, ("transc1", "arccsch")), (NSArcsec, ("transc1", "arcsec")),
   (NSArcsech, ("transc1", "arcsech")), (NSArcsinh, ("transc1", "arcsinh")),
   (NSArctanh, ("transc1", "arctanh")),
   (NSDeterminant, ("linalg1", "determinant")),
   (NSTranspose, ("linalg1", "transpose")),
   (NSVectorProduct, ("linalg1", "vectorproduct")),
   (NSScalarProduct, ("linalg1", "scalarproduct")),
   (NSOuterProduct, ("linalg1", "outerproduct")),
   (NSIntegers, ("setname1", "Z")), (NSReals, ("setname1", "R")),
   (NSRationals, ("setname1", "Q")), (NSNaturalNumbers, ("setname1", "N")),
   (NSComplexes, ("setname1", "C")), (NSPrimes, ("setname1", "P")),
   (NSExponentialE, ("nums1", "e")), (NSImaginaryi, ("nums1", "i")),
   (NSNotanumber, ("nums1", "NaN")), (NSTrue, ("logic1", "true")),
   (NSFalse, ("logic1", "false")), (NSPi, ("nums1", "pi")),
   (NSEulergamma, ("nums1", "gamma")), (NSInfinity, ("nums1", "infinity")),
   (NSLog, ("transc1", "log")), (NSMoment, ("s_data1", "moment")),
   (NSCompose, ("fns1", "compose")), (NSRoot, ("arith1", "root")),
   (NSMin, ("minmax1", "min")), (NSMax, ("minmax1", "max")),
   (NSPlus, ("arith1", "plus")), (NSTimes, ("arith1", "times")),
   (NSGcd, ("arith1", "gcd")), (NSLcm, ("arith1", "lcm")),
   (NSAnd, ("logic1", "and")), (NSOr, ("logic1", "or")),
   (NSXor, ("logic1", "xor")), (NSForall, ("quant1", "forall")),
   (NSExists, ("quant1", "exists")), (NSEq, ("relation1", "eq")),
   (NSGt, ("relation1", "gt")), (NSLt, ("relation1", "lt")),
   (NSGeq, ("relation1", "geq")), (NSLeq, ("relation1", "leq")),
   (NSInt, ("calculus1", "int")), (NSDiff, ("calculus1", "diff")),
   (NSPartialdiff, ("calculus1", "partialdiff")), (NSUnion, ("set1", "union")),
   (NSIntersect, ("set1", "intersect")), (NSCartesianProduct, ("set1", "cartesian_product")),
   (NSSetDiff, ("set1", "setdiff")), (NSCard, ("set1", "size")),
   (NSSum, ("arith1", "sum")), (NSProduct, ("arith1", "product")),
   (NSLimit, ("limit1", "limit")), (NSMean, ("s_data1", "mean")),
   (NSSdev, ("s_data1", "sdev")), (NSVariance, ("s_data1", "variance")),
   (NSMedian, ("s_data1", "median")), (NSMode, ("s_data1", "mode")),
   (NSSelector, ("linalg1", "matrix_selector")), (NSEmptySet, ("set1", "emptyset"))
  ]

pass8 :: NSAST -> NSAST
pass8 = exprPass8
exprPass8 x
  | Just (cd, n) <- msym = simpleCsymbol cd n
  | otherwise = x
  where
    msym = M.lookup x symbolMap

pass9 :: WithMaybeSemantics (WithNSCommon NSCi) -> WithMaybeSemantics (WithNSCommon NSCi)
pass9 = exprPass9
exprPass9 (WithMaybeSemantics s (WithNSCommon c (NSCi (Just t) v))) =
  WithMaybeSemantics
    (combineSemantics ( defaultSemantics {
                           semanticsAnnotationXml = [
                              XN.mkElement (XN.mkNsName "annotation-xml" mathmlNS)
                                [XN.mkAttr (XN.mkName "cd") [XN.mkText "mathmltypes"],
                                 XN.mkAttr (XN.mkName "name") [XN.mkText "type"],
                                 XN.mkAttr (XN.mkName "encoding") [XN.mkText "MathML-Content"]]
                                [XN.mkElement (XN.mkNsName "ci" mathmlNS) [] [XN.mkText $ nameType t]]
                              ]}) s) $ WithNSCommon c $ NSCi Nothing v
exprPass9 x = x

nameType :: NSVariableType -> String
nameType (NSStrictVariableType CiInteger) = "integer_type"
nameType (NSStrictVariableType CiReal) = "real_type"
nameType (NSStrictVariableType CiRational) = "rational_type"
nameType (NSStrictVariableType CiComplex) = "complex_cartesian_type"
nameType (NSStrictVariableType CiComplexPolar) = "complex_polar_type"
nameType (NSStrictVariableType CiComplexCartesian) = "complex_cartesian_type"
nameType (NSStrictVariableType CiConstant) = "constant_type"
nameType (NSStrictVariableType CiFunction) = "fn_type"
nameType (NSStrictVariableType CiVector) = "vector_type"
nameType (NSStrictVariableType CiSet) = "set_type"
nameType (NSStrictVariableType CiList) = "list_type"
nameType (NSStrictVariableType CiMatrix) = "matrix_type"
nameType (NSCiOther s) = s

combineSemantics :: Semantics -> Maybe Semantics -> Maybe Semantics
combineSemantics s Nothing = Just s
combineSemantics ((Semantics { semanticsAnnotationXml = l1, semanticsAnnotation = l2 }))
                 (Just s@(Semantics {semanticsAnnotationXml = l1', semanticsAnnotation = l2'})) =
  Just $ s { semanticsAnnotationXml = l1 ++ l1', semanticsAnnotation = l2 ++ l2' }
