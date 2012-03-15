module Data.ContentMathML3.Parser
where

import Control.Arrow
import Control.Arrow.ApplyUtils
import Text.XML.HXT.Core
import Data.ContentMathML3.Structure
import Data.Maybe
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Error
import Text.Parsec hiding ((<|>))
import Control.Applicative hiding (liftA, liftA2, liftA3)
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Printf
import qualified System.IO.Unsafe
import qualified Foreign
import qualified Foreign.C.Types
import Data.Char
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Data.Map as M

newtype InvalidMathML = InvalidMathML String deriving (Show, Eq, Ord)
instance Error InvalidMathML where
  strMsg m = InvalidMathML m
type PossibleMathMLError a = Either InvalidMathML a
type PME a = PossibleMathMLError a

mathmlNS = "http://www.w3.org/1998/Math/MathML"
mname lp = mkQName "mml" lp mathmlNS

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f l = liftM and $ mapM f l

melem lp = isElem >>> hasQName (mname lp)

melemExcluding :: ArrowXml a => [String] -> a XmlTree XmlTree
melemExcluding lexcl =
  let
    jlexcl = map (Just . mname) lexcl
    in
     (arrL $ \v -> if ((XN.getName v) `elem` jlexcl) then [] else [v])

parseMathML :: ArrowXml a => a XmlTree (PME NSASTC)
parseMathML = propagateNamespaces >>> melem "math" /> parseMathMLExpression

parseInt :: (Monad m, Integral a) => Int -> String -> String -> m a
parseInt base n v =
    either (fail $ n ++ " must be an integer in base " ++ (show base)) return $
      parse (intParser (fromIntegral base)) "" v

parseReal :: Monad m => Int -> String -> String -> m Double
parseReal base n v =
    either (fail $ n ++ " must be an real in base " ++ (show base)) return $
      parse (realParser (fromIntegral base)) "" v

allDigits = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
digitToNumber d | d >= '0' && d <= '9' = ord d - ord '0'
                | d >= 'A' && d <= 'Z' = ord d - ord 'A'
                | d >= 'a' && d <= 'z' = ord d - ord 'a'

intParser :: Integral a => a -> Parsec String () a
intParser base = liftM (0-) (char '-' >> unsignedIntParser base) <|>
                 unsignedIntParser base
unsignedIntParser :: Integral a => a -> Parsec String () a
unsignedIntParser base = let
  validDigits = take (fromIntegral base) allDigits
  in
   unsignedIntParser' validDigits (fromIntegral base) 0
unsignedIntParser' :: Integral a => String -> a -> a -> Parsec String () a
unsignedIntParser' vd b v =
  (do
      c <- liftM (fromIntegral . digitToNumber) (oneOf vd)
      unsignedIntParser' vd b ((v * b) + c)
  ) <|> return v

realParser :: Int -> Parsec String () Double
realParser base = do
  let validDigits = take (fromIntegral base) allDigits
  vi <- liftM (fromInteger . fromIntegral) $ intParser base
  (do
      char '.'
      let 
        invb :: Double
        invb = 1 / (fromInteger . fromIntegral $ base)
      vdec <- afterPointParser validDigits invb
      if vi >= 0 then return $ vi + vdec * invb else return $ vi - vdec * invb
   ) <|> (return vi)

afterPointParser digits invb = do
  c <- liftM (fromIntegral . digitToNumber) $ oneOf digits
  v <- afterPointParser digits invb
  return $ c + v * invb

intBitPatternToDouble :: (Integral a, Fractional b) => a -> b
intBitPatternToDouble bp =
  let
    cl :: Foreign.C.Types.CLong
    cl = (fromIntegral bp)
    cd :: Foreign.C.Types.CDouble
    cd =
      System.IO.Unsafe.unsafePerformIO $ Foreign.with cl $ \clp ->
        Foreign.peek (Foreign.castPtr clp)
  in
   fromRational . toRational $ cd

exactlyOneOrError :: Monad m => String -> [a] -> m a
exactlyOneOrError msg l = case l of
  (v:[]) -> return v
  l ->
    let
      nl = length l
      fullMsg = printf "Expected exactly one %s, but found %d" msg nl
    in
     fail fullMsg

parseMathMLExpression :: (Arrow a, ArrowXml a) => a XmlTree (PME NSASTC)
parseMathMLExpression =
  parseMaybeSemantics (parseWithNSCommon parseNSAST)

monadicEA :: ArrowApply a => (b -> (ErrorT InvalidMathML (ArrowAsMonad a) c)) -> a b (PME c)
monadicEA f = monadicA $ \el -> runErrorT $ f el

monadicEA' :: ArrowApply a => (b -> (ErrorT InvalidMathML (ArrowAsMonad a) c)) -> a (PME b) (PME c)
monadicEA' f = monadicEA $ \v -> ErrorT (return v) >>= f

unmonadicEA :: ArrowApply a => a b (PME c) -> b -> ErrorT InvalidMathML (ArrowAsMonad a) c
unmonadicEA a f = ErrorT (unmonadicA a f)

parseMaybeSemantics :: ArrowXml a => a XmlTree (PME c) -> a XmlTree (PME (MaybeSemantics c))
parseMaybeSemantics f =
  (melem "semantics" >>> (monadicEA $ \el -> do
      common <- unmonadicEA parseCommon el
      xmlAn <- lift $ unmonadicA (listA $ getChildren >>> melem "annotation-xml") el
      an <- lift $ unmonadicA (listA $ getChildren >>> melem "annotation") el
      cd <- lift $ unmonadicA (maybeAttr "cd") el
      n <- lift $ unmonadicA (maybeAttr "name") el
      fv <- unmonadicEA f el
      return $ Semantics { semanticsCommon = common,
                           semanticsCD = cd,
                           semanticsName = n,
                           unSemantics = fv,
                           semanticsAnnotationXml = xmlAn,
                           semanticsAnnotation = an }
                         )) <+> liftAM NoSemantics f

maybeAttr :: ArrowXml a => String -> a XmlTree (Maybe String)
maybeAttr = liftA listToMaybe . listA . getAttrValue

attrOrFail :: ArrowXml a => String -> String -> a XmlTree (PME String)
attrOrFail why attrname =
  liftA (maybe (Left . InvalidMathML $ why) Right . listToMaybe)
        (listA $ getAttrValue attrname)

parseWithNSCommon :: ArrowXml a => a XmlTree (PME c) -> a XmlTree (PME (WithNSCommon c))
parseWithNSCommon f = monadicEA $ \el -> do
  fv <- unmonadicEA f el
  du <- lift $ unmonadicA (maybeAttr "definitionURL") el
  enc <- lift $ unmonadicA (maybeAttr "encoding") el
  c <- unmonadicEA parseCommon el
  let nsc = NSCommon { nsCommon = c, nsCommonDefinitionURL = du,
                       nsCommonEncoding = enc }
  return $ WithNSCommon nsc fv

parseCommon :: ArrowXml a => a XmlTree (PME Common)
parseCommon = monadicA $ \el -> do
  id <- unmonadicA (maybeAttr "id") el
  xref <- unmonadicA (maybeAttr "xref") el
  class' <- unmonadicA (maybeAttr "class") el
  style <- unmonadicA (maybeAttr "style") el
  href <- unmonadicA (maybeAttr "href") el
  return . return $ Common { commonId = id, commonXref = xref, commonClass = class',
                             commonStyle = style, commonHref = href }

parseSepEl :: ArrowXml a => a XmlTree (PME (String, String))
parseSepEl = listA getChildren >>> (monadicEA $ \ell ->
  let (elh, elt) = break (not . XN.isText) ell
  in
   case elt of
     (sep:textlist) -> if all XN.isText textlist &&
                          XN.getName sep == Just (mname "sep")
                         then return $
                                (concatMap (fromMaybe "" . XN.getText) elh,
                                 concatMap (fromMaybe "" . XN.getText) textlist)
                         else fail $ "cn element should contain separated list, \
                                     \but contains non-text-nodes other than a \
                                     \single <sep/>"
     _ -> fail "No <sep/> found in cn element type requiring a <sep/>"
                                   )

parseNSConstantPart :: ArrowXml a => Int -> a XmlTree (PME NSConstantPart)
parseNSConstantPart b = monadicEA $ \el -> do
    t <- liftM (fromMaybe "real") (unmonadicEA (liftA return $ maybeAttr "type") el)
    let 
      parsePackContents :: ArrowXml a => (b -> c) -> (String -> String -> ErrorT InvalidMathML (ArrowAsMonad a) b) -> ErrorT InvalidMathML (ArrowAsMonad a) c
      parsePackContents pck prs =
          (lift (unmonadicA extractChildText el)) >>=
          (liftM pck . prs "cn contents")
    let parseSep2Pack pck prs = do
           (t1, t2) <- unmonadicEA parseSepEl el
           d1 <- prs b "separated cn entry" t1
           d2 <- prs b "separated cn entry" t2
           return $ pck d1 d2
    case () of
      () | t == "integer" -> parsePackContents NSCnInteger (parseInt b)
         | t == "real" -> parsePackContents NSCnReal (parseReal b)
         | t == "double" -> parsePackContents NSCnDouble (parseReal b)
         | t == "hexdouble" -> parsePackContents (NSCnDouble . intBitPatternToDouble)
                                                 (parseInt 16)
         | t == "e-notation" -> parseSep2Pack NSCnENotation parseReal
         | t == "rational" -> parseSep2Pack NSCnRational parseInt
         | t == "complex-cartesian" -> parseSep2Pack NSCnComplexCartesian parseReal
         | t == "complex-polar" -> parseSep2Pack NSCnComplexPolar parseReal
         | t == "constant" -> parsePackContents NSCnConstant (const return)
      _ -> parsePackContents (NSCnOther t) (const return)

ciTypeToConstructor :: String -> Maybe VariableType
ciTypeToConstructor = flip M.lookup
                        (M.fromList
                         [("integer", CiInteger), ("rational", CiRational),
                          ("real", CiReal), ("complex", CiComplex),
                          ("complex-polar", CiComplexPolar),
                          ("complex-cartesian", CiComplexCartesian),
                          ("constant", CiConstant), ("function", CiFunction),
                          ("vector", CiVector), ("list", CiList), ("set", CiSet),
                          ("matrix", CiMatrix)
                          ])

parseNSCiType :: ArrowXml a => a XmlTree (PME (Maybe NSVariableType))
parseNSCiType =
  monadicEA $ \el -> do
    ma <- lift $ unmonadicA (maybeAttr "type") el
    maybe (return Nothing)
          (\a ->
            maybe (return . Just . NSCiOther $ a)
                  (return . Just . NSStrictVariableType) (ciTypeToConstructor a))
          ma

parseNSSymbolContent :: ArrowXml a => a XmlTree (PME NSSymbolContent)
parseNSSymbolContent = monadicEA $ \el -> do
  mgl <- liftM listToMaybe $ lift $
           unmonadicA (listA $ getChildren >>> melem "mglyph") el
  case mgl of
    Just gl -> return $ NSCiMGlyph gl
    Nothing -> do
      me <- liftM listToMaybe $ lift $
               unmonadicA (listA $ getChildren >>> isElem) el
      case me of
        Just e -> return $ NSCiPresentationExpression e
        Nothing -> lift (unmonadicA extractChildText el) >>= return . NSCiText 
    
parseNSAST :: ArrowXml a => a XmlTree (PME NSAST)
parseNSAST = 
  (melem "cn" >>> (monadicEA $ \el -> do
      baseStr <- (lift $ unmonadicA (maybeAttr "base") el)
      base <- (maybe (return Nothing) (liftM Just . (parseInt 10 "base attribute")) baseStr)
      let useBase = fromMaybe 10 base
      cp <- unmonadicEA (parseNSConstantPart useBase) el
      return $ NSCn base cp)) <+>
  (melem "ci" >>> (liftAM NSASTCi $
                   liftAM2 NSCi parseNSCiType parseNSSymbolContent)
  ) <+>
  (melem "csymbol" >>>
     liftAM3 NSCsymbol
             (alwaysSuccessA $ maybeAttr "cd")
             (attrOrFail "Expected type attribute on csymbol" "type")
             parseNSSymbolContent) <+>
  (melem "cs" >>> liftAM NSCs (alwaysSuccessA extractChildText)) <+>
  (melem "apply" >>> (monadicEA $ \app -> do
      hElem <- unmonadicEA (getNthElemA "apply - find operator" 0) app
      h <- unmonadicEA parseMathMLExpression hElem
      tElems <- unmonadicEA (listAM $ getChildren >>> isntBvar >>> isntQualifier >>> parseMathMLExpression) app
      let t = drop 1 tElems
      bv <- unmonadicEA (listAM (getChildren >>> melem "bvar" >>> parseBvar)) app
      qual <- unmonadicEA (listAM (getChildren >>> isQualifier >>> parseQualifier)) app
      return $ NSApply h bv qual t
  )) <+>
  (melem "bind" >>> liftAM4 NSBind
                       ((listA getChildren >>^ head) >>> parseMathMLExpression)
                       (listAM (melem "bvar" >>> parseBvar))
                       (listAM (isQualifier >>> parseQualifier))
                       (listAM ((listA getChildren >>^ tail) >>> unlistA >>>
                                parseMathMLExpression))) <+>
  (melem "cerror" >>> liftAM2 NSError
                        ((listA getChildren >>^ head) >>> parseMathMLExpression)
                        (listAM ((listA getChildren >>^ tail) >>> unlistA >>>
                                parseMathMLExpression))) <+>
  (melem "cbytes" >>> alwaysSuccessA (liftA NSCBytes extractChildText)) <+>
  (melem "piecewise" >>>
     (liftAM NSPiecewise $
       liftAM2 (,)
                    (listAM $ parseWithNSCommon $ melem "piece" >>>
                     liftAM2 (,) (monadicEA $ \el -> do
                                     p <- unmonadicEA (getNthElemA "piece" 0) el
                                     unmonadicEA parseMathMLExpression p)
                          (monadicEA $ \el -> do
                              p <- unmonadicEA (getNthElemA "piece" 1) el
                              unmonadicEA parseMathMLExpression p))
                    (defaultA (Right Nothing) (getChildren >>>
                                               liftAM Just (parseWithNSCommon (melem "otherwise" />
                                                                               parseMathMLExpression)))))) <+>
  (melem "relation" >>>
     liftAM NSRelation (listAM (getChildren >>> parseMathMLExpression))) <+>
  (melem "function" >>>
     liftAM NSFunction (listAM (getChildren >>> parseMathMLExpression) >>>
                        monadicEA' (exactlyOneOrError "expression child of function"))) <+>
  (melem "declare" >>>
   (monadicEA $ \el -> do
       typeA <- lift $ unmonadicA (maybeAttr "type") el
       scopeA <- lift $ unmonadicA (maybeAttr "scope") el
       rawNArgsA <- lift $ unmonadicA (maybeAttr "nargs") el
       nargsA <- maybe (return Nothing) (\v -> parseInt 10 "nargs" v >>= return . Just) rawNArgsA
       rawNOccurA <- lift $ unmonadicA (maybeAttr "noccur") el
       noccurA <- case rawNOccurA of
         Nothing -> return Nothing
         Just "prefix" -> return $ Just NSDeclarePrefix
         Just "infix" -> return $ Just NSDeclareInfix
         Just "function-model" -> return $ Just NSDeclareFunctionModel
         Just v -> fail $ "Invalid noccur attribute value " ++ v
       ndecl <- unmonadicEA (listAM (getChildren >>> parseMathMLExpression)) el
       return $ NSDeclare typeA scopeA nargsA noccurA ndecl)) <+>
  (melem "lambda" >>>
   (monadicEA $ \el -> do
       bv <- unmonadicEA (listAM (getChildren >>> melem "bvar" >>> parseBvar)) el
       children <- unmonadicEA (listAM (getChildren >>> isntBvar >>> isntQualifier >>> parseMathMLExpression)) el
       expr <- exactlyOneOrError "non-qualifier element on lambda" children
       dom <- unmonadicEA (listAM (getChildren >>> parseDomainQualifier)) el
       return $ NSLambda bv dom expr
                          )) <+>
  (melem "vector" >>>
   (monadicEA $ \el -> do   
       bv <- unmonadicEA (listAM (getChildren >>> melem "bvar" >>> parseBvar)) el
       children <- unmonadicEA (listAM (getChildren >>> isntBvar >>> isntQualifier >>> parseMathMLExpression)) el
       dom <- unmonadicEA (listAM (getChildren >>> parseDomainQualifier)) el
       return $ NSVector bv dom children
   )) <+>
  (melem "matrix" >>>
   (monadicEA $ \el -> do
     bv <- unmonadicEA (listAM (getChildren >>> melem "bvar" >>> parseBvar)) el
     dom <- unmonadicEA (listAM (getChildren >>> parseDomainQualifier)) el
     rows <- unmonadicEA (listAM (getChildren >>> melem "matrixrow" >>>
                                  parseWithNSCommon parseMatrixRow)) el
     return $ NSMatrix bv dom rows
   )) <+>
  (melem "tendsto" >>>
   alwaysSuccessA (liftA NSTendsto (maybeAttr "type"))) <+>
  (foldl (<+>) zeroArrow $
     map (\(n,v) -> melem n >>>
                    (alwaysSuccessA . arr . const $ v)) $
    [("inverse", NSInverse), ("ident", NSIdent), ("domain", NSDomain),
     ("codomain", NSCodomain), ("image", NSImage), ("ln", NSLn),
     ("log", NSLog), ("moment", NSMoment), ("compose", NSCompose),
     ("quotient", NSQuotient), ("divide", NSDivide), ("minus", NSMinus),
     ("power", NSPower), ("rem", NSRem), ("root", NSRoot),
     ("factorial", NSFactorial), ("abs", NSAbs), ("conjugate", NSConjugate),
     ("arg", NSArg), ("real", NSReal), ("imaginary", NSImaginary),
     ("floor", NSFloor), ("ceiling", NSCeiling), ("exp", NSExp),
     ("max", NSMax), ("min", NSMin), ("plus", NSPlus), ("times", NSTimes),
     ("gcd", NSGcd), ("lcm", NSLcm), ("and", NSAnd), ("or", NSOr),
     ("xor", NSXor), ("not", NSNot), ("implies", NSImplies),
     ("equivalent", NSEquivalent), ("forall", NSForall),
     ("exists", NSExists), ("eq", NSEq), ("gt", NSGt), ("lt", NSLt),
     ("geq", NSGeq), ("leq", NSLeq), ("neq", NSNeq),
     ("approx", NSApprox), ("factorof", NSFactorof),
     ("int", NSInt), ("diff", NSDiff), ("partialdiff", NSPartialdiff),
     ("divergence", NSDivergence), ("grad", NSGrad), ("curl", NSCurl),
     ("laplacian", NSLaplacian), ("set", NSSet), ("list", NSList),
     ("union", NSUnion), ("intersect", NSIntersect),
     ("cartesianproduct", NSCartesianProduct), ("in", NSIn),
     ("notin", NSNotIn), ("notsubset", NSNotSubset),
     ("notprsubset", NSNotPrSubset), ("setdiff", NSSetDiff),
     ("subset", NSSubset), ("prsubset", NSPrSubset), ("card", NSCard),
     ("sum", NSSum), ("product", NSProduct), ("limit", NSLimit),
     ("sin", NSSin), ("cos", NSCos), ("tan", NSTan), ("sec", NSSec),
     ("csc", NSCsc), ("cot", NSCot), ("sinh", NSSinh), ("cosh", NSCosh),
     ("tanh", NSTanh), ("sech", NSSech), ("csch", NSCsch), ("coth", NSCoth),
     ("arcsin", NSArcsin), ("arccos", NSArccos), ("arctan", NSArctan),
     ("arccosh", NSArccosh), ("arccot", NSArccot), ("arccoth", NSArccoth),
     ("arccsc", NSArccsc), ("arccsch", NSArccsch), ("arcsec", NSArcsec),
     ("arcsech", NSArcsech), ("arcsinh", NSArcsinh), ("arctanh", NSArctanh),
     ("mean", NSMean), ("sdev", NSSdev), ("variance", NSVariance),
     ("median", NSMedian), ("mode", NSMode), ("determinant", NSDeterminant),
     ("transpose", NSTranspose), ("selector", NSSelector),
     ("vectorproduct", NSVectorProduct), ("scalarproduct", NSScalarProduct),
     ("outerproduct", NSOuterProduct), ("integers", NSIntegers),
     ("reals", NSReals), ("rationals", NSRationals),
     ("naturalnumbers", NSNaturalNumbers), ("complexes", NSComplexes),
     ("primes", NSPrimes), ("emptyset", NSEmptySet),
     ("exponentiale", NSExponentialE), ("imaginaryi", NSImaginaryi),
     ("notanumber", NSNotanumber), ("true", NSTrue), ("false", NSFalse),
     ("pi", NSPi), ("eulergamma", NSEulergamma), ("infinity", NSInfinity)
    ])
  
parseMatrixRow :: ArrowXml a => a XmlTree (PME NSMatrixRow)
parseMatrixRow = monadicEA $ \el -> do
  bv <- unmonadicEA (listAM (getChildren >>> melem "bvar" >>> parseBvar)) el
  dom <- unmonadicEA (listAM (getChildren >>> parseDomainQualifier)) el
  children <- unmonadicEA (listAM (getChildren >>> isntBvar >>> isntQualifier >>> parseMathMLExpression)) el
  return $ NSMatrixRow bv dom children

alwaysSuccessA :: (Arrow a, Error e) => a b c -> a b (Either e c)
alwaysSuccessA a = a >>^ Right
extractChildText :: ArrowXml a => a XmlTree String
extractChildText = liftA concat $ listA $ getChildren >>> getText

getNthElemA el n = listA getChildren >>^ (\l -> let v = drop n l in
                                            if null v then Left (InvalidMathML (el ++ ": Expected at least " ++ show (n + 1) ++ " child elements"))
                                                      else Right (head v))

listAM :: (ArrowList a) => a b (PME c) -> a b (PME [c])
listAM a =
  let
    leftOrRightList :: Either e a -> Either e [a] -> Either e [a]
    leftOrRightList _ l@(Left _) = l
    leftOrRightList (Left l) _ = Left l
    leftOrRightList (Right e) (Right l) = Right (e:l)
  in
   listA a >>^ (foldr leftOrRightList (Right []))
listToMaybeMax1 :: (ArrowApply a, ArrowList a, Arrow a) => String -> a b (PME c) -> a b (PME (Maybe c))
listToMaybeMax1 n a = (monadicEA $ \b -> do
                          alist <- unmonadicEA (listAM a) b
                          case alist of
                            [] -> return Nothing
                            v:[] -> return (Just v)
                            _ -> error (n ++ ": At most one matching element is allowed"))

defaultA :: (ArrowList a, ArrowApply a) => c -> a b c -> a b c
defaultA dv a = ((liftA2 (<|>) (listA a >>^ listToMaybe) (constA (Just dv))) >>^ maybeToList) >>> unlistA

isQualifier :: ArrowXml a => a XmlTree XmlTree
isQualifier = melem "domainofapplication" <+> melem "condition" <+> melem "interval" <+> melem "lowlimit" <+>
              melem "uplimit" <+> melem "degree" <+> melem "momentabout" <+> melem "logbase"

isntQualifier :: ArrowXml a => a XmlTree XmlTree
isntQualifier = melemExcluding ["domainofapplication", "condition", "interval", "lowlimit", "uplimit",
                                "degree", "momentabout", "logbase"]

isntBvar :: ArrowXml a => a XmlTree XmlTree
isntBvar = melemExcluding ["bvar"]

parseQualifier :: ArrowXml a => a XmlTree (PME NSQualifier)
parseQualifier = liftAM NSQualDomain parseDomainQualifier <+>
                 (melem "degree" /> liftAM NSQualDegree parseMathMLExpression) <+>
                 (melem "momentabout" /> liftAM NSQualMomentabout parseMathMLExpression) <+>
                 (melem "logbase" /> liftAM NSQualLogbase parseMathMLExpression)

parseDomainQualifier :: ArrowXml a => a XmlTree (PME NSDomainQualifier)
parseDomainQualifier =
  ((melem "domainofapplication" /> liftAM NSDomainOfApplication parseMathMLExpression) <+>
   (melem "condition" /> liftAM NSCondition parseMathMLExpression) <+>
   (melem "interval" >>> liftAM NSQInterval
    (parseWithNSCommon $ monadicEA $ \interval -> do
        closure <- lift $ unmonadicA (maybeAttr "closure") interval
        lowlim <- unmonadicEA (getNthElemA "interval" 0) interval >>= unmonadicEA parseMathMLExpression
        uplim <- unmonadicEA (getNthElemA "interval" 1) interval >>= unmonadicEA parseMathMLExpression
        return $ NSInterval closure lowlim uplim
    )) <+>
   (melem "lowlimit" /> liftAM NSLowlimit parseMathMLExpression) <+>
   (melem "uplimit" /> liftAM NSUplimit parseMathMLExpression)
  )
parseBvar :: ArrowXml a => a XmlTree (PME NSBvar)
parseBvar = melem "bvar" >>>
            liftAM2 NSBvar (parseMaybeSemantics (parseWithNSCommon $ liftAM2 NSCi parseNSCiType parseNSSymbolContent))
                           (liftAM listToMaybe $ listAM (getChildren >>> melem "degree" /> parseMathMLExpression))
