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
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import qualified Foreign
import qualified Foreign.C.Types
import Data.Char
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Data.Map as M

newtype InvalidMathML = InvalidMathML String
instance Error InvalidMathML where
  strMsg m = InvalidMathML m
type PossibleMathMLError a = Either InvalidMathML a
type PME a = PossibleMathMLError a

mathmlNS = "http://www.w3.org/1998/Math/MathML"
mname lp = mkQName "mml" lp mathmlNS

melem lp = isElem >>> hasQName (mname lp)

parseMathML :: ArrowXml a => a XmlTree (PME NSASTC)
parseMathML = melem "math" /> parseMathMLExpression

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
      Foreign.unsafePerformIO $ Foreign.with cl $ \clp ->
        Foreign.peek (Foreign.castPtr clp)
  in
   fromRational . toRational $ cd

parseMathMLExpression :: (Arrow a, ArrowXml a) => a XmlTree (PME NSASTC)
parseMathMLExpression =
  parseMaybeSemantics (parseWithNSCommon parseNSAST)

monadicEA :: ArrowApply a => (b -> (ErrorT InvalidMathML (ArrowAsMonad a) c)) -> a b (PME c)
monadicEA f = monadicA $ \el -> runErrorT $ f el

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
             (alwaysSuccessA $ maybeAttr "type")
             parseNSSymbolContent) <+>
  (melem "cs" >>> liftA NSCs (alwaysSuccessA extractChildText)) <+>
  (melem "apply" >>> liftAM4 NSApply
                       (listA getChildren >>^ head >>> parseMathMLExpression)
                       (listA (melem "bvar" >>> parseBvar))
                       (listToMaybeMax1 "qualifier" $
                          listA (isQualifier >>> parseQualifier))
                       (listA (unlistA (listA getChildren >>^ tail) >>>
                               parseMathMLExpression))) <+>
  (melem "bind" >>> liftAM4 NSBind
                       (listA getChildren >>^ head >>> parseMathMLExpression)
                       (listA (melem "bvar" >>> parseBvar))
                       (listToMaybeMax1 "qualifier" $
                          listA (isQualifier >>> parseQualifier))
                       (listA (unlistA (listA getChildren >>^ tail) >>>
                               parseMathMLExpression))) <+>
  (melem "cerror" >>> liftAM2 NSError
                        (listA getChildren >>^ head >>> parseMathMLExpression)
                        (listA (unlistA (listA getChildren >>^ tail) >>>
                                parseMathMLExpression))) <+>
  
alwaysSuccessA :: (Arrow a, Error e) => a b c -> a b (Either e c)
alwaysSuccessA a = a >>^ Right
extractChildText :: ArrowXml a => a XmlTree String
extractChildText = liftA concat $ listA $ getChildren >>> getText
