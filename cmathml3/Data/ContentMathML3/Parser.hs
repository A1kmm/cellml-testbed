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

parseMathMLExpression :: ArrowXml a => a XmlTree (PME NSASTC)
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

parseNSAST = melem "ci" />
               liftAM NSCi (
               parseWithNSCommon
                 (liftAM2 Ci
                    (parseVariableType =<< (getAttrValue "type"))
                    (liftA return extractChildText)
                 ))
extractChildText = liftA concat $ listA $ getChildren >>> getText
parseVariableType v
  | v == "integer" = return CiInteger
  | v == "real" = return CiReal
  | v == "rational" = return CiRational
  | v == "complex" = return CiComplex
  | v == "complex-polar" = return CiComplexPolar
  | v == "complex-cartesian" = return CiComplexCartesian
  | v == "constant" = return CiConstant
  | v == "function" = return CiFunction
  | v == "vector" = return CiVector
  | v == "set" = return CiSet
  | v == "list" = return CiList
  | v == "matrix" = return CiMatrix
  | otherwise = InvalidMathML "Invalid variable type"