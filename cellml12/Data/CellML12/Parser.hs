module Data.CellML12.Parser
where

import Control.Arrow
import Control.Arrow.ApplyUtils
import Text.XML.HXT.Core
import Data.CellML12.Structure
import qualified Data.ContentMathML3.Structure as M
import qualified Data.ContentMathML3.Parser as M
import qualified Data.ContentMathML3.NSToS as M
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

newtype InvalidCellML = InvalidCellML String deriving (Show, Eq, Ord)
type PCE a = Either InvalidCellML a
instance Error InvalidCellML where
  strMsg m = InvalidCellML m

cellmlNS = "http://www.cellml.org/cellml/1.2#"
rdfNS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
cmetaNS = "http://www.cellml.org/metadata/1.0#"
xlinkNS = "http://www.w3.org/1999/xlink"
mathmlNS = "http://www.w3.org/1998/Math/MathML"

cname lp = mkQName "cellml" lp cellmlNS

celem lp = isElem >>> hasLocalPart lp >>> hasNamespaceUri cellmlNS

parseCellML :: ArrowXml a => a XmlTree (PCE (WithCommon Model))
parseCellML = propagateNamespaces >>> celem "model" >>>
              parseWithCommon (liftAM5 Model (M.listAM $ getChildren >>> parseComponent)
                                             (M.listAM $ getChildren >>> parseConnection)
                                             (M.listAM $ getChildren >>> parseEncapsulation)
                                             (M.listAM $ getChildren >>> parseImport)
                                             (M.listAM $ getChildren >>> parseUnits)
                              )

parseWithCommon :: ArrowXml a => a XmlTree (PCE b) -> a XmlTree (PCE (WithCommon b))
parseWithCommon f = liftAM2 WithCommon
                      (liftAM4 Common
                         ((liftA listToMaybe $ listA (getQAttrValue0 $ mkQName "cmeta" "id" cmetaNS)) >>^ return)
                         (listA (getChildren >>> isElem >>> hasQName (mkQName "rdf" "RDF" rdfNS)) >>^ return)
                         ((listA $ getChildren >>> notInNamespace [cellmlNS, mathmlNS, rdfNS, cmetaNS]) >>^ return)
                         (listA (getChildren >>> getAttrl >>> notInNamespace ["", cellmlNS, rdfNS, cmetaNS, xlinkNS]) >>^ return)
                      ) f

notInNamespace :: ArrowXml a => [String] -> a XmlTree XmlTree
notInNamespace notIn = (this &&& getNamespaceUri) >>> arrL (\(el, ns) -> if ns `elem` notIn then [] else [el])

parseComponent :: ArrowXml a => a XmlTree (PCE (WithCommon Component))
parseComponent = celem "component" >>> parseWithCommon (
  liftAM4 Component (M.attrOrFail "Missing name on component" "name")
                    (M.listAM $ getChildren >>> parseVariable)
                    (M.listAM $ getChildren >>> M.parseMathML >>^ (either (fail. M.unInvalidMathML) return) >>^ (>>= either fail return . M.nsToStrict))
                    (M.listAM $ getChildren >>> parseUnits)
                                                      )

parseConnection :: ArrowXml a => a XmlTree (PCE (WithCommon Connection))
parseConnection = celem "connection" >>> parseWithCommon (
  liftAM3 Connection (M.attrOrFail "Missing component_1 on connection" "component_1")
                     (M.attrOrFail "Missing component_2 on connection" "component_2")
                     (M.listAM $ getChildren >>> parseMapVariables)
                                                        )

parseEncapsulation :: ArrowXml a => a XmlTree (PCE (WithCommon Encapsulation))
parseEncapsulation = celem "encapsulation" >>> parseWithCommon (
  liftAM Encapsulation (M.listAM $ getChildren >>> parseComponentRef)
                                                               )

parseComponentRef :: ArrowXml a => a XmlTree (PCE (WithCommon ComponentRef))
parseComponentRef = celem "component_ref" >>> parseWithCommon (
  liftAM2 ComponentRef (M.attrOrFail "Missing component attribute on component_ref" "component")
                       (M.listAM $ getChildren >>> parseComponentRef)
                                                              )

parseImport :: ArrowXml a => a XmlTree (PCE (WithCommon Import))
parseImport = celem "import" >>> parseWithCommon (
  liftAM4 Import (M.attrOrFail "Missing name attribute on import" "name")
                 (liftA (maybe (fail "Missing xlink:href attribute on import") return . listToMaybe)
                        (listA $ getQAttrValue0 (mkQName "xlink" "href" xlinkNS)))
                 (M.listAM $ getChildren >>> parseImportComponent)
                 (M.listAM $ getChildren >>> parseImportUnits)
                                                 )

parseUnits :: ArrowXml a => a XmlTree (PCE (WithCommon Units))
parseUnits = celem "units" >>> parseWithCommon (
  liftAM3 Units (M.attrOrFail "Missing name attribute on units" "name")
                (M.maybeAttr "base_units" >>^ (maybe (return True) (tryParseYesNo "base_units attribute on units")))
                (M.listAM $ getChildren >>> parseUnit)
                                               )
tryParseYesNo :: Monad m => String -> String -> m Bool
tryParseYesNo _ "yes" = return True
tryParseYesNo _ "no" = return False
tryParseYesNo failWhere v = fail $ "Unexpected value \"" ++ v ++ "\" on " ++ failWhere ++ ", expected yes or no"

parseUnit :: ArrowXml a => a XmlTree (PCE (WithCommon Unit))
parseUnit = celem "unit" >>> parseWithCommon (
  liftAM5 Unit (M.attrOrFail "Missing units attribute on unit" "units")
               (M.maybeAttr "prefix" >>^ (maybe (return 0) tryParsePrefix))
               (M.maybeAttr "exponent" >>^ maybe (return 1) (tryParseReal "exponent attribute on unit"))
               (M.maybeAttr "multiplier" >>^ maybe (return 1) (tryParseReal "multiplier attribute on unit"))
               (M.maybeAttr "offset" >>^ maybe (return 0) (tryParseReal "offset attribute on unit"))
                                             )

tryParsePrefix "yotta" = return 24
tryParsePrefix "zetta" = return 21
tryParsePrefix "exa" = return 18
tryParsePrefix "peta" = return 15
tryParsePrefix "tera" = return 12
tryParsePrefix "giga" = return 9
tryParsePrefix "mega" = return 6
tryParsePrefix "kilo" = return 3
tryParsePrefix "hecto" = return 2
tryParsePrefix "deka" = return 1
tryParsePrefix "deci" = return $ -1
tryParsePrefix "centi" = return $ -2
tryParsePrefix "milli" = return $ -3
tryParsePrefix "micro" = return $ -6
tryParsePrefix "nano" = return $ -9
tryParsePrefix "pico" = return $ -12
tryParsePrefix "femto" = return $ -15
tryParsePrefix "atto" = return $ -18
tryParsePrefix "zepto" = return $ -21
tryParsePrefix "yocto" = return $ -24
tryParsePrefix v = tryParseReal "prefix attribute on unit element" v

tryParseReal failWhere v = either (fail $ "Invalid real number \"" ++ v ++ "\" in " ++ failWhere)
                                  return $
                             parse (liftM2 (*) ((char '-' >> (return $ -1)) <|> (char '+' >> return 1) <|> (return 1)) (float haskell)) "" v

parseVariable :: ArrowXml a => a XmlTree (PCE (WithCommon Variable))
parseVariable = celem "variable" >>> parseWithCommon (
  liftAM5 Variable (M.attrOrFail "Missing name on variable" "name")
                   (M.attrOrFail "Missing type on variable" "type")
                   (M.maybeAttr "public_interface" >>^ maybe (return False) (tryParseYesNo "public_interface attribute on units"))
                   (M.maybeAttr "private_interface" >>^ maybe (return False) (tryParseYesNo "private_interface attribute on units"))
                   (M.maybeAttr "units" >>^ return)
                                                     )

parseMapVariables :: ArrowXml a => a XmlTree (PCE (WithCommon (String, String)))
parseMapVariables = celem "map_variables" >>> parseWithCommon (
  liftAM2 (,) (M.attrOrFail "Missing variable_1 on map_variables" "variable_1")
              (M.attrOrFail "Missing variable_2 on map_variables" "variable_2")
                                                             )

parseImportComponent :: ArrowXml a => a XmlTree (PCE (WithCommon ImportComponent))
parseImportComponent = celem "component" >>> parseWithCommon (
  liftAM2 ImportComponent
    (M.attrOrFail "Missing name attribute on import component" "name")
    (M.attrOrFail "Missing component_ref attribute on import component" "component_ref")
                                                            )

parseImportUnits :: ArrowXml a => a XmlTree (PCE (WithCommon ImportUnits))
parseImportUnits = celem "units" >>> parseWithCommon (
  liftAM2 ImportUnits
    (M.attrOrFail "Missing name attribute on import units" "name")
    (M.attrOrFail "Missing units_ref attribute on import units" "units_ref")
                                                    )
