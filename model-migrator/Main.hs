{-# LANGUAGE PatternGuards #-}
import Control.Monad.Error hiding (when)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.Identity
import Text.XML.HXT.Core hiding (trace)
import System.Environment
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Data.Generics.Uniplate.Data
import qualified Text.XML.HXT.Parser.XmlParsec as XP
import XNodeOrd
import Data.List
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Printf

data MigrationSettings a n = MigrationSettings { modelBvar :: a n XmlTree,
                                                 bvarValue :: a n XmlTree }

main =
  getArgs >>= (\args -> case args of
    input:output:bvar:bval:bunits:_ -> do
      inptext <- LBS.readFile input
      ms <- makeMigrationSettings bvar bval bunits
      outtext <- either (\m -> fail ("Error migrating model: " ++ m)) return $ tryMigrateModel inptext ms
      LBS.writeFile output outtext
    _ -> do
      putStrLn "Usage: model-migrator infile outfile bvar initialBvarValue units"
      putStrLn "Note: bvar name must be the same in all components - you may"
      putStrLn "need to manually fix your model afterwards to connect it up and"
      putStrLn "fix the evaluatedAt applications to use the correct name."
             )

someNumber = liftM (either fromIntegral id) (naturalOrFloat haskell)

makeMigrationSettings bvarName bvarValueString bvarUnits = do
  case parse someNumber ""  bvarValueString of
    Left _ -> fail "Cannot parse initial bound variable value"
    Right v -> return
               MigrationSettings { modelBvar = mkqelem (mathName "ci") []
                                               [txt bvarName],
                                   bvarValue = mkqelem (mathName "cn") [sqattr (cellmlName "units") bvarUnits]
                                               [txt $ printf "%f" v] }

tryMigrateModel :: LBS.ByteString -> MigrationSettings LA () -> Either String LBS.ByteString
tryMigrateModel mtext ms = runIdentity . runErrorT $
  case XP.parseXmlDocument "XML source" (LBS.unpack mtext) of
    [] -> fail "Cannot retrieve document"
    l@(r:_) | XN.isError r -> fail (maybe "Unknown error" id $ XN.getErrorMsg r)
            | Nothing <- melem -> fail "No document element in document"
            | Just de <- melem -> do
              r' <- doTransformModel ms . head . runLA propagateNamespaces $ de
              return . LBS.pack . head $ runLA (xshow (returnA >>> uniqueNamespacesFromDeclAndQNames)) r'
      where melem = find XN.isElem l

doTransformModel :: MigrationSettings LA () -> XmlTree -> ErrorT String Identity XmlTree
doTransformModel ms = transformBiM (changeTree ms) . transformBi changeNamespace

changeNamespace qn
  | ns == "http://www.cellml.org/cellml/1.0#" ||
    ns == "http://www.cellml.org/cellml/1.1#" =
      mkNsName ln "http://www.cellml.org/cellml/1.2#"
  | otherwise = qn
  where ns = namespaceUri qn
        ln = qualifiedName qn

changeTree :: Monad m => MigrationSettings LA () -> XmlTree -> m XmlTree
changeTree ms n
  | XN.isElem n = changeElement ms n
  | otherwise = return n

changeElement :: Monad m => MigrationSettings LA () -> XmlTree -> m XmlTree
changeElement ms n
  | ns == "http://www.cellml.org/cellml/1.2#" = changeCellMLElement ms (localPart ename) n
  | otherwise = return n
  where
    Just ename = XN.getElemName n
    ns = namespaceUri ename

cellmlName n = mkNsName n "http://www.cellml.org/cellml/1.2#"
mathName n = mkNsName n "http://www.w3.org/1998/Math/MathML"

hasCellMLName n = hasNamespaceUri "http://www.cellml.org/cellml/1.2#" >>> hasLocalPart n

changeCellMLElement :: Monad m => MigrationSettings LA () -> String -> XmlTree -> m XmlTree
changeCellMLElement _ "connection" n =
  case runLA ((processChildren (neg (hasCellMLName "map_components"))) &&&
               ((getChildren >>> hasCellMLName "map_components") >>>
                (getAttrValue0 "component_1" &&& getAttrValue0 "component_2"))) n
  of
    [] -> fail "Found a connection with missing map_components, component_1 or component_2"
    (n', (comp1, comp2)):_ ->
      return . head $
        runLA (addAttrl $ catA [sattr "component_1" comp1, sattr "component_2" comp2]) n'

changeCellMLElement _ "variable" n =
  return . changeAttributeValue "public_interface" dirToBool .
  changeAttributeValue "private_interface" dirToBool $ n
  
changeCellMLElement ms "component" n =
  -- Make a list of all initial values and their names...
  let ivs =
        runLA (getChildren >>> hasCellMLName "variable" >>>
               (getAttrValue0 "name" &&& (getAttrValue0 "initial_value" &&& getAttrValue0 "units"))) n
  in
   return . head $
     runLA (replaceChildren ((getChildren >>> ((addAttr "type" "real" >>> removeAttr "initial_value") `when`
                                               hasCellMLName"variable")) <+> makeIVMaths ms ivs)) n

changeCellMLElement _ "reaction" n =
  fail "This model has reactions, which are not translated by this tool"

changeCellMLElement _ "group" n =
  return . head .
  runLA
    (setQName (cellmlName "encapsulation") >>>
                replaceChildren ((getChildren >>> hasCellMLName "relationship_ref" >>> hasAttrValue "relationship" (=="encapsulation"))
                                 `guards`
                                 (getChildren /> hasCellMLName "component_ref"))) $ n

changeCellMLElement _ _ n = return n

makeIVMaths ms ivs = catA $ flip map ivs $ \(name, (value, units)) ->
  mkqelem (mathName "math") [] [
    mkqelem (mathName "apply") [] [
       mkqelem (mathName "eq") [] [],
       mkqelem (mathName "apply") [] [
         mkqelem (mathName "csymbol") [sattr "cd" "cellml1"] [txt "evaluatedAt"],
         constA () >>> modelBvar ms, constA () >>> bvarValue ms,
         mkqelem (mathName "ci") [] [txt name]
         ],
       case parse someNumber "" value of
         Left _ -> mkqelem (mathName "ci") [] [txt value]
         Right v -> mkqelem (mathName "cn") [sqattr (cellmlName "units") units] [txt $ printf "%f" v]
       ]
    ]

changeAttributeValue :: String -> (String -> String) -> XmlTree -> XmlTree
changeAttributeValue attrname f =
  XN.changeAttrl (map $ \t -> if XN.getQualifiedName t == Just attrname then
                                XN.mkAttr (mkName attrname) [XN.mkText . f . head $ runLA (xshow getChildren) t] else t)

dirToBool "in" = "yes"
dirToBool "out" = "yes"
dirToBool _ = "no"
