module Data.CellML12.Structure
where
import qualified Data.ContentMathML3.Structure as MathML
import Text.XML.HXT.DOM.TypeDefs
  
-- | Common represents the attributes and elements that may appear in or on every element
-- | in the CellML namespace.
data Common = Common { commonCmetaId :: Maybe String,        -- ^ The cmeta:id attribute
                       commonRDF :: [XmlTree],               -- ^ Any RDF/XML.
                       commonExtensionElements :: [XmlTree], -- ^ Any extension elements.
                       commonExtensionAttrs :: [XmlTree]     -- ^ Any extension attributes.
                     }
-- | A value of common corresponding to no additional attributes or elements.
defaultCommon = Common Nothing [] [] []

-- | Adds the Common attributes to an existing type.
data WithCommon a = WithCommon Common a

-- | Represents the CellML Model element.
data Model = Model {
    -- | The components defined in the XML document.
    modelComponents :: [WithCommon Component],
    -- | The connections defined in the XML document.
    modelConnections :: [WithCommon Connection],
    -- | The model encapsulation defined in the XML document.
    modelEncapsulation :: [WithCommon Encapsulation],
    -- | The imports defined in the XML document.
    modelImports :: [WithCommon Import],
    -- | The top-level units defined in the XML document.
    modelUnits :: [WithCommon Units]
        }

-- | Represents a CellML component element.
data Component = Component {
    -- The name of the component.
    componentName :: String,
    -- The variables defined in this component.
    componentVariables :: [WithCommon Variable],
    -- The mathematical equations defined in this component.
    componentMaths :: [MathML.ASTC],
    -- The units defined within this component.
    componentUnits :: [WithCommon Units]
  }

-- | Represents a CellML variable element.
data Variable = Variable {
  -- | The name of the variable.
  variableName :: String,
  -- | The type of the variable.
  variableType :: String,
  -- | True if this variable is exposed on the public interface.
  variablePublicInterface :: Bool,
  -- | True if this variable is exposed on the private interface.
  variablePrivateInterface :: Bool,
  -- | The value of the (optional) units attribute.
  variableUnits :: Maybe String
  }

-- | Represents a CellML units element.
data Units = Units {
  -- | The name of the units.
  unitsName :: String,
  -- | If true, this is a base units definition
  unitsBaseUnits :: Bool,
  -- | The units children.
  unitsDefinition :: [WithCommon Unit]
  }

-- | Represents a CellML unit element.
data Unit = Unit {
  -- | The value of the prefix attribute.
  unitPrefix :: Double,
  -- | The value of the exponent attribute.
  unitExponent :: Double,
  -- | The value of the multiplier attribute.
  unitMultiplier :: Double,
  -- | The value of the offset attribute.
  unitOffset :: Double
  }

-- | Represents a CellML connection element.
data Connection = Connection {
  -- | The value of the component_1 attribute.
  connectionComponent1 :: String,
  -- | The value of the component_2 attribute.
  connectionComponent2 :: String,
  -- | The map_variables elements, by first and second variable name.
  connectionMapVariables :: [WithCommon (String, String)]
  }

-- | Represents a CellML encapsulation element.
data Encapsulation = Encapsulation {
  -- | The component_ref children of the encapsulation element.
  encapsulationComponentRefs :: [WithCommon ComponentRef]
  }

-- | Represents a CellML component_ref element.
data ComponentRef = ComponentRef {
  -- | The value of the component attribute.
  componentRefComponent :: String,
  -- | The component_ref children of the element.
  componentRefComponentRefs :: [WithCommon ComponentRef]
  }

-- | Represents a CellML import element.
data Import = Import {
  -- | The value of the name attribute.
  importName :: String,
  -- | The value of the xlink:href attribute.
  importHref :: String,
  -- | The component element children.
  importComponent :: [WithCommon ImportComponent],
  -- | The units element children.
  importUnits :: [WithCommon ImportUnits]
  }

-- | Represents a CellML component element, in an import element
data ImportComponent = ImportComponent {
  -- | The value of the name attribute.
  importComponentName :: String,
  -- | The value of the component_ref attribute.
  importComponentComponentRef :: String
  }

-- | Represents a CellML units element, in an import element
data ImportUnits = ImportUnits {
  -- | The value of the name attribute.
  importUnitsName :: String,
  -- | The value of the component_ref attribute.
  importUnitsUnitsRef :: String
  }
