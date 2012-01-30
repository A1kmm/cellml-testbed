{-# LANGUAGE DeriveDataTypeable #-}
module Data.ContentMathML3.Structure (
  Common(Common), commonId, commonXref, commonClass,
  commonStyle, commonHref, commonDefault, ConstantPart(..), VariableType(..), AST(..))
where

import Text.XML.HXT.DOM.TypeDefs
import Data.ContentMathML3.XNodeOrd
import Data.Data  
import Data.Typeable

-- | Represents the attributes common to all MathML elements.
data Common = Common {
  commonId    :: Maybe String, -- ^ An identifier for the element
  commonXref  :: Maybe String, -- ^ The identifier of the parallel presentation MathML markup.
  commonClass :: Maybe String, -- ^ The identifier of the class used in styling.
  commonStyle :: Maybe String, -- ^ The style to be applied.
  commonHref  :: Maybe String  -- ^ The URI to link the element to.
  } deriving (Eq, Ord, Typeable, Data)

commonDefault = Common Nothing Nothing Nothing Nothing Nothing

-- | A constant (cn) value representation.
data ConstantPart = CnInteger Int | CnReal Double | CnDouble Double | CnHexDouble Double deriving (Eq, Ord, Typeable, Data)

-- | A variable (ci) type
data VariableType = CiInteger | CiReal | CiRational | CiComplex | CiComplexPolar |
                    CiComplexCartesian | CiConstant | CiFunction | CiVector |
                    CiSet | CiList | CiMatrix deriving (Eq, Ord, Typeable, Data)
                                     
-- | Strict MathML 3 Abstract Syntax Tree
data ASTC = ASTCommon Common AST deriving (Eq, Ord, Typeable, Data)

-- | Strict Abstract Syntax Tree, without common information on tree root.
data AST = Cn ConstantPart                                          -- ^ Constant
         | Ci VariableType String                                   -- ^ Variable
         | Csymbol { csymbolContentDictionary :: String,
                     csymbolSymbolName :: String }                  -- ^ External symbol
         | Cs Common String                                         -- ^ String literal
         | Apply { applyOperator :: ASTC, applyOperands :: [ASTC] } -- ^ Function application
         | Bind { bindOperator :: ASTC, bindBvar :: [ASTC], 
                  bindExpression :: ASTC }                          -- ^ Binding
         | Error { errorType :: ASTC, errorArgs :: [ASTC] }         -- ^ Error
         | CBytes String                                            -- ^ A string of bytes.
         | DomainOfApplication ASTC                                 -- ^ The domain of application
         | Semantics ASTC XNode                                     -- ^ Annotations
           deriving (Eq, Ord, Data, Typeable)

-- | Represents the attributes common to all non-strict MathML elements.
data NSCommon = NSCommon {
  commonId    :: Maybe String,   -- ^ An identifier for the element
  commonXref  :: Maybe String,   -- ^ The identifier of the parallel presentation MathML markup.
  commonClass :: Maybe String,   -- ^ The identifier of the class used in styling.
  commonStyle :: Maybe String,   -- ^ The style to be applied.
  commonHref  :: Maybe String,   -- ^ The URI to link the element to.
  commonDefinitionURL :: Maybe String, -- ^ The definition URL for the element.
  commonEncoding :: Maybe String -- ^ The encoding of the defintion.
  } deriving (Eq, Ord, Typeable, Data)

nsCommonDefault = NSCommon Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Non-strict MathML 3 Abstract Syntax Tree
data NSASTC = NSASTCommon NSCommon NSAST deriving (Eq, Ord, Typeable, Data)

-- | A constant (cn) value representation.
data NSConstantPart = NSCnInteger Int | NSCnReal Double | NSCnDouble Double | NSCnHexDouble Double |
                      NSCnENotation Double Double | NSCnRational Int Int | NSCnComplexCartesian Double Double |
                      NSCnComplexPolar Double Double | NSCnConstant Double | NSCnText String deriving (Eq, Ord, Typeable, Data)

-- | Non-strict MathML AST, without common information on tree root.
data NSAST = NSCn NSConstantPart | NSCn { nsCnConstant :: NSCommon, nsCnBase :: Maybe Int, nsCnData :: NSConstantPart }
           deriving (Eq, Ord, Typeable, Data)
