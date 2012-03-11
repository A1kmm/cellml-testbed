{-# LANGUAGE DeriveDataTypeable #-}
module Data.ContentMathML3.Structure
where

import Text.XML.HXT.DOM.TypeDefs
import Data.ContentMathML3.XNodeOrd
import Data.Data  
import Data.Typeable

-- | Represents the attributes common to all strict MathML 3 content elements.
data Common = Common {
  commonId    :: Maybe String, -- ^ An identifier for the element
  commonXref  :: Maybe String, -- ^ The identifier of the parallel presentation MathML markup.
  commonClass :: Maybe String, -- ^ The identifier of the class used in styling.
  commonStyle :: Maybe String, -- ^ The style to be applied.
  commonHref  :: Maybe String  -- ^ The URI to link the element to.
  } deriving (Eq, Ord, Typeable, Data)

-- | A starting point for building a Common
commonDefault = Common Nothing Nothing Nothing Nothing Nothing

-- | A constant (cn) value representation.
data ConstantPart = CnInteger Int      -- ^ An integer
                  | CnReal Double      -- ^ A real number
                  | CnDouble Double    -- ^ A double precision real number
                  | CnHexDouble Double -- ^ A double precision real number, in hex
                  deriving (Eq, Ord, Typeable, Data)

-- | A variable (ci) type
data VariableType = CiInteger           -- ^ An integer-valued variable.
                  | CiReal              -- ^ A real-valued variable.
                  | CiRational          -- ^ A rational-valued variable.
                  | CiComplex           -- ^ A complex-valued variable.
                  | CiComplexPolar      -- ^ A complex (polar) valued variable.
                  | CiComplexCartesian  -- ^ A complex (cartesian) valued variable.
                  | CiConstant          -- ^ A constant-valued variable.
                  | CiFunction          -- ^ A function-valued variable.
                  | CiVector            -- ^ A vector-valued variable.
                  | CiSet               -- ^ A set-valued variable.
                  | CiList              -- ^ A list-valued variable.
                  | CiMatrix            -- ^ A matrix-valued variable.
                  deriving (Eq, Ord, Typeable, Data)

data WithCommon a = WithCommon Common a deriving (Eq, Ord, Typeable, Data)
data MaybeSemantics a = Semantics {
  semanticsCommon :: Common,
  semanticsCD :: Maybe String,
  semanticsName :: Maybe String,
  unSemantics :: a, 
  semanticsAnnotationXml :: [XmlTree],
  semanticsAnnotation :: [XmlTree]
  } | NoSemantics a deriving (Eq, Ord, Typeable, Data)

-- | Strict MathML 3 Abstract Syntax Tree
type ASTC = MaybeSemantics (WithCommon AST)

data Ci = Ci (Maybe VariableType) String deriving (Eq, Ord, Typeable, Data)
type CCi = WithCommon Ci

-- | Strict Abstract Syntax Tree, without common information on tree root
data AST = Cn ConstantPart                                  -- ^ Constant
         | ASTCi (CCi)                                      -- ^ Variable
         | Csymbol { csymbolContentDictionary :: String,    -- ^ The OpenMath cd
                     csymbolSymbolName :: String            -- ^ The name in the cd
                   }                                        -- ^ External symbol
         | Cs String                                        -- ^ String literal
         | Apply { applyOperator :: ASTC,                   -- ^ The operator to apply
                   applyOperands :: [ASTC]                  -- ^ The operands to use
                 }                                          -- ^ Function application
         | Bind { bindOperator :: ASTC,                     -- ^ The binding operator
                  bindBvar :: [MaybeSemantics (CCi)],       -- ^ The bound variables
                  bindExpression :: ASTC                    -- ^ The expression
                }                                           -- ^ Binding
         | Error { errorType :: ASTC, errorArgs :: [ASTC] } -- ^ A math error
         | CBytes String                                    -- ^ A string of bytes
           deriving (Eq, Ord, Data, Typeable)

-- | Represents the attributes common to all non-strict MathML elements.
data NSCommon = NSCommon {
  nsCommon :: Common,                    -- ^ Strict common attributes
  nsCommonDefinitionURL :: Maybe String, -- ^ The definition URL for the element.
  nsCommonEncoding :: Maybe String       -- ^ The encoding of the defintion.
  } deriving (Eq, Ord, Typeable, Data)

-- | Default values for non-strict common attributes (all absent)
nsCommonDefault = NSCommon commonDefault Nothing Nothing

-- | Represents some data structure alongside the non-strict common data.
data WithNSCommon a = WithNSCommon NSCommon a deriving (Eq, Ord, Typeable, Data)

-- | Non-strict MathML 3 Abstract Syntax Tree
type NSASTC = MaybeSemantics (WithNSCommon NSAST)

-- | A non-strict constant (cn) value representation.
data NSConstantPart = NSCnInteger Int                    -- ^ An integer constant
                    | NSCnReal Double                    -- ^ A real constant
                    | NSCnDouble Double                  -- ^ A double precision real
                    | NSCnHexDouble Double               -- ^ A real constant in hex
                    | NSCnENotation Double Double        -- ^ A constant in e-notation
                    | NSCnRational Int Int               -- ^ A rational constant
                       -- | A complex cartesian constant
                    | NSCnComplexCartesian Double Double
                       -- | A complex polar constant
                    | NSCnComplexPolar Double Double
                    | NSCnConstant String                -- ^ A predefined constant
                    | NSCnOther String String            -- ^ Another type of constant
                    deriving (Eq, Ord, Typeable, Data)

-- | The type of a variable
data NSVariableType = NSStrictVariableType VariableType -- ^ A strict variable type
                    | NSCiOther String                    -- ^ A user-defined type
                    deriving (Eq, Ord, Typeable, Data)

-- | The content of a non-strict ci or csymbol element.
data NSSymbolContent = NSCiText String                  -- ^ A named element
                     | NSCiMGlyph XmlTree                 -- ^ An mglyph node
                     | NSCiPresentationExpression XmlTree -- ^ Presentation MathML
                     deriving (Eq, Ord, Typeable, Data)

-- | A non-strict ci
data NSCi = NSCi (Maybe NSVariableType) NSSymbolContent deriving (Eq, Ord, Typeable, Data)

-- | A bound variable
data NSBvar = NSBvar { bvarCi :: MaybeSemantics (WithNSCommon NSCi), -- ^ The inner ci
                       bvarDegree :: Maybe NSASTC           -- ^ The degree (e.g. for diff)
                     } deriving (Eq, Ord, Typeable, Data)

-- | Non-strict MathML AST, without common information on tree root.
data NSAST = NSCn { nsCnBase :: Maybe Int,     -- ^ The base used to represent it
                    nsCnData :: NSConstantPart -- ^ The constant data itself
                  }
             | NSASTCi NSCi -- ^ A ci element
               -- | A csymbol element
             | NSCsymbol { nsCsymbolContentDictionary :: Maybe String,
                           nsCsymbolSymbolType :: String, 
                           nsCsymbolContent :: NSSymbolContent }
             | NSCs String -- ^ A string constant (cs)
             | NSApply { nsApplyOperator :: NSASTC,
                         nsApplyBvar :: [NSBvar],
                         nsApplyQualifier :: [NSQualifier],
                         nsApplyOperands :: [NSASTC] }   -- ^ Function application
             | NSBind { nsApplyOperator :: NSASTC,
                        nsApplyBvar :: [NSBvar],
                        nsApplyQualifiers :: [NSQualifier],
                        nsApplyOperands :: [NSASTC] }    -- ^ Function binding
             | NSError { nsErrorType :: NSASTC,
                         nsErrorArgs :: [NSASTC] }       -- ^ Error
             | NSCBytes String                           -- ^ A string of bytes.
               -- | A piecewise expression
             | NSPiecewise ([WithNSCommon (NSASTC, NSASTC)], Maybe (WithNSCommon NSASTC))
               -- | A (deprecated) relation
             | NSRelation [NSASTC]
               -- | A (deprecated) function
             | NSFunction NSASTC
               -- | A (deprecated) declare
             | NSDeclare { nsDeclareType :: Maybe String,
                           nsScope :: Maybe String,
                           nsNArgs :: Maybe Int,
                           nsOccurrence :: Maybe NSDeclareOccurrence, 
                           nsDeclareExprs :: [NSASTC] }
             | NSInverse                               -- ^ Inverse
             | NSIdent                                 -- ^ Identify function
             | NSDomain                                -- ^ Domain
             | NSCodomain                              -- ^ Codomain
             | NSImage                                 -- ^ Image
             | NSLn                                    -- ^ Natural log
             | NSLog                                   -- ^ Log
             | NSMoment                                -- ^ Moment
             | NSLambda { nsLambdaBVar :: [NSBvar],
                          nsLambdaDomain :: [NSDomainQualifier], 
                          nsLambdaExpr :: NSASTC }     -- ^ Lambda function
             | NSCompose                               -- ^ Compose
             | NSQuotient                              -- ^ Quotient
             | NSDivide                                -- ^ Divide
             | NSMinus                                 -- ^ Minus
             | NSPower                                 -- ^ Power
             | NSRem                                   -- ^ Remainder
             | NSRoot                                  -- ^ Root
             | NSFactorial                             -- ^ Factorial
             | NSAbs                                   -- ^ Absolute
             | NSConjugate                             -- ^ Conjugate
             | NSArg                                   -- ^ Argument
             | NSReal                                  -- ^ Real
             | NSImaginary                             -- ^ Imaginary
             | NSFloor                                 -- ^ Floor
             | NSCeiling                               -- ^ Ceiling
             | NSExp                                   -- ^ Exponential
             | NSMax                                   -- ^ Maximum
             | NSMin                                   -- ^ Minimum
             | NSPlus                                  -- ^ Plus
             | NSTimes                                 -- ^ Times
             | NSGcd                                   -- ^ Greatest common denominator
             | NSLcm                                   -- ^ Lowest common multiple
             | NSAnd                                   -- ^ And
             | NSOr                                    -- ^ Or
             | NSXor                                   -- ^ Exclusive or
             | NSNot                                   -- ^ Not
             | NSImplies                               -- ^ Implies
             | NSEquivalent                            -- ^ Equivalent
             | NSForall                                -- ^ Forall
             | NSExists                                -- ^ Exists
             | NSEq                                    -- ^ Equal
             | NSGt                                    -- ^ Greater Than
             | NSLt                                    -- ^ Less Than
             | NSGeq                                   -- ^ Greater Than or Equal To
             | NSLeq                                   -- ^ Less Than Or Equal To
             | NSNeq                                   -- ^ Not Equal
             | NSApprox                                -- ^ Approximately Equal
             | NSFactorof                              -- ^ Factor of
             | NSTendsto (Maybe String)                -- ^ Tends to
             | NSInt                                   -- ^ Integral
             | NSDiff                                  -- ^ Differential
             | NSPartialdiff                           -- ^ Partial Differential
             | NSDivergence                            -- ^ Divergence
             | NSGrad                                  -- ^ Gradient
             | NSCurl                                  -- ^ Curl
             | NSLaplacian                             -- ^ Laplacian
             | NSSet                                   -- ^ Set
             | NSList                                  -- ^ List
             | NSUnion                                 -- ^ Union
             | NSIntersect                             -- ^ Intersection
             | NSCartesianProduct                      -- ^ Cartesian product
             | NSIn                                    -- ^ Set membership
             | NSNotIn                                 -- ^ Non set membership
             | NSNotSubset                             -- ^ Set not subset
             | NSNotPrSubset                           -- ^ Set not proper subset
             | NSSetDiff                               -- ^ Set difference
             | NSSubset                                -- ^ Subset
             | NSPrSubset                              -- ^ Proper subset
             | NSCard                                  -- ^ Cardinality
             | NSSum                                   -- ^ Sum
             | NSProduct                               -- ^ Product
             | NSLimit                                 -- ^ Limit
             | NSSin                                   -- ^ Sine
             | NSCos                                   -- ^ Cosine
             | NSTan                                   -- ^ Tangent
             | NSSec                                   -- ^ Secant
             | NSCsc                                   -- ^ Cosecant
             | NSCot                                   -- ^ Cotangent
             | NSSinh                                  -- ^ Hyperbolic sine
             | NSCosh                                  -- ^ Hyperbolic cosine
             | NSTanh                                  -- ^ Hyperbolic tangent
             | NSSech                                  -- ^ Hyperbolic secant
             | NSCsch                                  -- ^ Hyperbolic cosecant
             | NSCoth                                  -- ^ Hyperbolic cotangent
             | NSArcsin                                -- ^ Inverse sine
             | NSArccos                                -- ^ Inverse cosine
             | NSArctan                                -- ^ Inverse tangent
             | NSArccosh                               -- ^ Inverse hyperbolic cosine
             | NSArccot                                -- ^ Inverse cotangent
             | NSArccoth                               -- ^ Inverse hyperbolic cotangent
             | NSArccsc                                -- ^ Inverse cosecant
             | NSArccsch                               -- ^ Inverse hyperbolic cosecant
             | NSArcsec                                -- ^ Inverse secant
             | NSArcsech                               -- ^ Inverse hyperbolic secant
             | NSArcsinh                               -- ^ Inverse hyperbolic sine
             | NSArctanh                               -- ^ Inverse hyperbolic tan
             | NSMean                                  -- ^ Mean
             | NSSdev                                  -- ^ Standard deviation
             | NSVariance                              -- ^ Variance
             | NSMedian                                -- ^ Median
             | NSMode                                  -- ^ Mode
             | NSVector { nsVectorBvar :: [NSBvar],
                          nsVectorDomain :: [NSDomainQualifier], 
                          nsVectorExpressions :: [NSASTC] } -- ^ Vector constructor
             | NSMatrix { nsMatrixBvar :: [NSBvar],
                          nsMatrixDomain :: [NSDomainQualifier],
                          nsMatrixRows :: [WithNSCommon NSMatrixRow] } -- ^ Matrix
             | NSDeterminant                           -- ^ Determinant
             | NSTranspose                             -- ^ Transpose
             | NSSelector                              -- ^ Selector
             | NSVectorProduct                         -- ^ Vector product
             | NSScalarProduct                         -- ^ Scalar product
             | NSOuterProduct                          -- ^ Outer product
             | NSIntegers                              -- ^ Integers
             | NSReals                                 -- ^ Reals
             | NSRationals                             -- ^ Rationals
             | NSNaturalNumbers                        -- ^ Natural numbers
             | NSComplexes                             -- ^ Complex numbers
             | NSPrimes                                -- ^ Primes
             | NSEmptySet                              -- ^ Empty set
             | NSExponentialE                          -- ^ Exponential e
             | NSImaginaryi                            -- ^ Imaginary i
             | NSNotanumber                            -- ^ notanumber
             | NSTrue                                  -- ^ true
             | NSFalse                                 -- ^ false
             | NSPi                                    -- ^ pi
             | NSEulergamma                            -- ^ Euler gamma
             | NSInfinity                              -- ^ +Infinity
             deriving (Eq, Ord, Typeable, Data)

data NSMatrixRow = NSMatrixRow { nsMatrixRowBvar :: [NSBvar],
                                 nsMatrixRowDomain :: [NSDomainQualifier],
                                 msMatrixRowExpressions :: [NSASTC] }
                     deriving (Eq, Ord, Typeable, Data)

data NSDeclareOccurrence = NSDeclarePrefix | NSDeclareInfix | NSDeclareFunctionModel
                            deriving (Eq, Ord, Typeable, Data)

data NSInterval = NSInterval { nsIntervalClosure :: Maybe String,
                               nsIntervalLow :: NSASTC, 
                               nsIntervalHigh :: NSASTC }
                    deriving (Eq, Ord, Typeable, Data)

data NSDomainQualifier = NSDomainOfApplication NSASTC | NSCondition NSASTC |
                         NSQInterval (WithNSCommon NSInterval) |
                         NSLowlimit NSASTC | 
                         NSUplimit NSASTC deriving (Eq, Ord, Typeable, Data)
data NSQualifier = NSQualDomain NSDomainQualifier |
                   NSQualDegree NSASTC |
                   NSQualMomentabout NSASTC |
                   NSQualLogbase NSASTC
                     deriving (Eq, Ord, Typeable, Data)
