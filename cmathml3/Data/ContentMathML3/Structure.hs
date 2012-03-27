{-# LANGUAGE DeriveDataTypeable #-}
module Data.ContentMathML3.Structure
where

import Text.XML.HXT.DOM.TypeDefs
import Data.ContentMathML3.XNodeOrd
import Data.Data  
import Data.Typeable
import Data.Maybe

-- | Represents the attributes common to all strict MathML 3 content elements.
data Common = Common {
  commonId    :: Maybe String, -- ^ An identifier for the element
  commonXref  :: Maybe String, -- ^ The identifier of the parallel presentation MathML markup.
  commonClass :: Maybe String, -- ^ The identifier of the class used in styling.
  commonStyle :: Maybe String, -- ^ The style to be applied.
  commonHref  :: Maybe String  -- ^ The URI to link the element to.
  } deriving (Eq, Ord, Typeable, Data, Show)

-- | A starting point for building a Common
commonDefault = Common Nothing Nothing Nothing Nothing Nothing

-- | A constant (cn) value representation.
data ConstantPart = CnInteger Int      -- ^ An integer
                  | CnReal Double      -- ^ A real number
                  | CnDouble Double    -- ^ A double precision real number
                  | CnHexDouble Double -- ^ A double precision real number, in hex
                  deriving (Eq, Ord, Typeable, Data, Show)

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
                  deriving (Eq, Ord, Typeable, Data, Show)

data WithCommon a = WithCommon Common a deriving (Eq, Ord, Typeable, Data, Show)
data Semantics = Semantics {
  semanticsCommon :: Common,
  semanticsCD :: Maybe String,
  semanticsName :: Maybe String,
  semanticsAnnotationXml :: [XmlTree],
  semanticsAnnotation :: [XmlTree]
  } deriving (Eq, Ord, Typeable, Data, Show)
data WithMaybeSemantics a = WithMaybeSemantics (Maybe Semantics) a deriving (Eq, Ord, Typeable, Data, Show)
defaultSemantics = Semantics commonDefault Nothing Nothing [] []
withDefaultSemantics a = WithMaybeSemantics (Just defaultSemantics) a

noSemCom :: a -> WithMaybeSemantics (WithCommon a)
noSemCom a = WithMaybeSemantics Nothing (WithCommon commonDefault a)

noNSSemCom :: a -> WithMaybeSemantics (WithNSCommon a)
noNSSemCom a = WithMaybeSemantics Nothing (WithNSCommon nsCommonDefault a)

-- | Strict MathML 3 Abstract Syntax Tree
type ASTC = WithMaybeSemantics (WithCommon AST)

data Ci = Ci String deriving (Eq, Ord, Typeable, Data, Show)
type CCi = WithCommon Ci

-- | Strict Abstract Syntax Tree, without common information on tree root
data AST = Cn ConstantPart                                  -- ^ Constant
         | ASTCi Ci                                         -- ^ Variable
         | Csymbol { csymbolContentDictionary :: Maybe String,  -- ^ The OpenMath cd
                     csymbolSymbolName :: String            -- ^ The name in the cd
                   }                                        -- ^ External symbol
         | Cs String                                        -- ^ String literal
         | Apply { applyOperator :: ASTC,                   -- ^ The operator to apply
                   applyOperands :: [ASTC]                  -- ^ The operands to use
                 }                                          -- ^ Function application
         | Bind { bindOperator :: ASTC,                     -- ^ The binding operator
                  bindBvar :: [WithMaybeSemantics (CCi)],   -- ^ The bound variables
                  bindExpression :: ASTC                    -- ^ The expression
                }                                           -- ^ Binding
         | Error { errorType :: ASTC, errorArgs :: [ASTC] } -- ^ A math error
         | CBytes String                                    -- ^ A string of bytes
           deriving (Eq, Ord, Data, Typeable, Show)

-- | Represents the attributes common to all non-strict MathML elements.
data NSCommon = NSCommon {
  nsCommon :: Common,                    -- ^ Strict common attributes
  nsCommonDefinitionURL :: Maybe String, -- ^ The definition URL for the element.
  nsCommonEncoding :: Maybe String       -- ^ The encoding of the defintion.
  } deriving (Eq, Ord, Typeable, Data, Show)

-- | Default values for non-strict common attributes (all absent)
nsCommonDefault = NSCommon commonDefault Nothing Nothing

-- | Represents some data structure alongside the non-strict common data.
data WithNSCommon a = WithNSCommon NSCommon a deriving (Eq, Ord, Typeable, Data, Show)

-- | Non-strict MathML 3 Abstract Syntax Tree
type NSASTC = WithMaybeSemantics (WithNSCommon NSAST)

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
                    deriving (Eq, Ord, Typeable, Data, Show)

-- | The type of a variable
data NSVariableType = NSStrictVariableType VariableType -- ^ A strict variable type
                    | NSCiOther String                    -- ^ A user-defined type
                    deriving (Eq, Ord, Typeable, Data, Show)

-- | The content of a non-strict ci or csymbol element.
data NSSymbolContent = NSCiText String                  -- ^ A named element
                     | NSCiMGlyph XmlTree                 -- ^ An mglyph node
                     | NSCiPresentationExpression XmlTree -- ^ Presentation MathML
                     deriving (Eq, Ord, Typeable, Data, Show)

-- | A non-strict ci
data NSCi = NSCi (Maybe NSVariableType) NSSymbolContent deriving (Eq, Ord, Typeable, Data, Show)

-- | A bound variable
data NSBvar = NSBvar { bvarCi :: WithMaybeSemantics (WithNSCommon NSCi), -- ^ The inner ci
                       bvarDegree :: Maybe NSASTC           -- ^ The degree (e.g. for diff)
                     } deriving (Eq, Ord, Typeable, Data, Show)

-- | Non-strict MathML AST, without common information on tree root.
data NSAST = NSCn { nsCnBase :: Maybe Int,     -- ^ The base used to represent it
                    nsCnData :: NSConstantPart -- ^ The constant data itself
                  }
             | NSASTCi NSCi -- ^ A ci element
               -- | A csymbol element
             | NSCsymbol { nsCsymbolContentDictionary :: Maybe String,
                           nsCsymbolSymbolType :: Maybe String, 
                           nsCsymbolContent :: NSSymbolContent }
             | NSCs String -- ^ A string constant (cs)
             | NSApply { nsApplyOperator :: NSASTC,
                         nsApplyBvar :: [NSBvar],
                         nsApplyQualifier :: [NSQualifier],
                         nsApplyOperands :: [NSASTC] }   -- ^ Function application
             | NSBind { nsBindOperator :: NSASTC,
                        nsBindBvar :: [NSBvar],
                        nsBindQualifiers :: [NSQualifier],
                        nsBindOperands :: [NSASTC] }    -- ^ Function binding
             | NSError { nsErrorType :: NSASTC,
                         nsErrorArgs :: [NSASTC] }       -- ^ Error
             | NSCBytes String                           -- ^ A string of bytes.
               -- | A piecewise expression
             | NSPiecewise ([WithNSCommon (NSASTC, NSASTC)], Maybe (WithNSCommon NSASTC))
               -- | A (deprecated) relation
             | NSRelation NSASTC [NSASTC]
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
             | NSSet [NSBvar] [NSDomainQualifier] [NSASTC] -- ^ Set
             | NSList [NSBvar] [NSDomainQualifier] [NSASTC] -- ^ List
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
             | NSMatrixByRow [WithNSCommon NSMatrixRow]
             | NSMatrixByFunction { nsMatrixBvar :: [NSBvar],
                                    nsMatrixDomain :: [NSDomainQualifier],
                                    nsMatrixExpr :: NSASTC } -- ^ Matrix
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
             deriving (Eq, Ord, Typeable, Data, Show)

data NSMatrixRow = NSMatrixRow { nsMatrixRowBvar :: [NSBvar],
                                 nsMatrixRowDomain :: [NSDomainQualifier],
                                 msMatrixRowExpressions :: [NSASTC] }
                     deriving (Eq, Ord, Typeable, Data, Show)

data NSDeclareOccurrence = NSDeclarePrefix | NSDeclareInfix | NSDeclareFunctionModel
                            deriving (Eq, Ord, Typeable, Data, Show)

data NSInterval = NSInterval { nsIntervalClosure :: Maybe String,
                               nsIntervalLow :: NSASTC, 
                               nsIntervalHigh :: NSASTC }
                    deriving (Eq, Ord, Typeable, Data, Show)

data NSDomainQualifier = NSDomainOfApplication NSASTC | NSCondition NSASTC |
                         NSQInterval (WithNSCommon NSInterval) |
                         NSLowlimit NSASTC | 
                         NSUplimit NSASTC deriving (Eq, Ord, Typeable, Data, Show)
data NSQualifier = NSQualDomain NSDomainQualifier |
                   NSQualDegree NSASTC |
                   NSQualMomentabout NSASTC |
                   NSQualLogbase NSASTC
                     deriving (Eq, Ord, Typeable, Data, Show)

class CanHaveDomain a where
  getDomains :: a -> [NSDomainQualifier]
  setDomains :: a -> [NSDomainQualifier] -> a
  getBvars :: a -> [NSBvar]
  setBvars :: a -> [NSBvar] -> a

instance CanHaveDomain NSAST where
  getDomains (NSLambda _ dq _) = dq
  getDomains (NSSet _ dq _) = dq
  getDomains (NSList _ dq _) = dq
  getDomains (NSVector _ dq _) = dq
  getDomains (NSMatrixByFunction _ dq _) = dq
  getDomains (NSApply _ _ q _) = mapMaybe (\qv ->
                                            case qv of
                                              NSQualDomain d -> Just d
                                              _ -> Nothing) q
  getDomains (NSBind _ _ q _) = mapMaybe (\qv ->
                                            case qv of
                                              NSQualDomain d -> Just d
                                              _ -> Nothing) q
  getDomains _ = []

  setDomains (NSLambda a _ c) dq = NSLambda a dq c
  setDomains (NSSet a _ c) dq = NSSet a dq c
  setDomains (NSList a _ c) dq = NSList a dq c
  setDomains (NSVector a _ c) dq = NSVector a dq c
  setDomains (NSMatrixByFunction a _ c) dq = NSMatrixByFunction a dq c
  setDomains (NSApply a b c d) dq = NSApply a b (map NSQualDomain dq ++
                                                 filter (\qv ->
                                                          case qv of
                                                            NSQualDomain d -> False
                                                            _ -> True
                                                        ) c) d
  setDomains (NSBind a b c d) dq = NSBind a b (map NSQualDomain dq ++
                                               filter (\qv ->
                                                        case qv of
                                                          NSQualDomain d -> False
                                                          _ -> True
                                                      ) c) d
  setDomains x _ = x

  getBvars (NSLambda bv _ _) = bv
  getBvars (NSSet bv _ _) = bv
  getBvars (NSList bv _ _) = bv
  getBvars (NSVector bv _ _) = bv
  getBvars (NSMatrixByFunction bv _ _) = bv
  getBvars (NSApply _ bv _ _) = bv
  getBvars (NSBind _ bv _ _) = bv
  getBvars _ = []
  
  setBvars (NSLambda _ b c) bv = NSLambda bv b c
  setBvars (NSSet _ b c) bv = NSSet bv b c
  setBvars (NSList _ b c) bv = NSSet bv b c
  setBvars (NSVector _ b c) bv = NSVector bv b c
  setBvars (NSMatrixByFunction _ b c) bv = NSMatrixByFunction bv b c
  setBvars (NSApply a _ c d) bv = NSApply a bv c d
  setBvars (NSBind a _ c d) bv = NSBind a bv c d
  setBvars x _ = x

instance CanHaveDomain NSMatrixRow where
  getDomains (NSMatrixRow _ dq _) = dq
  setDomains (NSMatrixRow a _ c) dq = NSMatrixRow a dq c
  
  getBvars (NSMatrixRow bv _ _) = bv
  setBvars (NSMatrixRow _ b c) bv = NSMatrixRow bv b c
