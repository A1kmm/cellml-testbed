module Data.ContentMathML3.Serialiser (serialiseMathML)
where

import Control.Arrow
import Control.Arrow.ApplyUtils
import Text.XML.HXT.Core
import Data.ContentMathML3.Structure
import Data.ContentMathML3.Parser (mathmlNS, mname)
import Text.Printf
import qualified Foreign.C.Types
import qualified Foreign
import qualified System.IO.Unsafe

mnsAttr :: ArrowXml a => a b XmlTree
mnsAttr = sqattr (mkQName "" "xmlns" "http://www.w3.org/2000/xmlns/") mathmlNS

melem :: ArrowXml a => String -> [(String, String)] -> [a n XmlTree] -> a n XmlTree
melem lp attr el = mkqelem (mname lp) (mnsAttr:(map (\(n, v) -> sattr n v) attr)) el

serialiseMathML :: ArrowXml a => a ASTC XmlTree
serialiseMathML = injectArrow $ \ast -> melem "math" [] [serialiseASTC ast]

serialiseASTC :: ArrowXml a => ASTC -> a b XmlTree
serialiseASTC (WithMaybeSemantics m (WithCommon (Common { commonId = commonId, commonXref = commonXref, commonClass = commonClass,
                                                          commonStyle = commonStyle, commonHref = commonHref }) ast)) =
  serialiseAST ast >>> maybeAddAttr "id" commonId >>> maybeAddAttr "xref" commonXref
                   >>> maybeAddAttr "class" commonClass >>> maybeAddAttr "style" commonStyle
                   >>> maybeAddAttr "href" commonHref

serialiseAST (Cn cp) =
  let
    (cptype, cpstring) = constPartInfo cp
  in
   melem "cn" [("type", cptype)] [txt cpstring]

serialiseAST (ASTCi (Ci name)) = melem "ci" [] [txt name]
serialiseAST (Csymbol cd sname) = melem "csymbol" [] [txt sname] >>> maybeAddAttr "cd" cd
serialiseAST (Cs val) = melem "cs" [] [txt val]
serialiseAST (Apply op args) = melem "apply" [] ((serialiseASTC op):(map serialiseASTC args))
serialiseAST (Bind op bvar expr) =
  melem "bind" []
        ((map (\(WithMaybeSemantics sem (WithCommon c ci)) -> melem "bvar" [] [serialiseASTC (WithMaybeSemantics sem (WithCommon c (ASTCi (ci))))])
              bvar) ++
         [serialiseASTC expr])
serialiseAST (Error t args) = melem "error" [] ((serialiseASTC t):(map serialiseASTC args))
serialiseAST (CBytes s) = melem "cbytes" [] [txt s]

constPartInfo (CnInteger i) = ("integer", show i)
constPartInfo (CnReal r) = ("real", show r)
constPartInfo (CnDouble d) = ("double", show d)
constPartInfo (CnHexDouble d) = ("hexdouble", printf "%08X" ((doubleToIntBitPattern d) :: Int))

doubleToIntBitPattern :: (Real a, Integral b) => a -> b
doubleToIntBitPattern d =
  let
    cd :: Foreign.C.Types.CDouble
    cd = (realToFrac d)
    cl :: Foreign.C.Types.CLong
    cl =
      System.IO.Unsafe.unsafePerformIO $ Foreign.with cd $ \cdp ->
        Foreign.peek (Foreign.castPtr cdp)
  in
   fromIntegral cl

injectArrow :: ArrowApply a => (b -> a () c) -> a b c
injectArrow f = arr (\x -> (f x, ())) >>> app

maybeAddAttr :: ArrowXml a => String -> Maybe String -> a XmlTree XmlTree
maybeAddAttr _ Nothing = arr id
maybeAddAttr n (Just v) = addAttr n v
