module Data.ContentMathML3.Parser
where

import Text.XML.HXT.Core

mathmlNS = "http://www.w3.org/1998/Math/MathML"
mname lp = mkQName "mml" lp mathmlNS

melem lp = isElem >>> hasQName (mname lp)

parseMathML :: ArrowXML a => a XMLTree NSAST
parseMathML = melem "math" /> parseMathMLExpression
parseMathMLExpression :: ArrowXML a => a XMLTree NSAST
parseMathMLExpression =
  getXML