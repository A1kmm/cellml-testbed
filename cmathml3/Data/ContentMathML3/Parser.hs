module Data.ContentMathML3.Parser
where

import Text.XML.HXT.Core
import Control.Arrow.SLA

parseMathML :: String -> AST
parseMathML = xread