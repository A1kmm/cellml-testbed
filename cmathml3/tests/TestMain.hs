import Data.ContentMathML3.Parser
import Data.ContentMathML3.Structure
import Text.XML.HXT.Core
import System.FilePath
import Control.Monad

main = forM ["test1.xml"] $ \fn -> do
     v <- runX $ readDocument [] ("tests" </> fn) /> parseMathML
     print v
