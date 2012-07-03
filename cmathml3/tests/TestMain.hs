import Data.ContentMathML3.Parser
import Data.ContentMathML3.Serialiser
import Data.ContentMathML3.Structure
import Text.XML.HXT.Core
import System.FilePath
import Control.Monad
import Data.ContentMathML3.NSToS
import Data.Function.Selector

main = forM ["test1.xml"] $ \fn -> do
     v <- runX $ readDocument [] ("tests" </> fn) >>> (propagateNamespaces /> parseMathML)
     case v of
       [Left (InvalidMathML err)] -> putStrLn $ "Parse error: " ++ err
       [Right m] -> 
         case (nsToStrict m) of
           Left err -> putStrLn $ "Error converting to strict form: " ++ err
           Right sm -> do
             liftM head (runX $ root [] [arr (const sm) >>> serialiseMathML >>> uniqueNamespacesFromDeclAndQNames] >>>
                         writeDocumentToString []) >>= putStrLn
             return ()
       _ -> putStrLn "No data parsed"
