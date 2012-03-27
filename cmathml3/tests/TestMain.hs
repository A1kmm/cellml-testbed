import Data.ContentMathML3.Parser
import Data.ContentMathML3.Structure
import Text.XML.HXT.Core
import System.FilePath
import Control.Monad
import Data.ContentMathML3.NSToS

main = forM ["test1.xml"] $ \fn -> do
     v <- runX $ readDocument [] ("tests" </> fn) /> parseMathML
     case v of
       [Left (InvalidMathML err)] -> putStrLn $ "Parse error: " ++ err
       [Right m] -> print $ nsToStrict m
       _ -> putStrLn "No data parsed"
