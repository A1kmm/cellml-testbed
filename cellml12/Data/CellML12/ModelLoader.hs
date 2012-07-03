{-# LANGUAGE PatternGuards #-}
module Data.CellML12.ModelLoader
where
import Data.CellML12.Structure
import Data.CellML12.IndexedModel
import Data.IORef
import Network.Curl
import qualified Data.Map as M
import Text.XML.HXT.Arrow.XmlState.URIHandling
import Text.XML.HXT.Core
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.CellML12.Parser
import Control.Monad
import qualified Text.XML.HXT.Parser.XmlParsec as XP
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Control.Monad.Trans.Error
import Text.Parsec
import Data.List

class ModelLoader m where
  loadModel :: String -> String -> m IndexedModel

data LoadModelsState = LoadModelsState { refCurlOptions :: IORef [CurlOption],
                                         refLoadedModels :: IORef (M.Map String IndexedModel) }
initialLoadModelsState =
  liftM2 LoadModelsState (newIORef []) (newIORef M.empty)

data LoadModels a = LoadModels { unLoadModels :: (LoadModelsState -> ErrorT InvalidCellML IO a) }

instance Monad LoadModels where
  x >>= y = LoadModels (\s -> (unLoadModels x) s >>= (\v -> (unLoadModels (y v)) s))
  return x = LoadModels { unLoadModels = const (return x) }

getCurlOpts = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts }) -> liftIO (readIORef rcurlopts)
setCurlOpts o = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts }) -> liftIO (writeIORef rcurlopts o)
modifyCurlOpts f = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts }) -> liftIO (modifyIORef rcurlopts f)
addCurlOpt o = modifyCurlOpts $ \l -> o:l

instance ModelLoader LoadModels where
  loadModel ctx u = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts, refLoadedModels = rloaded }) ->
    case expandURIString u ctx of
      Nothing -> fail $ "Cannot make URL absolute: " ++ u
      Just au -> do
        alreadyLoaded <- liftIO (readIORef rloaded)
        case (M.lookup au alreadyLoaded) of
          Just m -> return m
          Nothing -> do
            curlOpts <- liftIO (readIORef rcurlopts)
            modelText <- liftM snd . liftIO $ withCurlDo (curlGetString_ au curlOpts)
            Control.Monad.when (LBS.null modelText) $ fail ("Could not retrieve CellML model from " ++ au)
            case XP.parseXmlDocument "XML source" (LBS.unpack modelText) of
              [] -> fail $ "Model " ++ au ++ " contains no XML elements"
              l@(r:_) | XN.isError r -> fail (maybe "Unknown error" id $ XN.getErrorMsg r)
                      | Nothing <- melem -> fail "No document element in document"
                      | Just de <- melem -> case runLA parseCellML de of
                        [] -> fail "No CellML document found"
                        (Left (InvalidCellML err)):_ -> fail $ "Error parsing CellML: " ++ err
                        (Right cm):_ -> do
                          let mmodel = indexModel au cm
                          liftIO $ modifyIORef rloaded (M.insert au mmodel)
                          return mmodel
                where melem = find XN.isElem l

instance MonadIO LoadModels where
  liftIO = LoadModels . const . ErrorT . liftM Right

runLoadModels :: LoadModels a -> IO (Either InvalidCellML a)
runLoadModels lm = do
  s <- initialLoadModelsState
  runErrorT $ unLoadModels lm s
