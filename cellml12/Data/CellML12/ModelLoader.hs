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

class ModelLoader m where
  loadModel :: String -> String -> m (Either InvalidCellML IndexedModel)
  
data LoadModelsState = LoadModelsState { refCurlOptions :: IORef [CurlOption],
                                         refLoadedModels :: IORef (M.Map String IndexedModel) }
initialLoadModelsState = 
  liftM2 LoadModelsState (newIORef []) (newIORef M.empty)
  
data LoadModels a = LoadModels { unLoadModels :: (LoadModelsState -> IO a) }

instance Monad LoadModels where
  x >>= y = LoadModels (\s -> (unLoadModels x) s >>= (\v -> (unLoadModels (y v)) s))
  return x = LoadModels { unLoadModels = const (return x) }

getCurlOpts = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts }) -> readIORef rcurlopts
setCurlOpts o = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts }) -> writeIORef rcurlopts o
modifyCurlOpts f = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts }) -> modifyIORef rcurlopts f
addCurlOpt o = modifyCurlOpts $ \l -> o:l

instance ModelLoader LoadModels where
  loadModel ctx u = LoadModels $ \(LoadModelsState { refCurlOptions = rcurlopts, refLoadedModels = rloaded }) ->
    case expandURIString u ctx of
      Nothing -> return $ fail $ "Cannot make URL absolute: " ++ u
      Just au -> do
        alreadyLoaded <- readIORef rloaded
        case (M.lookup au alreadyLoaded) of
          Just m -> return . return $ m
          Nothing -> do
            curlOpts <- readIORef rcurlopts
            modelText <- liftM snd $ withCurlDo (curlGetString au curlOpts)
            Control.Monad.when (null modelText) $ fail ("Could not retrieve CellML model from " ++ au)
            let l = runLA (xread >>> parseCellML) modelText
            Control.Monad.when (null l) $ fail ("XML-level failure to parse model from " ++ u)
            let mmodel = liftM (indexModel au) $ head l
            case mmodel of
              Left _ -> return mmodel
              Right m -> do
                modifyIORef rloaded (M.insert au m)
                return mmodel

instance MonadIO LoadModels where
  liftIO = LoadModels . const

runLoadModels :: LoadModels a -> IO a
runLoadModels lm = unLoadModels lm =<< initialLoadModelsState
