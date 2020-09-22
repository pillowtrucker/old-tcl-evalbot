{-# LANGUAGE OverloadedStrings #-}
module GypsFulvus(execMain) where
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import System.Directory
import qualified Data.Text as T
import Control.Concurrent(ThreadId, forkIO, killThread)
import GypsFulvus.PluginStuff
import Control.Monad(liftM,filterM,forever)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Hashable
import qualified Control.Monad.Parallel as Par
import System.Plugins.Load
import qualified Carrion.Plugin.IO.STDIO as CPISTDIO
import Prelude hiding ((++),putStrLn)
import Data.Text.IO(putStrLn)
data Placeholder = Placeholder
data CommandMap = CommandMap (M.Map T.Text Placeholder)
data CommandWorkspace = CommandWorkspace Placeholder
data Sewer = Sewer {getSewerMap :: M.Map Int Manhole}
a ++ b = T.append a b


sharedDataPath :: IO FilePath
sharedDataPath = getXdgDirectory XdgData "gypsfulvus" >>= makeAbsolute

configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "gypsfulvus" >>= makeAbsolute



assCallbackWithManholeInSewer
  :: Hashable a1 =>
     TMVar (Sewer)
     -> a1 -> Manhole -> STM (TMVar Sewer)
assCallbackWithManholeInSewer sewer callback_name callback_manhole = do
  sewer_old <- takeTMVar sewer
  h_cname <- return $ hash callback_name
  let newSewer =Sewer $ M.insert h_cname callback_manhole (getSewerMap sewer_old)
  putTMVar sewer $ newSewer
  return sewer

lookupManholeInSewer :: TMVar(Sewer) -> T.Text -> STM (Maybe Manhole)
lookupManholeInSewer s p = do
  s_l <- readTMVar s
  return $ M.lookup (hash p) (getSewerMap s_l)
  
dispatchCommands sharedCommandWorkspace sharedTaskQueue = undefined
-- broadcast ouputs from routines to all (interested) parties
broadcastToConsumers consumerBroadcastChannel sharedCommandWorkspace sharedTaskQueue = undefined
-- collect all input from all comms plugins and queue for dispatch
collectInputs collectorChannel availableCommandMap sharedCommandWorkspace sharedTaskQueue = undefined

        
-- load all the routines that the bot can run (e.g. run tcl code, calculator, youtube, etc.)
loadLabourPlugins availableCommandMap = undefined
-- thread to pass any work to be done
corePlugName :: T.Text
corePlugName = "core"

runForever :: TMVar Sewer -> IO ()
runForever s =
  let block = do
                mh <- lookupManholeInSewer s corePlugName
                case mh of
                  Just mh' -> readTChan $ getInputChan mh'
                  Nothing -> retry
  in forever $ do
       someGarbage <- atomically block
       let theAutor = show $ getSewageAutor someGarbage
       putStrLn $ (T.pack theAutor) ++ " sez:"
       putStrLn $ getSewage someGarbage
registerComms = undefined

listDirectory' = listDirectory
  
makeInputManhole :: TMVar(Sewer) -> String -> IO (Maybe Manhole)
makeInputManhole s p = do
  coreManhole <- atomically $ lookupManholeInSewer s corePlugName
  case coreManhole of
    Just cm -> do
      coreInputChan <- return $ getInputChan cm
      pluginInputChan <- atomically $ newTChan
      return $ Just $ Manhole pluginInputChan coreInputChan
    Nothing -> return Nothing

tryRegisterIOPlugin :: TMVar(Sewer) -> IO InitStatus
tryRegisterIOPlugin s = do
  let plugName = "STDIO"
  im <- makeInputManhole s plugName
  case im of
    Just im' -> do
      stdioModuleStatus <- CPISTDIO.initPlugin im'
      case stdioModuleStatus of
        GoodInitStatus -> do
          atomically $ assCallbackWithManholeInSewer s plugName im'
          return GoodInitStatus
        BadInitStatus errs -> return $ BadInitStatus $ "couldn't load stdio plugin: " ++ errs
    Nothing -> return $ BadInitStatus $ T.pack "Catastrophic failure - core ejected."
loadCoreCommands = undefined

makeNewSewer coreManhole = do
  let
    plugName = "core"
  emptySewer <- atomically $ newTMVar $ Sewer M.empty
  atomically $ assCallbackWithManholeInSewer (emptySewer) corePlugName coreManhole
  

execMain :: IO ()
execMain = do
  collectorChannel <- atomically newTChan -- normal channel for dumping any user input, this is the output channel for all plugins
  dumperChannel <- atomically newTChan -- uh this doesnt make any sense, every dings needs to have its own channel
  newSewer <- makeNewSewer $ Manhole collectorChannel dumperChannel
  tryRegisterIOPlugin newSewer
  canary <- atomically $ newTMVar False -- simple 'should I exit' canary
  
--  forkIO $ loadCommsPlugins canary collectorChannel
  
--  availableCommandMap <- atomically $ newTMVar CommandMap
--  loadLabourPlugins availableCommandMap
--  sharedCommandWorkspace <- atomically $ newTMVar CommandWorkspace
--  sharedTaskQueue <- atomically $ newTChan
--  dispatchTID <- forkIO $ dispatchCommands sharedCommandWorkspace sharedTaskQueue
--  broadcastTID <- forkIO $ broadcastToConsumers consumerBroadcastChannel sharedCommandWorkspace sharedTaskQueue
--  collectorTID <- forkIO $ collectInputs collectorChannel availableCommandMap sharedCommandWorkspace sharedTaskQueue
--  myTIDs = [dispatchTID,broadcastTID,collectorTID] 
  let myTIDs  = []
  runForever newSewer
  mapM_ killThread myTIDs




--makePluginsForgetThis canary collectorChannel =
--  let potentialPlugins = srcPluginPath >>= \pp -> listDirectory pp >>= filterM (\fuku -> doesDirectoryExist (pp ++ "/" ++ fuku)) >>= mapM (\fuku -> return (pp ++ "/" ++ fuku))
--  in do
--    srcPluginPath >>= putStrLn
--    srcPluginPath >>= listDirectory >>= mapM putStrLn
--    srcPluginPath >>= \pp -> listDirectory pp >>= filterM (\fuku -> putStrLn (pp ++ "/" ++ fuku) >> doesDirectoryExist (pp ++ "/" ++ fuku)) 
--    pp <- potentialPlugins
--    mapM_ putStrLn pp
--    ff <- mapM (\d -> findFile [d] "Plugin.hs") pp
--    let rff = map (fromMaybe "") $ filter (/= Nothing) ff
--    s <- mapM (\hng -> makeAll hng ["-v","-dynamic"]) rff
--    mapM (\s' -> case s' of
--                   MakeSuccess _ p -> putStrLn p
--                   MakeFailure e -> putStrLn $ show e) s
--    _ <- atomically $ swapTMVar canary True
    -- I don't actually want to quit here but I don't like errors from STM heuristics when the canary is GCed
    
--    return ()
-- end makePluginsForgetThis
