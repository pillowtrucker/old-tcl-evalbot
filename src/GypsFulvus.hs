module GypsFulvus(execMain, Manhole(..), Sewage(..), InitStatus(..), SewageAutorInfo(..), IrcMask(..), genericAutorToNSAutor, nsAutorToGenericAutor, regift,stripCommandPrefix',inspectManhole) where
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import System.Directory
import qualified Data.Text as T
import Control.Concurrent(ThreadId, forkIO, killThread)
import GypsFulvus.PluginStuff
import Control.Monad(liftM)
import qualified Data.Map.Strict as M
import Data.Hashable
data Placeholder = Placeholder
data CommandMap = CommandMap (M.Map T.Text Placeholder)
data CommandWorkspace = CommandWorkspace Placeholder
data Sewer = Sewer (M.Map Int Manhole)


srcPluginPath :: IO FilePath
srcPluginPath = getXdgDirectory XdgData "gypsfulvus/srcplugins" >>= makeAbsolute
binPluginPath :: IO FilePath
binPluginPath = getXdgDirectory XdgData "gypsfulvus/srcplugins" >>= makeAbsolute


configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "gypsfulvus"



assCallbackWithManholeInSewer
  :: Hashable a1 =>
     TMVar (M.Map Int a2 -> M.Map Int a2)
     -> a1 -> a2 -> STM ()
assCallbackWithManholeInSewer sewer callback_name callback_manhole = do
  sewer_old <- takeTMVar sewer
  h_cname <- return $ hash callback_name
  putTMVar sewer $ M.insert h_cname callback_manhole
  
dispatchCommands sharedCommandWorkspace sharedTaskQueue = undefined
-- broadcast ouputs from routines to all (interested) parties
broadcastToConsumers consumerBroadcastChannel sharedCommandWorkspace sharedTaskQueue = undefined
-- collect all input from all comms plugins and queue for dispatch
collectInputs collectorChannel availableCommandMap sharedCommandWorkspace sharedTaskQueue = undefined

        
-- load all the routines that the bot can run (e.g. run tcl code, calculator, youtube, etc.)
loadLabourPlugins availableCommandMap = undefined
-- thread to pass any work to be done


runForever :: TMVar Bool -> IO ()
runForever diediedie =
  let block = do
        canaryDead <- readTMVar diediedie
        if (canaryDead) then
          return canaryDead
        else
          retry
  in atomically block >>= \isDone ->
      if (isDone) then putStrLn "Exiting cleanly." else error "I escaped my eternal prison somehow." -- it shouldn't be possible for the else to be reached unless something melts down
registerComms = undefined

loadIOBackends sewer = undefined
loadCoreCommands = undefined

execMain :: IO ()
execMain = do
  collectorChannel <- atomically newTChan -- normal channel for dumping any user input, this is the output channel for all plugins
  dumperChannel <- atomically newTChan -- uh this doesnt make any sense, every dings needs to have its own channel

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
  runForever canary
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
