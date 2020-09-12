module Main where
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Concurrent(ThreadId, forkIO, killThread)
data Placeholder = Placeholder
data CommandMap = CommandMap (M.Map T.Text Placeholder)
data CommandWorkspace = CommandWorkspace Placeholder

-- load all the plugins for IO (e.g. IRC, stdio, maybe matrix procol, telnet, whatever)
loadCommsPlugins collectorChannel = undefined
-- load all the routines that the bot can run (e.g. run tcl code, calculator, youtube, etc.)
loadLabourPlugins availableCommandMap = undefined
-- thread to pass any work to be done
dispatchCommands sharedCommandWorkspace sharedTaskQueue = undefined
-- broadcast ouputs from routines to all (interested) parties
broadcastToConsumers consumerBroadcastChannel sharedCommandWorkspace sharedTaskQueue = undefined
-- collect all input from all comms plugins and queue for dispatch
collectInputs collectorChannel availableCommandMap sharedCommandWorkspace sharedTaskQueue = undefined


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



main :: IO ()
main = do
  collectorChannel <- atomically newTChan -- normal channel for dumping any user input
  consumerBroadcastChannel <- atomically newBroadcastTChan
  loadCommsPlugins collectorChannel
  availableCommandMap <- atomically $ newTMVar CommandMap
  loadLabourPlugins availableCommandMap
  sharedCommandWorkspace <- atomically $ newTMVar CommandWorkspace
  sharedTaskQueue <- atomically $ newTChan
  dispatchTID <- forkIO $ dispatchCommands sharedCommandWorkspace sharedTaskQueue
  broadcastTID <- forkIO $ broadcastToConsumers consumerBroadcastChannel sharedCommandWorkspace sharedTaskQueue
  collectorTID <- forkIO $ collectInputs collectorChannel availableCommandMap sharedCommandWorkspace sharedTaskQueue
  
  canary <- atomically $ newTMVar False -- simple 'should I exit' canary
  runForever canary
  mapM_ killThread [dispatchTID, broadcastTID, collectorTID]
