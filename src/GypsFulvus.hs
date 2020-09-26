{-# LANGUAGE OverloadedStrings #-}
module GypsFulvus(execMain) where
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import System.Directory
import qualified Data.Text as T
import Control.Concurrent(ThreadId, forkIO, killThread, threadDelay)
import GypsFulvus.PluginStuff
import Control.Monad(liftM,filterM,forever)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Hashable
import qualified Control.Monad.Parallel as Par
import qualified Carrion.Plugin.IO.STDIO as CPISTDIO
import qualified Carrion.Plugin.TCL as TCLSIMP
import qualified Carrion.Plugin.IO.IRC.Client as IRCSIMP
import Prelude hiding ((++),putStrLn,putStr)
import Data.Text.IO(putStrLn, putStr)
import Debug.Trace
data Placeholder = Placeholder
data CommandMap = CommandMap {getCommandMap :: M.Map Int T.Text}
data CommandWorkspace = CommandWorkspace Placeholder
data Sewer = Sewer {getSewerMap :: M.Map Int Manhole}
data IOPIDS = IOPIDS [Int]

(++) :: T.Text -> T.Text -> T.Text
a ++ b = T.append a b


lookupPluginNameByCommand
  :: TMVar CommandMap -> T.Text -> STM (Maybe T.Text)
lookupPluginNameByCommand m c = do
  m <- readTMVar m
  case T.breakOn " " c of
    (sic,_) -> return $ M.lookup (hash sic) (getCommandMap m)

registerCommands :: TMVar(CommandMap) -> T.Text -> [T.Text] -> STM ()
registerCommands m pn tellFunc = do
  m' <- takeTMVar m
  let ncm = M.unions (map (\com -> M.insert (hash com) pn (getCommandMap m')) $ tellFunc)
  putTMVar m (CommandMap ncm)

  
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

corePlugName :: T.Text
corePlugName = "core"
mySignature = GenericStyleAutor corePlugName "local" "local"

isIOPlugin :: Sewage -> TMVar IOPIDS -> IO Bool
isIOPlugin sewage iopids = let pname = (hash . getName .nsAutorToGenericAutor . getSewageAutor $ sewage)
                    in do
  IOPIDS iop <- atomically $ readTMVar iopids
  return $ pname `elem` iop

runForever :: TMVar Sewer -> TMVar(CommandMap) -> TMVar(IOPIDS) -> IO ()
runForever s cmap iopids =
  let block = do
                mh <- lookupManholeInSewer s corePlugName
                case mh of
                  Just mh' -> readTChan $ getInputChan mh'
                  Nothing -> retry
  in forever $ do
       someGarbage <- atomically block
       let theAutor = show $ getSewageAutor someGarbage
       let theSewage = getSewage someGarbage
       amIIO <- isIOPlugin someGarbage iopids
       if (amIIO) then
         trySendToWorker s someGarbage cmap
       else do
         pm <- atomically $ lookupManholeInSewer s "IRC-Simple"
         case pm of
           Just pm -> regiftToWorker someGarbage pm
           Nothing -> return ()
       putStrLn $ T.pack theAutor ++ " sez:"
       putStrLn $ theSewage

trySendToWorker
  :: TMVar Sewer -> Sewage -> TMVar CommandMap -> IO ()
trySendToWorker sewer sewage cmap = do
  let sewage' = getSewage sewage
  pn <- atomically $ lookupPluginNameByCommand cmap sewage'
  case pn of
    Just pn' -> do
      pm <- atomically $ lookupManholeInSewer sewer pn'
      case pm of
        Just m -> regiftToWorker sewage m
        Nothing -> putStrLn $ "couldn't find channel to " ++ pn'
    Nothing -> putStrLn $ "Couldn't find plugin for command " ++ sewage'    



  
makeManhole :: TMVar(Sewer) -> T.Text -> IO (Maybe Manhole)
makeManhole s p = do
  coreManhole <- atomically $ lookupManholeInSewer s corePlugName
  case coreManhole of
    Just cm -> do
      coreInputChan <- return $ getInputChan cm
      pluginInputChan <- atomically $ newTChan
      return $ Just $ Manhole pluginInputChan coreInputChan
    Nothing -> return Nothing


tryRegisterPlugin :: TMVar(Sewer) -> T.Text -> (Manhole -> IO InitStatus) -> [T.Text] -> IO InitStatus
tryRegisterPlugin s plugName initFunc tellCommandsFunc = do
  im <- makeManhole s plugName
  case im of
    Just im' -> do
      moduleInitStatus <- initFunc im'
      case moduleInitStatus of
        GoodInitStatus -> do
          atomically $ assCallbackWithManholeInSewer s plugName im'
          return GoodInitStatus
        BadInitStatus errs -> return $ BadInitStatus $ "couldn't load the " ++ plugName ++ " plugin: " ++ errs
    Nothing -> return $ BadInitStatus $ T.pack "Catastrophic failure - core ejected."


makeNewSewer :: Manhole -> IO (TMVar Sewer)
makeNewSewer coreManhole = do
  let
    plugName = "core"
  emptySewer <- atomically $ newTMVar $ Sewer M.empty
  atomically $ assCallbackWithManholeInSewer (emptySewer) corePlugName coreManhole

regiop :: Hashable a => a -> TMVar IOPIDS -> STM ()
regiop pn iopids = do
  IOPIDS iopids' <- takeTMVar iopids
  putTMVar iopids (IOPIDS $ (hash pn):iopids')
  
stdioPlugName :: T.Text
stdioPlugName = "STDIO haskeline"

tclPlugName :: T.Text
tclPlugName = "TCL-Simple"
ircPlugName :: T.Text
ircPlugName = "IRC-Simple"

execMain :: IO ()
execMain = do
  collectorChannel <- atomically newTChan -- normal channel for dumping any user input, this is the output channel for all plugins
  dumperChannel <- atomically newTChan -- uh this doesnt make any sense, every dings needs to have its own channel
  commandMap <- atomically $ newTMVar $ CommandMap M.empty
  iopids <- atomically $ newTMVar $ IOPIDS []
  newSewer <- makeNewSewer $ Manhole collectorChannel dumperChannel
  tryRegisterPlugin newSewer stdioPlugName CPISTDIO.initPlugin CPISTDIO.tellCommands
  atomically $ registerCommands commandMap stdioPlugName CPISTDIO.tellCommands
  atomically $ regiop stdioPlugName iopids
  tryRegisterPlugin newSewer ircPlugName IRCSIMP.initPlugin IRCSIMP.tellCommands
  atomically $ registerCommands commandMap ircPlugName IRCSIMP.tellCommands
  atomically $ regiop ircPlugName iopids
  tryRegisterPlugin newSewer tclPlugName TCLSIMP.initPlugin TCLSIMP.tellCommands
  atomically $ registerCommands commandMap tclPlugName TCLSIMP.tellCommands
  let myTIDs  = []
  runForever newSewer commandMap iopids
  mapM_ killThread myTIDs
