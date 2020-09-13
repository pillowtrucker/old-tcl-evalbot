module GypsFulvus.PluginStuff(loadCommsPlugins) where
import Control.Monad
import System.Directory
import System.Plugins.Make
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

srcPluginPath :: IO FilePath
srcPluginPath = getXdgDirectory XdgData "gypsfulvus/src_plugins" >>= makeAbsolute


configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "gypsfulvus"




-- load all the plugins for IO (e.g. IRC, stdio, maybe matrix procol, telnet, whatever)



loadCommsPlugins canary collectorChannel =
  let potentialPlugins = srcPluginPath >>= \pp -> listDirectory pp >>= filterM (\fuku -> doesDirectoryExist (pp ++ "/" ++ fuku)) >>= mapM (\fuku -> return (pp ++ "/" ++ fuku))
  in do
    srcPluginPath >>= putStrLn
    srcPluginPath >>= listDirectory >>= mapM putStrLn
    srcPluginPath >>= \pp -> listDirectory pp >>= filterM (\fuku -> putStrLn (pp ++ "/" ++ fuku) >> doesDirectoryExist (pp ++ "/" ++ fuku)) 
    pp <- potentialPlugins
    mapM_ putStrLn pp
    ff <- mapM (\d -> findFile [d] "Plugin.hs") pp
    let rff = map (fromMaybe "") $ filter (/= Nothing) ff
    s <- mapM (\hng -> makeAll hng ["-v","-dynamic"]) rff
    mapM (\s' -> case s' of
                   MakeSuccess _ p -> putStrLn p
                   MakeFailure e -> do
                     putStrLn $ show e
                     
                     return ()) s
    _ <- atomically $ swapTMVar canary True
    -- I don't actually want to quit here but I don't like errors from STM heuristics when the canary is GCed
    
    return ()

        
-- load all the routines that the bot can run (e.g. run tcl code, calculator, youtube, etc.)
loadLabourPlugins availableCommandMap = undefined
-- thread to pass any work to be done
