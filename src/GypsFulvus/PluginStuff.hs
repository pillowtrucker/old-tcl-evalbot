{-# LANGUAGE OverloadedStrings #-}
module GypsFulvus.PluginStuff(loadCommsPlugins, Sewage(..), Manhole(..), InitStatus(..), SewageAutorInfo(..), IrcMask(..), genericAutorToNSAutor, nsAutorToGenericAutor) where
import Control.Monad
import System.Directory
import System.Plugins.Make
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import qualified Data.Text as T

(♯) :: T.Text -> T.Text -> T.Text
a ♯ b = (T.append) a b

tooTeToSt :: T.Text -> T.Text -> String
tooTeToSt a b = tup $ a ♯ "@" ♯ b

tp :: String -> T.Text
tp = T.pack
tup :: T.Text -> String
tup = T.unpack
data IrcMask = IrcMask {
                       getIdent:: T.Text,
                       getHostname :: T.Text}
instance Show IrcMask where
  show (IrcMask a b) = tooTeToSt a b

data SewageAutorInfo = NetworkIdentStyleAutor {
                                         getNick :: T.Text,
                                         getMask :: IrcMask,
                                         getChannel :: T.Text}
                     | GenericStyleAutor {getName :: T.Text,
                                     getLocation :: T.Text,
                                     getContext :: T.Text}
instance Show SewageAutorInfo where
  show (NetworkIdentStyleAutor a b c) = tup (c ♯ ":" ♯ a ♯ "!" ♯ tp (show b))
  show (GenericStyleAutor a b c) = tup $ c ♯ ":" ♯ (tp $ tooTeToSt a b)



genericAutorToNSAutor :: SewageAutorInfo -> SewageAutorInfo
genericAutorToNSAutor (GenericStyleAutor a b c) = NetworkIdentStyleAutor a (IrcMask a b) c
genericAutorToNSAutor b = b

nsAutorToGenericAutor :: SewageAutorInfo -> SewageAutorInfo
nsAutorToGenericAutor (NetworkIdentStyleAutor a (IrcMask _ b') c) = GenericStyleAutor a b' c
nsAutorToGenericAutor b = b

type Nickname = T.Text
type NetworkIdent = T.Text
type NetworkHostname = T.Text
type NetworkChannel = T.Text
makeNetworkIdentStyleAutor
  :: Nickname -> NetworkIdent -> NetworkHostname -> NetworkChannel -> SewageAutorInfo
makeNetworkIdentStyleAutor n i h c = NetworkIdentStyleAutor n (IrcMask i h) c

data Sewage = Sewage {
  getSewageAutor :: T.Text,
  getSewage :: T.Text
              }
data Manhole = Manhole {
                       getInputChan :: TChan Sewage,
                       getOutputChan :: TChan Sewage}
data InitStatus = GoodInitStatus | BadInitStatus T.Text

srcPluginPath :: IO FilePath
srcPluginPath = getXdgDirectory XdgData "gypsfulvus/srcplugins" >>= makeAbsolute


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
                   MakeFailure e -> putStrLn $ show e) s
    _ <- atomically $ swapTMVar canary True
    -- I don't actually want to quit here but I don't like errors from STM heuristics when the canary is GCed
    
    return ()

        
-- load all the routines that the bot can run (e.g. run tcl code, calculator, youtube, etc.)
loadLabourPlugins availableCommandMap = undefined
-- thread to pass any work to be done
