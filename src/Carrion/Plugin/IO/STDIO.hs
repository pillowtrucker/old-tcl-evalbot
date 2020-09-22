{-# LANGUAGE OverloadedStrings #-}
module Carrion.Plugin.IO.STDIO
    ( initPlugin,
      processCommand,
      testThing,
      tellCommands,
    ) where
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent(forkIO)
import Control.Concurrent.STM
import qualified Data.Text as T
import GypsFulvus.PluginStuff(Manhole(..),Sewage(..), InitStatus(..),SewageAutorInfo(..),genericAutorToNSAutor,inspectManhole,regift, stripCommandPrefix')
import System.Console.Haskeline
import Data.Maybe
import qualified Data.List as L
import Prelude hiding ((++))

a ++ b = T.append a b
testThing = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ T.unpack ("Input was: " ++ T.pack input)
                                loop
mySignature = GenericStyleAutor "STDIO haskeline" "local" "local"
tellCommands = [""]

stripCommandLocal c m = stripCommandPrefix' c tellCommands m mySignature
initPlugin :: Manhole -> IO InitStatus
initPlugin manhole = do
  
  forkIO $ rEPL manhole
  return GoodInitStatus

processCommand = undefined

processUserInputs = undefined
processCommandResults = undefined

rEPL manhole = do
  let getInputs = runInputT defaultSettings loop
      fuku :: InputT IO ()
      fuku = do
        aresult <- liftIO (inspectManhole manhole)
        outputStrLn $ T.unpack . getSewage $ aresult
        fuku
      loop :: InputT IO ()
      loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do liftIO $ regift (Sewage mySignature (T.pack input)) manhole
                                loop
      getResults = runInputT defaultSettings fuku      
  forkIO $ getInputs
  forkIO $ getResults
  return ()
