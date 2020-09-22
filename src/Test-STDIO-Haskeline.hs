module Main
where
import Carrion.Plugin.IO.STDIO
import GypsFulvus.PluginStuff
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import qualified Data.Text as T
main :: IO ()
main = do
  inchan <- atomically $ newTChan
  outchan <- atomically $ newTChan
  let mymanhole = Manhole inchan outchan
  initPlugin mymanhole
  let testCommand = Sewage (GenericStyleAutor (T.pack "Test Bin") (T.pack "local") (T.pack "local")) (T.pack "inspect inspect")
  atomically $ writeTChan inchan testCommand
  forever $ do
    newstuff <- atomically $ readTChan outchan
    putStrLn $ "Backend " ++ (show $ getSewageAutor newstuff) ++ " returned " ++ (T.unpack $ getSewage newstuff)
