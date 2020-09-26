{-# LANGUAGE OverloadedStrings #-}
module Main
where
import Carrion.Plugin.IO.IRC.Client(initPlugin)
import GypsFulvus.PluginStuff
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent
import qualified Data.Text as T
main :: IO ()
main = do
  inchan <- atomically $ newTChan
  outchan <- atomically $ newTChan
  let mymanhole = Manhole inchan outchan
  forkIO $ initPlugin mymanhole >> return ()
  forever $ do
    newstuff <- atomically $ readTChan outchan
    putStrLn $ "Backend " ++ (show $ getSewageAutor newstuff) ++ " returned " ++ (T.unpack $ getSewage newstuff)
