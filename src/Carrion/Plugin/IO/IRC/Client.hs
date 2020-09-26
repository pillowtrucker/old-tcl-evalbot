{-# LANGUAGE OverloadedStrings #-}
module Carrion.Plugin.IO.IRC.Client
  (initPlugin,tellCommands)
where
import GypsFulvus.PluginStuff(Manhole(..),Sewage(..), InitStatus(..),SewageAutorInfo(..),genericAutorToNSAutor,inspectManhole,regift, stripCommandPrefix')
import Network.IRC.Client
import Data.Conduit.Network.TLS
import Network.Connection
import Network.IRC.Conduit
import Network.TLS
import Network.TLS.Extra
import Data.X509.Validation
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.ByteString          (ByteString)
import Data.Text.Encoding       (decodeUtf8)
import Control.Lens
import Control.Concurrent(threadDelay,forkIO)
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Monad(liftM)
import Data.Monoid
import qualified Data.Map as M
import Data.ByteString(ByteString)
import Network.IRC.CTCP(CTCPByteString(..))
import Control.Applicative ((<$>), (<|>))
type MyNicknames = M.Map (T.Text) ([T.Text])


a ♯ b = T.append a b
unpack = T.unpack
myPlugName :: T.Text
myPlugName = T.pack "IRC-Simple"
lOCAL :: T.Text 
lOCAL = T.pack "local"

mySignature = GenericStyleAutor myPlugName lOCAL lOCAL
tellCommands = ["tcl"]
privateBotCommands = ["!join","!part","!kick","!op","!cycle","!reconnect","!ostracise","tcl"]
myOwners = ["hastur"]

myChannels :: [T.Text]
myChannels = ["#exquisitebot"]

-- this dogshit irc library doesnt seem to have a concept of 'people in the channel(s)'
rPL_NAMREPLY :: Int
rPL_NAMREPLY = 353

joinHandler' :: EventHandler s
joinHandler' = EventHandler (\ev -> matchNumeric 331 ev <|> matchNumeric 332 ev) $ \_ args -> case args of
  (c:_) -> do
    send $ RawMsg $ "NAMES " ♯ c
  _ -> pure ()

namesReplyHandler
  :: a -> TMVar (M.Map T.Text [T.Text]) -> EventHandler s
namesReplyHandler mh nns = huntAlligators (matchNumeric' rPL_NAMREPLY (mh,nns)) $ \src ((mh,nns), (meirl:theEqualsSignAsASeparateElementWhyTheFuckNot:theChan:theNicknames:[])) -> do
--     let fff =  (T.breakOn " :" (foldr1 (\a b -> a ♯ " *BOINK* " ♯ b) thetail))
--         (theChan,theNicknames) = fff & _2 %~ (T.splitOn " " . T.drop 1)
     grr <- liftIO . atomically $ do
        lnns <- takeTMVar nns
        let curList = M.lookup theChan lnns
            fff = M.insert theChan (case curList of
                                 Nothing -> T.splitOn " " theNicknames
                                 Just cl -> cl ++ (T.splitOn " " theNicknames)) lnns
        return fff
     liftIO $ putStrLn $ "what the fuck did I just do: "  ++  show grr
     return ()

matchNumeric'
  :: Int -> a1 -> Event a2 -> Maybe (a1, [a2])
matchNumeric' n intruder ev = case _message ev of
                         Numeric num args | n == num -> Just (intruder,args)
                         _ -> Nothing

huntCrocodiles
  :: Getting (First b) (Message a1) b
     -> a2 -> Event a1 -> Maybe (a2, b)
huntCrocodiles k mh ev = case preview k . _message $ ev of
  Nothing -> Nothing
  Just sth -> Just (mh,sth)

unimplementedCommand :: T.Text
unimplementedCommand = "Command not implemented."


huntAlligators
  :: (Event T.Text -> Maybe b)
     -> (Source T.Text -> b -> IRC s ()) -> EventHandler s
huntAlligators mf cf = EventHandler mf cf

fYourKickHandler :: Manhole -> EventHandler s
fYourKickHandler mh = huntAlligators (huntCrocodiles _Kick mh) $ \src (mh, (channame, nickname, reason)) -> do
        tvarI <- get instanceConfig <$> getIRCState
        iGotBooted <- liftIO . atomically $ do
          theNick <- get nick <$> readTVar tvarI
          return $ case src of
            Channel c _
              | nickname == theNick -> True
              | otherwise    -> False
            _ -> False
        if(iGotBooted) then do
          liftIO $ regift (Sewage mySignature (T.pack "got kicked from " ♯ channame)) mh
          liftIO (threadDelay 10000000)
          send $ Join channame
        else return ()

spamCoordinator :: Manhole -> T.Text -> IO ()
spamCoordinator mh msg = regift (Sewage mySignature msg) mh

detectCommandHandler :: Manhole -> EventHandler s
detectCommandHandler mh = huntAlligators (huntCrocodiles _Privmsg mh) $ \src (mh,(tgt,blergh)) -> do
              tvarI <- get instanceConfig <$> getIRCState
              case blergh of
                Right body -> do
                  let theC = ((T.breakOn " " body) ^. _1)
                  let fff = theC `elem` privateBotCommands
                  if(fff) then do
                    mCommand <- liftIO $ stripCommandLocal body mh
                    case mCommand of
                      Nothing -> return ()
                      Just c -> do
                        
                        liftIO $ spamCoordinator mh body -- actually process the commands here
                  else return ()
                Left _ -> return ()
stripCommandLocal :: T.Text -> Manhole -> IO (Maybe T.Text)
stripCommandLocal c m = stripCommandPrefix' c tellCommands m mySignature
        
initPlugin :: Manhole -> IO InitStatus
initPlugin mh = do
  let myHost = "darkarmy.chat"
      myPort = 6697
      myNickname = "ExquisiteRobot"
      cpara = defaultParamsClient (unpack $ decodeUtf8 myHost) ""
      validate cs vc sid cc = do
         -- First validate with the standard function
         res <- (onServerCertificate $ clientHooks cpara) cs vc sid cc
         -- Then strip out non-issues
         return $ filter (`notElem` [UnknownCA, SelfSigned]) res
      myClientConfig = (tlsClientConfig myPort myHost) { tlsClientTLSSettings = TLSSettings cpara
    { clientHooks = (clientHooks cpara)
      { onServerCertificate = validate }
    , clientSupported = (clientSupported cpara)
      { supportedVersions = [TLS12, TLS11, TLS10]
      , supportedCiphers = ciphersuite_strong
      }
    }
  }
      rejoinOnKickHandler = fYourKickHandler mh
      detectCommandHandler' = detectCommandHandler mh
      conn = tlsConnection $ WithClientConfig myClientConfig
  myNNS <- atomically $ newTMVar M.empty
  let namesReplyHandler' = namesReplyHandler mh myNNS
      mySpecialHandlers = [rejoinOnKickHandler,detectCommandHandler',joinHandler',namesReplyHandler']
      cfg  = defaultInstanceConfig myNickname & channels %~ (myChannels ++) & handlers %~ (++ mySpecialHandlers)
  forkIO $ runClient conn cfg ()
  return GoodInitStatus
