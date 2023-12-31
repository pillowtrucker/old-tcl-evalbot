{-# LANGUAGE OverloadedStrings #-}
module Carrion.Plugin.IO.IRC.Client
  (initPlugin,tellCommands,myPlugName)
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
import Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import Control.Lens
import Control.Concurrent(threadDelay,forkIO)
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Monad(liftM,forever)
import Data.Monoid
import qualified Data.Map as M
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Network.IRC.CTCP(CTCPByteString(..))
import Control.Applicative ((<$>), (<|>))
import Data.List(nub,(\\))
import Data.Ini
import Data.Maybe(fromMaybe)
import qualified Data.Text.IO as TIO
type MyNicknames = M.Map (T.Text) ([T.Text])


(♯) :: T.Text -> T.Text -> T.Text
a ♯ b = T.append a b

unpack :: T.Text -> String
unpack = T.unpack

myPlugName :: T.Text
myPlugName = T.pack "IRC-Simple"

lOCAL :: T.Text 
lOCAL = T.pack "local"

mySignature :: SewageAutorInfo
mySignature = GenericStyleAutor myPlugName myPlugName lOCAL

tellCommands :: [T.Text]
tellCommands = ["tcl"]

privateBotCommands :: [T.Text]
privateBotCommands = ["!join","!part","!kick","!op","!cycle","!reconnect","!ostracise","tcl"]

myOwners :: [[Char]]
myOwners = ["hastur"]

--myChannels :: [T.Text]
--myChannels = ["#exquisitebot"]

-- this dogshit irc library doesnt seem to have a concept of 'people in the channel(s)'
rPL_NAMREPLY :: Int
rPL_NAMREPLY = 353

sendNAMES :: T.Text -> IRC s ()
sendNAMES c = send $ RawMsg $ "NAMES " ♯ c

joinHandler' :: EventHandler s
joinHandler' = EventHandler (\ev -> matchNumeric 331 ev <|> matchNumeric 332 ev) $ \_ args -> case args of
  (c:_) -> do
    sendNAMES c
  _ -> pure ()

replaceNNS
  :: Ord k =>
     TMVar (M.Map k [T.Text]) -> k -> T.Text -> STM (M.Map k [T.Text])
replaceNNS nns theChan theNicknames= do
        lnns <- takeTMVar nns
        let curList = M.lookup theChan lnns
            fff = M.insert theChan (case curList of
                                 Nothing -> T.splitOn " " theNicknames
                                 Just cl -> nub (cl ++ (T.splitOn " " theNicknames))) lnns
        putTMVar nns fff
        return fff

otherJoinHandler :: EventHandler s
otherJoinHandler = huntAlligators (matchType _Join) $ \_ c -> sendNAMES c

otherPartHandler
  :: TMVar (M.Map (NickName T.Text) [ChannelName T.Text])
     -> EventHandler s
otherPartHandler nns = huntAlligators (matchType' _Part nns) $ \src (nns, (c,r)) -> do
  case src of
    Channel n c -> do
      liftIO . atomically $ removeFromNNS nns c n
      return ()
    _ -> return ()

otherQuitHandler
  :: TMVar (M.Map (NickName T.Text) [ChannelName T.Text])
     -> EventHandler s
otherQuitHandler nns = huntAlligators (matchType' _Quit nns) $ \src (nns, r) -> do
  case src of
    Channel n c -> do
      liftIO . atomically $ removeFromNNS nns c n
      return ()
    _ -> return ()

removeFromNNS
  :: (Ord k, Eq a) =>
     TMVar (M.Map k [a]) -> k -> a -> STM (M.Map k [a])
removeFromNNS nns theChan theNick = do
        lnns <- takeTMVar nns
        let curList = M.lookup theChan lnns
            fff = M.insert theChan (case curList of
                                 Nothing -> []
                                 Just cl -> nub (filter (/= theNick) cl)) lnns
        putTMVar nns fff
        return fff

namesReplyHandler
  :: a -> TMVar (M.Map T.Text [T.Text]) -> EventHandler s
namesReplyHandler mh nns = huntAlligators (matchNumeric' rPL_NAMREPLY (mh,nns)) $
  \src ((mh,nns), (meirl:theEqualsSignAsASeparateElementWhyTheFuckNot:theChan:theNicknames:[])) ->
    (liftIO . atomically $ replaceNNS nns theChan theNicknames) >> return () -- >>= (liftIO . putStrLn . show)
     

matchNumeric'
  :: Int -> a1 -> Event a2 -> Maybe (a1, [a2])
matchNumeric' n intruder ev = case _message ev of
                         Numeric num args | n == num -> Just (intruder,args)
                         _ -> Nothing

matchType'
  :: Getting (First b) (Message a1) b
     -> a2 -> Event a1 -> Maybe (a2, b)
matchType' k intruder ev = case preview k . _message $ ev of
  Nothing -> Nothing
  Just sth -> Just (intruder,sth)

unimplementedCommand :: T.Text
unimplementedCommand = "Command not implemented."


huntAlligators
  :: (Event T.Text -> Maybe b)
     -> (Source T.Text -> b -> IRC s ()) -> EventHandler s
huntAlligators mf cf = EventHandler mf cf


fYourKickHandler :: TMVar (M.Map T.Text [T.Text]) -> EventHandler s
fYourKickHandler nns = huntAlligators (matchType' _Kick nns) $ \src (nns, (channame, nickname, reason)) -> do
        tvarI <- get instanceConfig <$> getIRCState
        iGotBooted <- liftIO . atomically $ do
          theNick <- get nick <$> readTVar tvarI
          return $ case src of
            Channel c _
              | nickname == theNick -> True
              | otherwise    -> False
            _ -> False
        if(iGotBooted) then do
--          liftIO $ regift (Sewage mySignature (T.pack "got kicked from " ♯ channame)) mh
          liftIO (threadDelay 10000000)
          send $ Join channame
        else liftIO . atomically $ removeFromNNS nns nickname channame >> return ()

spamCoordinator :: Manhole -> T.Text -> IO ()
spamCoordinator mh msg = regift (Sewage mySignature msg) mh

spamFromIRC :: Manhole -> T.Text -> T.Text -> T.Text -> IO ()
spamFromIRC mh msg thenick thechan = regift (Sewage (GenericStyleAutor thenick myPlugName thechan) msg) mh

stripDangerousNickname :: p -> T.Text -> T.Text
stripDangerousNickname n = T.filter (\c -> (not . (c `elem`)) ['[',']','{','}'])

detectCommandHandler
  :: (TMVar (M.Map (ChannelName T.Text) [T.Text]), Manhole)
     -> EventHandler s
detectCommandHandler (nns,mh) = huntAlligators (matchType' _Privmsg (nns,mh)) $ \src ((nns,mh),(tgt,blergh)) -> do
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
                        case src of
                          Channel thechannelname thenickname -> do
--                            liftIO $ putStrLn $ "what the fuck: " ++ T.unpack thenickname ++ " " ++ T.unpack thechannelname
                            lnns <- liftIO . atomically $ readTMVar nns
                            let thenames = (fromMaybe [T.pack ""]) $ M.lookup thechannelname lnns
                            liftIO $ spamCoordinator mh $ T.pack "tcl cache put irc chanlist [list " ♯ (foldr1 (\a b -> a ♯ " " ♯ b) $ (map (stripDangerousNickname $ T.pack)) $ thenames) ♯ "]"
                            liftIO $ spamFromIRC mh body thenickname thechannelname -- actually process the commands here
                          _ -> return () -- no secret commands fuck it
                  else return ()
                Left _ -> return ()

stripCommandLocal :: T.Text -> Manhole -> IO (Maybe T.Text)
stripCommandLocal c m = stripCommandPrefix' c tellCommands m mySignature

data IRCConfig = IRCConfig {getIRCHost:: T.Text, getIRCPort :: Int, getIRCChannels :: [T.Text], getIRCNickname :: T.Text} | FuckedIRCConfig T.Text

getIRCConfig :: IO IRCConfig
getIRCConfig = do
  c <- TIO.readFile "./exquisiterobot.conf" >>= return . parseIni
  case c of
    Left _ -> return $ FuckedIRCConfig "Couldn't read the configuration file."
    Right i -> do
      let host = lookupValue "Server" "hostname" i
          port = lookupValue "Server" "port" i
          channels = lookupValue "Server" "channels" i
          myNickname = lookupValue "Server" "nickname" i
      case (host,port,channels,myNickname) of
        (Right h, Right p, Right cs, Right n) -> return $ IRCConfig h (read . T.unpack $ p) (T.splitOn " " cs) n
        (h,p,cs,n) -> return $ FuckedIRCConfig $ foldr1 (♯) . map (T.pack . show) $ [h,p,cs,n]

initPlugin :: Manhole -> IO InitStatus
initPlugin mh = do
  ircConfig <- getIRCConfig
  case ircConfig of
    IRCConfig myHost myPort myChannels myNickname -> do
      let cpara = defaultParamsClient (unpack myHost) ""
          validate cs vc sid cc = do
            -- First validate with the standard function
            res <- (onServerCertificate $ clientHooks cpara) cs vc sid cc
          -- Then strip out non-issues
            return $ filter (`notElem` [UnknownCA, SelfSigned, Expired]) res
          myClientConfig = (tlsClientConfig myPort (encodeUtf8 myHost)) { tlsClientTLSSettings = TLSSettings cpara
                                                                          { clientHooks = (clientHooks cpara)
                                                                            { onServerCertificate = validate }
                                                                          , clientSupported = (clientSupported cpara)
                                                                            { supportedVersions = [TLS12, TLS11, TLS10]
                                                                            , supportedCiphers = ciphersuite_strong
                                                                            }
                                                                          }
                                                                        }
          conn = (tlsConnection $ WithClientConfig myClientConfig) & flood .~ 0
      myNNS <- atomically $ newTMVar M.empty
      let namesReplyHandler' = namesReplyHandler mh myNNS
          rejoinOnKickHandler = fYourKickHandler myNNS
          mySpecialHandlers = [rejoinOnKickHandler,detectCommandHandler',joinHandler',namesReplyHandler',otherJoinHandler,otherPartHandler myNNS, otherQuitHandler myNNS]
          cfg  = defaultInstanceConfig myNickname & channels %~ (myChannels ++) & handlers %~ (++ mySpecialHandlers)
          detectCommandHandler' = detectCommandHandler (myNNS,mh)
      myIRCState <- newIRCState conn cfg ()
      forkIO $ runClientWith myIRCState
      forkIO $ acceptExternalComms myIRCState mh
      return GoodInitStatus
    FuckedIRCConfig err -> return $ BadInitStatus err
    
acceptExternalComms :: MonadIO f => IRCState s -> Manhole -> f b
acceptExternalComms myIRCState manhole =
  let inspectManhole = atomically . readTChan . getInputChan
      regift g = atomically . (flip writeTChan g) . getOutputChan in
  forever $ do
    newGift <- liftIO $ inspectManhole manhole
--    putStrLn $ "trying to maybe send to " ++ (T.unpack .getChannel . genericAutorToNSAutor . getSewageAutor $ newGift)
    
    runIRCAction (mapM (\fff -> send $ Privmsg (getChannel . genericAutorToNSAutor . getSewageAutor $ newGift) $ Right fff) (foldr1 (++) . map (T.chunksOf 255) . nlSplit $ getSewage newGift)) myIRCState



nlSplit :: T.Text -> [T.Text]
nlSplit = T.splitOn "\n"
