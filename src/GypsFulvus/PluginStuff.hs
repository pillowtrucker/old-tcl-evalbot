{-# LANGUAGE OverloadedStrings #-}
module GypsFulvus.PluginStuff(Sewage(..), Manhole(..), InitStatus(..), SewageAutorInfo(..), IrcMask(..), genericAutorToNSAutor, nsAutorToGenericAutor, inspectManhole, regift, stripCommandPrefix', regiftToWorker, Carrion(..),CarrionPlugin(..)) where
import Control.Monad


import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import qualified Data.List as L
import qualified Data.Text as T

(♯) :: T.Text -> T.Text -> T.Text
a ♯ b = (T.append) a b

tooTeToSt :: T.Text -> T.Text -> String
tooTeToSt a b = tup $ a ♯ "@" ♯ b

stripCommandPrefix
  :: T.Text -> [T.Text] -> Either [Maybe T.Text] (Maybe T.Text)
stripCommandPrefix c = uniqueHit . filter (/= Nothing) . map (\cs -> T.stripPrefix (cs ♯ " ") (c ♯ " "))
  where
    uniqueHit cs = if (L.length cs == (1 :: Int)) then Right $ head cs else Left cs

stripCommandPrefix'
  :: T.Text -> [T.Text] -> Manhole -> SewageAutorInfo -> IO (Maybe T.Text)
stripCommandPrefix' c ccs m sig = case stripCommandPrefix c ccs of
  Right c -> return c
  Left cs -> do
    sew <- regift (Sewage sig (if L.null cs then ("No such command: " ♯ c) else ("Found multiple matching commands: " ♯ ((L.foldr1 (\h ng  -> h ♯ ", " ♯ ng)) $ (map (fromMaybe "")) cs)))) m
    return Nothing

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
  getSewageAutor :: SewageAutorInfo,
  getSewage :: T.Text
              }
data Manhole = Manhole {
                       getInputChan :: TChan Sewage,
                       getOutputChan :: TChan Sewage}
data InitStatus = GoodInitStatus | BadInitStatus T.Text deriving Show

inspectManhole :: Manhole -> IO Sewage
inspectManhole = atomically . readTChan . getInputChan

regift :: Sewage -> Manhole -> IO ()
regift g = atomically . (flip writeTChan g) . getOutputChan

regiftToWorker :: Sewage -> Manhole -> IO ()
regiftToWorker g = atomically . (flip writeTChan g) . getInputChan

data CarrionPlugin = InputPlugin {getInitPlugin :: (Manhole -> IO InitStatus), getTellCommands :: [T.Text], getMyPlugName :: T.Text} | WorkerPlugin {getInitPlugin :: (Manhole -> IO InitStatus), getTellCommands :: [T.Text], getMyPlugName :: T.Text}

class Carrion a where
  initPlugin :: a -> Manhole -> IO InitStatus
  tellCommands :: a -> [T.Text]
  tellPlugName :: a -> T.Text
instance Carrion CarrionPlugin where
  initPlugin = getInitPlugin
  tellCommands = getTellCommands
  tellPlugName = getMyPlugName
