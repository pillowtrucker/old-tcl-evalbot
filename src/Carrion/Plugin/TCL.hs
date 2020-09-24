{-# LANGUAGE ForeignFunctionInterface #-}

module Carrion.Plugin.TCL
    ( initPlugin,
      processCommand,
      tellCommands
    ) where
import Control.Monad
import Control.Concurrent(forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import System.Posix.DynamicLinker
import System.Environment
import Foreign.Ptr
import Foreign.C.String
import qualified Data.Text as T
import GypsFulvus.PluginStuff(Manhole(..),Sewage(..), InitStatus(..),SewageAutorInfo(..),genericAutorToNSAutor, stripCommandPrefix', regift)
data Tcl_Interp = Tcl_Interp deriving Show
type Tcl_Interp_Ptr = Ptr Tcl_Interp
type TCL_Actual_Version = CString
type TCL_Wanted_Version = CString
type TclScriptString = CString
type TclScriptStringByteLen = Int
type TclEvalFlags = Int
type WantExact = Int
type TclScriptFilename = CString
data TCLCommand = TCLCommand {getTCLCNick :: String,
                              getTCLCMask :: String,
                              getTCLCHandle_o_O:: String,
                              getTCLCChannel :: String,
                              getTCLCActualCommand :: String
                             }

type Tcl_EvalFile_Sig = (Tcl_Interp_Ptr -> TclScriptFilename -> IO Int)
type Tcl_EvalEx_Sig = (Tcl_Interp_Ptr -> TclScriptString -> TclScriptStringByteLen -> TclEvalFlags -> IO Int)
type Tcl_GetStringResult_Sig = (Tcl_Interp_Ptr -> IO CString)
type Tcl_CancelEval_Sig =  (Tcl_Interp_Ptr -> Ptr Tcl_Obj_Dummy -> Ptr Tcl_ClientData_Dummy -> Int -> IO Int)
type Tcl_AsyncInvoke_Sig = (Tcl_Interp_Ptr -> Int -> IO Int)
foreign import ccall "dynamic" mkTcl_CreateInterp :: FunPtr (IO Tcl_Interp_Ptr) -> IO (Tcl_Interp_Ptr)

data Tcl_Obj_Dummy = Tcl_Obj_Dummy
data Tcl_ClientData_Dummy = Tcl_ClientData_Dummy

foreign import ccall "&Tcl_InitStubs_wrap" tcl_InitStubs :: FunPtr (Tcl_Interp_Ptr -> TCL_Wanted_Version -> WantExact -> IO TCL_Actual_Version)
foreign import ccall "dynamic" mkTcl_InitStubs :: FunPtr (Tcl_Interp_Ptr -> TCL_Wanted_Version -> WantExact -> IO TCL_Actual_Version) -> (Tcl_Interp_Ptr -> TCL_Wanted_Version -> WantExact -> IO TCL_Actual_Version)
foreign import ccall "dynamic" mkTcl_FindExecutable :: FunPtr (CString -> IO CString) -> (CString -> IO CString)
foreign import ccall "dynamic" mkTcl_InitMemory :: FunPtr (Tcl_Interp_Ptr -> IO ()) -> (Tcl_Interp_Ptr -> IO ())
foreign import ccall "dynamic" mkTcl_Init :: FunPtr (Tcl_Interp_Ptr -> IO Int) -> (Tcl_Interp_Ptr -> IO Int)
foreign import ccall "dynamic" mkTcl_CancelEval :: FunPtr Tcl_CancelEval_Sig -> Tcl_CancelEval_Sig
foreign import ccall "dynamic" mkTcl_AsyncInvoke :: FunPtr Tcl_AsyncInvoke_Sig -> Tcl_AsyncInvoke_Sig
foreign import ccall "dynamic" mkTcl_EvalFile :: FunPtr Tcl_EvalFile_Sig -> Tcl_EvalFile_Sig
foreign import ccall "dynamic" mkTcl_GetStringResult :: FunPtr Tcl_GetStringResult_Sig -> Tcl_GetStringResult_Sig
foreign import ccall "dynamic" mkTcl_EvalEx :: FunPtr (Tcl_Interp_Ptr -> TclScriptString -> TclScriptStringByteLen -> TclEvalFlags -> IO Int) -> (Tcl_Interp_Ptr -> TclScriptString -> TclScriptStringByteLen -> TclEvalFlags -> IO Int)

tu :: T.Text -> String
tu = T.unpack
tellCommands :: [T.Text]
tellCommands = map T.pack ["tcl"]
myPluginName = T.pack "TCL-Simple"
tl :: T.Text
tl = T.pack "local"
mySignature :: SewageAutorInfo
mySignature = GenericStyleAutor myPluginName tl tl
stripCommandLocal :: T.Text -> Manhole -> IO (Maybe T.Text)
stripCommandLocal c m = stripCommandPrefix' c tellCommands m mySignature
fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson
  :: SewageAutorInfo -> String -> TCLCommand
fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson b = case b of
  GenericStyleAutor a b c -> fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson . genericAutorToNSAutor $ GenericStyleAutor a b c
  NetworkIdentStyleAutor a b c -> TCLCommand (tu a) (show b) "" (tu c)


mkTCLCommandFromAIAndMsg :: SewageAutorInfo -> String -> TCLCommand
mkTCLCommandFromAIAndMsg = fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson

data TCLInterpreterWrapper = TCLInterpreterWrapper {getInterp :: TMVar(Tcl_Interp_Ptr),
                                                    getEvalFile :: Tcl_EvalFile_Sig,
                                                    getEvalEx :: Tcl_EvalEx_Sig,
                                                    getGetStringResult :: Tcl_GetStringResult_Sig,
                                                    getCancelEval :: Tcl_CancelEval_Sig,
                                                    getAsyncInvoke :: Tcl_AsyncInvoke_Sig
                                                   }

lEN_AUTO :: Int
lEN_AUTO = -1
eVAL_FLAGS_CLEAR :: Int
eVAL_FLAGS_CLEAR = 0

dumpDebug _ = return ()

initPlugin :: Manhole -> IO InitStatus
initPlugin manhole = do
  myFakeArg0 <- getExecutablePath >>= newCString
  myTCLDl <- dlopen "/usr/lib/libtcl8.6.so" [RTLD_NOW]
  let bless name convf = dlsym myTCLDl name >>= \fp -> return $ convf $ fp
  tcl_CreateInterp <- bless "Tcl_CreateInterp" mkTcl_CreateInterp
  interp <- tcl_CreateInterp
  let tcl_InitStubs' = mkTcl_InitStubs tcl_InitStubs
  wanted_interp_version <- newCString "8.6"
  actual_version <- tcl_InitStubs' interp wanted_interp_version 0 >>= peekCString
  dumpDebug actual_version
  tcl_FindExecutable <- bless "Tcl_FindExecutable" mkTcl_FindExecutable
  theComputedExecutablePath <- tcl_FindExecutable $ myFakeArg0
  if nullPtr == theComputedExecutablePath then
    dumpDebug "Couldn't Tcl_FindExecutable()"
  else
    peekCString theComputedExecutablePath >>= dumpDebug
  tcl_InitMemory <- bless "Tcl_InitMemory" mkTcl_InitMemory
  tcl_InitMemory interp
  tcl_Init <- bless "Tcl_Init" mkTcl_Init
  tcl_Init_status <- tcl_Init interp
  dumpDebug $ show tcl_Init_status
  if (tcl_Init_status /= 0) then
    return $ BadInitStatus $ T.pack "non-zero return"
  else do
    tcl_EvalEx <- bless "Tcl_EvalEx" mkTcl_EvalEx
    tcl_GetStringResult <- bless "Tcl_GetStringResult" mkTcl_GetStringResult
    tcl_EvalFile <- bless "Tcl_EvalFile" mkTcl_EvalFile
    tcl_CancelEval <- bless "Tcl_CancelEval" mkTcl_CancelEval
    tcl_AsyncInvoke <- bless "Tcl_AsyncInvoke" mkTcl_AsyncInvoke
    smeginitstatus <- newCString "./src/smeggdrop/smeggdrop.tcl" >>= \fn -> tcl_EvalFile interp fn
    threadsafe_interp_duh <- atomically $ newTMVar interp

    let wrappedinterp = TCLInterpreterWrapper threadsafe_interp_duh tcl_EvalFile tcl_EvalEx tcl_GetStringResult tcl_CancelEval tcl_AsyncInvoke
    forkIO $ rEPL wrappedinterp manhole
    return GoodInitStatus




processCommand :: TCLInterpreterWrapper -> Sewage -> IO T.Text
processCommand wi s = do
  let tcl_EvalEx = getEvalEx wi
      tcl_GetStringResult = getGetStringResult wi
      tcl_CancelEval = getCancelEval wi
      tcl_AsyncInvoke = getAsyncInvoke wi
      i = getInterp wi
      autInfo = getSewageAutor s
      sewCmd = T.unpack $ getSewage s
      autDefNS = genericAutorToNSAutor autInfo
      sewNick = T.unpack $ getNick autDefNS
      sewMask = show $ getMask autDefNS
      sewChan = T.unpack $ getChannel autDefNS
  interp <- atomically $ takeTMVar i
  let runscript s = tcl_EvalEx interp s lEN_AUTO eVAL_FLAGS_CLEAR
      runTclCommand s = newCString s >>= runscript
      errorInfo = runTclCommand "return $errorInfo"
      
      doTheTCL c = runTclCommand c >>= \st ->
        case st of
          0 -> tcl_GetStringResult interp >>= \rs -> if nullPtr == rs then dumpDebug ("Command: " ++ c ++" ; returned a null pointer result.") >> return "FAILED" else peekCString rs >>= \nrs -> dumpDebug ("Output of command: " ++ c ++ " ;" ++ nrs ++ ";") >> return nrs
          _ -> errorInfo >> tcl_GetStringResult interp >>= peekCString
      performFromIRC = doTheTCL $ "pub:tcl:perform \"" ++ sewNick ++ "\" \"" ++ sewMask ++ "\" {} \"" ++ sewChan ++ "\" {" ++ sewCmd ++ "}"
--  harvester <- forkIO $ do
--    threadDelay 15000000
--    putStrLn "cancelling thread!!!"
--    fff <- tcl_CancelEval interp nullPtr nullPtr 0x100000
--    putStrLn $ "cancel status " ++ (show fff)
--    hngggg <- tcl_AsyncInvoke interp 0
--    putStrLn $ "asyncinvoke returned " ++ (show hngggg)
  res <- performFromIRC
--  putStrLn "putting back the interp"
  atomically $ putTMVar i interp
  return $ T.pack res

rEPL wrappedtclinterp manhole =
  let inspectManhole = atomically . readTChan . getInputChan
      regift g = atomically . (flip writeTChan g) . getOutputChan in
  forever $ do
    newGift <- inspectManhole manhole
    strippedCmd <- stripCommandLocal (getSewage newGift) manhole
    case strippedCmd of
      Just cmdBodyStripped -> do
        let giftStripped = Sewage (getSewageAutor newGift) cmdBodyStripped
        let hmm = gnarlyBalanced $ T.unpack cmdBodyStripped
        case hmm of
          Nothing -> do
            processedGift <- processCommand wrappedtclinterp giftStripped
            regift (Sewage mySignature processedGift) manhole
          Just berror -> regift (Sewage mySignature (T.pack berror)) manhole
      Nothing -> return ()

-- stolen from the internet and adapted for tcl
-- Return whether a string contains balanced brackets.  Nothing indicates a
-- balanced string, while (Just i) means an imbalance was found at, or just
-- after, the i'th bracket.  We assume the string contains only brackets.
isBalanced :: Char -> Char -> String -> Maybe String
isBalanced openc closec = bal (-1) 0
  where
    bal :: Int -> Int -> String -> Maybe String
    bal _ 0 [] = Nothing
    bal i _ [] = Just $ "Opening bracket unmatched until end of command." -- unmatched opening
    bal i (-1) _ = Just $ "Unmatched closing bracket at position " ++ show i -- unmatched close
    bal i n (singlec:bs)
      | singlec == openc = bal (i + 1) (n + 1) bs
      | singlec == closec = bal (i + 1) (n - 1) bs
      | singlec == '\\' = case bs of
          (sc:rs) -> if sc == openc || sc == closec then bal (i+2) n rs else bal (i+1) n rs 
      | otherwise = bal (i+1) n bs    

gnarlyBalanced = isBalanced '{' '}'
-- it's better not to check for double quotes and square brackets I think since they can be escaped and not used internally for pub:tcl:perform...

squareBalanced = isBalanced '[' ']'

dquoteBalanced = isBalanced '"' '"'
