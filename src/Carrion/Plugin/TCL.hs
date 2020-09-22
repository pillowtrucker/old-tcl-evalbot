{-# LANGUAGE ForeignFunctionInterface #-}

module Carrion.Plugin.TCL
    ( initPlugin,
      processCommand,
      testThing
    ) where
import Control.Monad
import Control.Concurrent(forkIO)
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

foreign import ccall "dynamic" mkTcl_CreateInterp :: FunPtr (IO Tcl_Interp_Ptr) -> IO (Tcl_Interp_Ptr)



foreign import ccall "&Tcl_InitStubs_wrap" tcl_InitStubs :: FunPtr (Tcl_Interp_Ptr -> TCL_Wanted_Version -> WantExact -> IO TCL_Actual_Version)
foreign import ccall "dynamic" mkTcl_InitStubs :: FunPtr (Tcl_Interp_Ptr -> TCL_Wanted_Version -> WantExact -> IO TCL_Actual_Version) -> (Tcl_Interp_Ptr -> TCL_Wanted_Version -> WantExact -> IO TCL_Actual_Version)
foreign import ccall "dynamic" mkTcl_FindExecutable :: FunPtr (CString -> IO CString) -> (CString -> IO CString)
foreign import ccall "dynamic" mkTcl_InitMemory :: FunPtr (Tcl_Interp_Ptr -> IO ()) -> (Tcl_Interp_Ptr -> IO ())
foreign import ccall "dynamic" mkTcl_Init :: FunPtr (Tcl_Interp_Ptr -> IO Int) -> (Tcl_Interp_Ptr -> IO Int)
foreign import ccall "dynamic" mkTcl_EvalFile :: FunPtr (Tcl_Interp_Ptr -> TclScriptFilename -> IO Int) -> (Tcl_Interp_Ptr -> TclScriptFilename -> IO Int)
foreign import ccall "dynamic" mkTcl_GetStringResult :: FunPtr (Tcl_Interp_Ptr -> IO CString) -> (Tcl_Interp_Ptr -> IO CString)
foreign import ccall "dynamic" mkTcl_EvalEx :: FunPtr (Tcl_Interp_Ptr -> TclScriptString -> TclScriptStringByteLen -> TclEvalFlags -> IO Int) -> (Tcl_Interp_Ptr -> TclScriptString -> TclScriptStringByteLen -> TclEvalFlags -> IO Int)
type Tcl_EvalFile_Sig = (Tcl_Interp_Ptr -> TclScriptFilename -> IO Int)
type Tcl_EvalEx_Sig = (Tcl_Interp_Ptr -> TclScriptString -> TclScriptStringByteLen -> TclEvalFlags -> IO Int)
type Tcl_GetStringResult_Sig = (Tcl_Interp_Ptr -> IO CString)
tu :: T.Text -> String
tu = T.unpack
tellCommands = map T.pack ["tcl"]
myPluginName = T.pack "TCL smeggdrop"
tl = T.pack "local"
mySignature = GenericStyleAutor myPluginName tl tl
stripCommandLocal c m = stripCommandPrefix' c tellCommands m mySignature
fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson
  :: SewageAutorInfo -> String -> TCLCommand
fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson b = case b of
  GenericStyleAutor a b c -> fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson . genericAutorToNSAutor $ GenericStyleAutor a b c
  NetworkIdentStyleAutor a b c -> TCLCommand (tu a) (show b) "" (tu c)


mkTCLCommandFromAIAndMsg :: SewageAutorInfo -> String -> TCLCommand
mkTCLCommandFromAIAndMsg = fuckingSewageAutorToFuckingTCLCommandFormatFuckYouSamStephenson

data TCLInterpreterWrapper = TCLInterpreterWrapper {getInterp :: Tcl_Interp_Ptr,
                                                    getEvalFile :: Tcl_EvalFile_Sig,
                                                    getEvalEx :: Tcl_EvalEx_Sig,
                                                    getGetStringResult :: Tcl_GetStringResult_Sig
                                                    
                                                   }

lEN_AUTO :: Int
lEN_AUTO = -1
eVAL_FLAGS_CLEAR :: Int
eVAL_FLAGS_CLEAR = 0
testThing :: IO ()
testThing = do
  myFakeArg0 <- getExecutablePath >>= newCString
  myTCLDl <- dlopen "/usr/lib/libtcl8.6.so" [RTLD_NOW]
  myFunTcl_CreateInterp <- dlsym myTCLDl "Tcl_CreateInterp"
  let tcl_CreateInterp = mkTcl_CreateInterp myFunTcl_CreateInterp 
  interp <- tcl_CreateInterp
  let tcl_InitStubs' = mkTcl_InitStubs tcl_InitStubs
  wanted_interp_version <- newCString "8.6"
  actual_version_c <- tcl_InitStubs' interp wanted_interp_version 0
  actual_version <- peekCString actual_version_c
  putStrLn actual_version
  myFunTcl_FindExecutable <- dlsym myTCLDl "Tcl_FindExecutable"
  let tcl_FindExecutable = mkTcl_FindExecutable myFunTcl_FindExecutable
  theComputedExecutablePath <- tcl_FindExecutable $ myFakeArg0
  if nullPtr == theComputedExecutablePath then
    putStrLn "Couldn't Tcl_FindExecutable()"
  else
    peekCString theComputedExecutablePath >>= putStrLn
  myFunTcl_InitMemory <- dlsym myTCLDl "Tcl_InitMemory"
  let tcl_InitMemory = mkTcl_InitMemory myFunTcl_InitMemory
  tcl_InitMemory interp
  myFunTcl_Init <- dlsym myTCLDl "Tcl_Init"
  let tcl_Init = mkTcl_Init myFunTcl_Init
  tcl_Init_status <- tcl_Init interp
  myFunTcl_EvalEx <- dlsym myTCLDl "Tcl_EvalEx"
  let tcl_EvalEx = mkTcl_EvalEx myFunTcl_EvalEx
  testScript <- newCString "set a [expr 2 + 2]; puts $a;"
  let runscript s = tcl_EvalEx interp s lEN_AUTO eVAL_FLAGS_CLEAR
  let runTclCommand s = newCString s >>= runscript
  testScriptStatus <- runscript testScript
  putStrLn $ show testScriptStatus
  newCString "puts \"test persistence [expr $a +2]\";" >>= runscript >>= putStrLn . show  
  let bless name convf = dlsym myTCLDl name >>= \fp -> return . convf $ fp
  tcl_GetStringResult <- bless "Tcl_GetStringResult" mkTcl_GetStringResult
  let errorInfo = runTclCommand "puts $errorInfo"
      doTheTCL c = runTclCommand c >>= \st ->
        case st of
          0 -> tcl_GetStringResult interp >>= \rs -> if nullPtr == rs then putStrLn ("Command: " ++ c ++" ; returned a null pointer result.") else peekCString rs >>= \nrs -> putStrLn ("Output of command: " ++ c ++ " ;" ++ nrs ++ ";")
          _ -> errorInfo>> return ()
      fakeFromIRC c = doTheTCL $ "return [pub:tcl:perform root test!test@test.org test #test {" ++ c ++ "}]"
  tcl_EvalFile <- bless "Tcl_EvalFile" mkTcl_EvalFile
  smeginitstatus <- newCString "/home/pszczola/Carrion-Plugin-TCL/src/smeggdrop/smeggdrop.tcl" >>= \fn -> tcl_EvalFile interp fn
  --newCString "puts $errorInfo;" >>= runscript >>= putStrLn . show
  errorInfo
  runTclCommand "puts $SMEGGDROP_ROOT"
  putStrLn $ show $ smeginitstatus
--  fakeFromIRC "proc testo4444 args {return \"booboo\n\"}"

dumpDebug = putStrLn

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
    smeginitstatus <- newCString "./src/smeggdrop/smeggdrop.tcl" >>= \fn -> tcl_EvalFile interp fn
    let wrappedinterp = TCLInterpreterWrapper interp tcl_EvalFile tcl_EvalEx tcl_GetStringResult
    forkIO $ rEPL wrappedinterp manhole
    return GoodInitStatus


processCommand wi s = do
  let tcl_EvalEx = getEvalEx wi
      tcl_GetStringResult = getGetStringResult wi
      interp = getInterp wi
      runscript s = tcl_EvalEx interp s lEN_AUTO eVAL_FLAGS_CLEAR
      runTclCommand s = newCString s >>= runscript
      errorInfo = runTclCommand "return $errorInfo"
      autInfo = getSewageAutor s
      sewCmd = T.unpack $ getSewage s
      autDefNS = genericAutorToNSAutor autInfo
      sewNick = T.unpack $ getNick autDefNS
      sewMask = show $ getMask autDefNS
      sewChan = T.unpack $ getChannel autDefNS
      doTheTCL c = runTclCommand c >>= \st ->
        case st of
          0 -> tcl_GetStringResult interp >>= \rs -> if nullPtr == rs then dumpDebug ("Command: " ++ c ++" ; returned a null pointer result.") >> return "FAILED" else peekCString rs >>= \nrs -> dumpDebug ("Output of command: " ++ c ++ " ;" ++ nrs ++ ";") >> return nrs
          _ -> errorInfo >> tcl_GetStringResult interp >>= peekCString
      performFromIRC = doTheTCL $ "return [pub:tcl:perform \"" ++ sewNick ++ "\" \"" ++ sewMask ++ "\" {} \"" ++ sewChan ++ "\" {" ++ sewCmd ++ "}]"
  performFromIRC

rEPL wrappedtclinterp manhole =
  let inspectManhole = atomically . readTChan . getInputChan
      regift g = atomically . (flip writeTChan g) . getOutputChan in
  forever $ do
    newGift <- inspectManhole manhole
    processedGift <- processCommand wrappedtclinterp newGift
    regift (Sewage (GenericStyleAutor (T.pack "TCL Intepreter") (T.pack "local") (T.pack "local")) (T.pack processedGift)) manhole
    return ()
