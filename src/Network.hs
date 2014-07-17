module Network where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans
import Network.Socket
import System.IO
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim (runParserT, modifyState)
import Text.Parsec.String (GenParser, Parser)

import qualified Data.HashMap.Strict as Map

import Types

import Example

data ATCState = ATCState {
  atcFrequencies :: [(Frequency, Designation, Chan ATCCommand)],
  atcRecorders :: [ThreadId]
  }
                
data ATCHandlerState = ATCHandlerState {
  ahAtcState :: ATCState,
  ahHandle :: Handle,
  ahFreq :: Maybe (Chan ATCCommand, Frequency),
  ahQuit :: Bool
  }
                       
type ATCParser = ParsecT String ATCHandlerState IO ()
                       
means :: String -> a -> Parser a
means a b = string a >> return b

callsigns :: Parser String
callsigns = choice ["Cactus" `means` "AWE",
                    "Lufthansa" `means` "DLH",
                    "German Cargo" `means` "BOX"]


atcInit :: IO ATCState
atcInit = do
  newChannels <- mapM genfreqchannel exampleFrequencies
  recorders <- mapM (forkIO . recordATC) newChannels
  return ATCState {
    atcFrequencies=newChannels,
    atcRecorders=recorders
    }
  where
    genfreqchannel :: (Frequency, Designation) -> IO (Frequency, Designation, Chan ATCCommand)
    genfreqchannel (f,d) = do
      chan <- newChan
      return (f,d,chan)
      
recordATC :: (Frequency, Designation, Chan ATCCommand) -> IO ()
recordATC (f,d,c) = do
  let fn = "log_" ++ show f ++ ".txt"
  withFile fn WriteMode $ \ h -> do
    hPutStrLn h $ "Creating log file for frequency " ++ show f ++ " (" ++ d ++ ")"
    hFlush h
    forever $ do
      command <- readChan c
      hPrint h (show command)
      hFlush h
      
atcGetChannel :: ATCState -> Frequency -> IO (Maybe (Chan ATCCommand))
atcGetChannel state freq
  | length chans == 1 = Just <$> (dupChan . thr'3 . head) chans
  | otherwise         = return Nothing
  where
    chans = filter ((== freq) . fst'3) (atcFrequencies state)
    fst'3 (a,_,_) = a
    thr'3 (_,_,c) = c

atcServer :: IO ATCState
atcServer = withSocketsDo $ do
  state <- atcInit
  atcSock <- socket AF_INET Stream 0
  setSocketOption atcSock ReuseAddr 1
  bindSocket atcSock (SockAddrInet 11990 iNADDR_ANY)
  listen atcSock 2
  forkIO $ forever $ do
    conn <- accept atcSock
    forkIO $ atcHandler conn state
  return state
    
atcHandler :: (Socket, SockAddr) -> ATCState -> IO ()
atcHandler (s, a) state = do
  h <- socketToHandle s ReadWriteMode
  hSetBuffering h NoBuffering
  hPutStrLn h "Welcome to HC's ATC simulator!"
  hPutStrLn h "Enter help for a list of commands."
  hPutStrLn h "----------------------------------------------------------------------\n"
  hPutStrLn h "To begin your shift, select a frequency!"
  hPutStrLn h "The command for that is: f <FREQ>"
  hPutStrLn h "<FREQ> must be in the form 118.0, 118.4 or 118.250."
  hPutStrLn h "The following frequencies are available:"
  mapM_ (\(f,d,_) -> hPutStrLn h $ "    " ++ show f ++ " | " ++ d) (atcFrequencies state)
  hPutStrLn h "======================================================================\n"
  
  let cs = ATCHandlerState {
        ahAtcState = state,
        ahHandle = h,
        ahFreq=Nothing,
        ahQuit=False
        }

  void $ foreverWithState cs (atcMainLoop h state)
  
  hPutStrLn h "Hope you had safe fun! Bye!"
  hClose h
  
  
foreverWithState :: ATCHandlerState -> (ATCHandlerState -> IO ATCHandlerState) -> IO ATCHandlerState
foreverWithState s f = do
  newstate <- f s
  if not (ahQuit newstate)
    then foreverWithState newstate f
    else return newstate

atcMainLoop :: Handle -> ATCState -> ATCHandlerState -> IO ATCHandlerState
atcMainLoop h s cs = do
  let freq = case ahFreq cs of
        Just (_, freq) -> show freq
        Nothing        -> "NoFreq"
  hPutStr h $ "ATC [" ++ freq ++ "] > "
  l <- hGetLine h
  res <- runParserT acmds cs "stdin" l
  case res of
    Left err -> hPrint h err >> return cs
    Right newstate -> return newstate
  
--acmds :: ATCHandlerState -> ATCParser
--acmds = choice . (zipWith (id) commands) . repeat
--  where
--    commands = [acmdF]
    
aPutStrLn :: String -> ATCParser
aPutStrLn s = do
  state <- getState
  lift $ hPutStrLn (ahHandle state) s

aPrint :: Show a => a -> ATCParser
aPrint = aPutStrLn . show
    
acmds :: ParsecT String ATCHandlerState IO ATCHandlerState
acmds = choice commands >> getState
  where
    commands = [acmdF,
                acmdQuit]
    
acmdQuit :: ATCParser
acmdQuit = do
  string "quit"
  modifyState (\s -> s {ahQuit=True})

acmdF :: ATCParser
acmdF = do
  atcstate <- ahAtcState <$> getState
  string "f"
  spaces
  part1 <- sequence $ take 3 $ repeat digit
  string "."
  part2 <- (:[]) <$> digit
  part3 <- "00" `option` sequence [digit, digit]
  let freq = part1 ++ "." ++ part2 ++ part3 ++ "MHz"
      freqi :: Int
      freqi = (read part1) * 1000000 +
              (read part2) * 100000 +
              (read part3) * 1000
      freqchan = findFreq (atcFrequencies atcstate) (Frequency freqi)
  aPutStrLn $ "Setting your frequency to " ++ freq ++ "!"
  case freqchan of
    Just (chan, designation) -> do
      modifyState (\s -> s {ahFreq=Just (chan, Frequency freqi)} )
      aPutStrLn $ "There. You are tuned into " ++ designation ++ " now."
    Nothing ->
      aPutStrLn "Frequency is not in use"
  where
    findFreq :: [(Frequency, Designation, Chan ATCCommand)] -> Frequency -> Maybe (Chan ATCCommand, Designation)
    findFreq ((f,d,c):fs) wf = if f == wf
                               then Just (c, d)
                               else findFreq fs wf
    findFreq [] _            = Nothing
