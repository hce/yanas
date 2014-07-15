module Network where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Network.Socket
import System.IO
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import qualified Data.HashMap.Strict as Map

import Types

import Example


data ATCState = ATCState {
  atcFrequencies :: [(Frequency, Designation, Chan ATCCommand)],
  atcRecorders :: [ThreadId]
  }
                
data ATCHandlerState = ATCHandlerState {
  ahFreq :: Maybe (Chan ATCCommand, Frequency),
  ahQuit :: Bool
  }
                
-- means :: String -> a -> Parsec a
-- means a b = string a >> return b
-- callsigns :: Parsec String
-- callsigns = choice ["Cactus" `means` "AWE",
--                    "Lufthansa" `means` "DLH",
--                    "German Cargo" `means` "BOX"]

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
  hPutStrLn h "The following frequencies are available:"
  mapM_ (\(f,d,_) -> hPutStrLn h $ "    " ++ show f ++ " | " ++ d) (atcFrequencies state)
  hPutStrLn h "======================================================================\n"
  
  let cs = ATCHandlerState {
        ahFreq=Nothing,
        ahQuit=False
        }

  void $ foreverWithState cs (atcMainLoop h state)
  
  hPutStrLn h "Hope you hade safe fun! Bye!"
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
  hPutStrLn h $ "You said " ++ show l
  return $ case l of
    "quit" -> cs { ahQuit=True }
    otherwise -> cs
  
  

  