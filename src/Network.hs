module Network (
  ATCState,
  atcGetSockaddr,
  atcSay,
  atcServer,
  getAllCommands
  )
       where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Network.Socket
import System.IO
import System.Timeout
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim (runParserT, modifyState)
import Text.Parsec.String (GenParser, Parser)

import qualified Data.HashMap.Strict as Map

import Command
import Types

import Example

type CommandChannel = [(Frequency, TVar [ATCCommand])]

data ATCState = ATCState {
  atcFrequencies :: [(Frequency, Designation, Chan ATCCommand)],
  atcGetters :: [(Frequency, TVar [ATCCommand])],
  atcRecorders :: [ThreadId],
  atcSocket :: Maybe Socket
  }
                
data ATCHandlerState = ATCHandlerState {
  ahAtcState :: ATCState,
  ahLastACCallsign :: [String],
  ahHandle :: Handle,
  ahFreq :: Maybe (Chan ATCCommand, Frequency),  
  ahChanListener :: Maybe (ThreadId, TVar [ATCCommand]),
  ahQuit :: Bool
  }
                       
type ATCParser a = ParsecT String ATCHandlerState IO a

getCommandChannel :: ATCState -> CommandChannel
getCommandChannel = atcGetters

getAllCommands :: ATCState -> IO [(Frequency, ATCCommand)]
getAllCommands a = concat <$> mapM getCommands (atcGetters a)
  where
    getCommands :: (Frequency, TVar [ATCCommand]) -> IO [(Frequency, ATCCommand)]
    getCommands (f, v) = do
      commands <- getItems v
      return $ zip (repeat f) commands

means :: String -> a -> ATCParser a
means a b = string a >> return b

parseCallsign :: ATCParser String
parseCallsign = choice ["Cactus" `means` "AWE",
                        "Lufthansa" `means` "DLH",
                        "German Cargo" `means` "BOX"]
                
parseWaypoint :: ATCParser Waypoint
parseWaypoint = undefined
                
parseAC :: ATCParser String
parseAC = do
  accallsign <- choice [callsign, registration]
  modifyState (\s -> s {ahLastACCallsign=[accallsign]})
  return accallsign
  where 
    registration = do
      fst <- upper
      fol <- if fst == 'D'
             then replicateM 4 upper
             else many1 $ choice [digit, upper]
      return $ fst:fol
    callsign = do
      cs1 <- parseCallsign
      space
      cs2 <- many1 $ choice [digit, upper]
      return $ cs1 ++ cs2

atcInit :: IO ATCState
atcInit = do
  newChannels <- mapM genfreqchannel exampleFrequencies
  newChannels' <- mapM (\(a,b,chan) -> (dupChan chan >>= \c1 -> return (a,b,c1))) newChannels
  newGetters <- mapM (\(f,_,chan) -> (makeChanListener chan >>= \cl -> return (f, snd cl))) newChannels'
  recorders <- mapM (forkIO . recordATC) newChannels
  return ATCState {
    atcFrequencies=newChannels,
    atcGetters=newGetters,
    atcRecorders=recorders,
    atcSocket=Nothing
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
      
atcSay :: ATCState -> Frequency -> String -> IO ()
atcSay state freq text = do
  Just chan <- atcGetChannel' state freq
  writeChan chan (ATCText text)
      
atcGetChannel :: ATCState -> Frequency -> IO (Maybe (Chan ATCCommand))
atcGetChannel = _atcGetChannel dupChan

atcGetChannel' :: ATCState -> Frequency -> IO (Maybe (Chan ATCCommand))
atcGetChannel' = _atcGetChannel (return . id)

_atcGetChannel :: (Chan ATCCommand -> IO (Chan ATCCommand)) -> ATCState -> Frequency -> IO (Maybe (Chan ATCCommand))
_atcGetChannel f state freq
  | length chans == 1 = Just <$> (f . thr'3 . head) chans
  | otherwise         = return Nothing
  where
    chans = filter ((== freq) . fst'3) (atcFrequencies state)
    fst'3 (a,_,_) = a
    thr'3 (_,_,c) = c

atcGetSockaddr :: ATCState -> IO SockAddr
atcGetSockaddr s = do
  let Just socket = atcSocket s
  getSocketName socket
  
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
  return state { atcSocket=Just atcSock }
    
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
        ahQuit=False,
        ahLastACCallsign=[],
        ahChanListener=Nothing
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

atcPrintResponses :: Handle -> ATCHandlerState -> IO Bool
atcPrintResponses h cs =
  case ahChanListener cs of
    Just (_, chan) -> do
      cmds <- getItems chan
      let messages = [msg | msg@(ATCText {}) <- cmds]
      mapM_ (hPutStrLn h . ("\r  Freq | " ++) . cmdText) messages
      return True
    Nothing -> return False

atcMainLoop :: Handle -> ATCState -> ATCHandlerState -> IO ATCHandlerState
atcMainLoop h s cs = do
  let freq = case ahFreq cs of
        Just (_, freq) -> show freq
        Nothing        -> "NoFreq"
  -- threadDelay 1000000 -- hack
  atcPrintResponses h cs
  let prompt = "ATC [" ++ freq ++ "] > "
--  l <- atcGetLine 1000000 h prompt (atcPrintResponses h cs)
  hPutStr h prompt
  l <- hGetLine h
  maybeUtter (ahFreq cs) l
  res <- runParserT acmds cs "stdin" l
  case res of
    Left err -> hPrint h err >> return cs
    Right newstate -> return newstate
    
maybeUtter :: Maybe (Chan ATCCommand, Frequency) -> String -> IO ()
maybeUtter Nothing _ = return ()
maybeUtter (Just (chan, freq)) text = writeChan chan $ ATCText text

--acmds :: ATCHandlerState -> ATCParser
--acmds = choice . (zipWith (id) commands) . repeat
--  where
--    commands = [acmdF]
    
aPutStrLn :: String -> ATCParser ()
aPutStrLn s = do
  state <- getState
  lift $ hPutStrLn (ahHandle state) s

aPrint :: Show a => a -> ATCParser ()
aPrint = aPutStrLn . show

aSimpleCommand :: ACCommand -> ATCParser()
aSimpleCommand cmd = do
  acs <- ahLastACCallsign <$> getState
  freqinfo <- ahFreq <$> getState
  when (isNothing freqinfo) $ fail "Tune into a frequency first"
  let Just (chan, _) =  freqinfo
      cmd' = ACCmd {
        cmdCallsign=acs,
        cmdCondition=Nothing,
        cmdLimit=Nothing,
        cmdValidity=(Nothing,Nothing),
        cmdBroadcast=False,
        cmdCommand=cmd
        }
  lift $ writeChan chan cmd'
    
acmds :: ATCParser ATCHandlerState
acmds = choice (map try commands) >> getState
  where
    commands = [acmdF,
                acmdQuit,
                acCommands]
    acCommands = do
      parseAC -- call sign
      many1 $ do
        many1 $ string " "
        acCommand
      return ()
    
    acCommand = choice $ map try trueAtcCommands
    
trueAtcCommands = [acmdTurn, acmdClimbDescent, acmdQNH]

    
acmdQuit :: ATCParser ()
acmdQuit = do
  string "quit"
  modifyState (\s -> s {ahQuit=True})
  
acmdQNH :: ATCParser ()
acmdQNH = do
  choice [string "QNH", string "qnh"]
  space
  qnh <- read <$> many1 digit
  aSimpleCommand $ QNH qnh
  
acmdTurn :: ATCParser ()
acmdTurn = do
  (turndir, skipheading) <- (TurnOwnDiscretion, True) `option` do
    string "turn"
    space
    turndir' <- leftright
    space
    return (turndir', False)
  choice $ map (\a->a turndir) (direct : (if skipheading then [] else [heading]))
  where
    heading turndir = do
      string "heading"
      space
      dir <- read <$> replicateM 3 digit
      (aSimpleCommand . Turn . turndir . Heading) dir
    direct turndir = do
      string "direct"
      space
      wpnt <- parseWaypoint
      (aSimpleCommand . Turn . turndir . Direct) wpnt
    leftright = choice ["left" `means` TurnLeft,
                        "right" `means` TurnRight]
                
acmdClimbDescent :: ATCParser ()
acmdClimbDescent = do
  climbdescent <- choice ["climb" `means` Climb,
                          "descend" `means` Descend]
  space
  flightlevel <- False `option` ("flightlevel " `means` True)
  number <- read <$> many1 digit
  if flightlevel then return "" else string " feet"
  let vpos = (if flightlevel then Flightlevel else Altitude) number
  rate <- option OwnRate $ try $ do
    space
    number2 <- read <$> many1 digit
    space
    string "feet" >> space
    string "per" >> space
    string "minute"
    rateflag <- option Nothing $ do
      space
      string "or" >> space
      Just <$> choice ["greater" `means` OrMore,
                       "more" `means` OrMore,
                       "less" `means` OrLess]
    return $ Rate number2 rateflag
  aSimpleCommand $ climbdescent vpos rate
  
acmdF :: ATCParser ()
acmdF = do
  atcstate <- ahAtcState <$> getState
  string "f"
  space
  part1 <- replicateM 3 digit
  string "."
  part2 <- (:[]) <$> digit
  part3 <- "00" `option` sequence [digit, digit]
  let freq = part1 ++ "." ++ part2 ++ part3 ++ "MHz"
      freqi :: Int
      freqi = read part1 * 1000000 +
              read part2 * 100000 +
              read part3 * 1000
      freqchan = findFreq (atcFrequencies atcstate) (Frequency freqi)
  aPutStrLn $ "Setting your frequency to " ++ freq ++ "!"
  case freqchan of
    Just (chan, designation) -> do
      oldFreqListener <- ahChanListener <$> getState
      case oldFreqListener of
        Just (oldThreadId, _) -> lift $ killThread oldThreadId
        Nothing -> return ()
      chan' <- lift $ dupChan chan
      (threadId, freqListener) <- lift $ makeChanListener chan'
      modifyState (\s -> s { ahFreq=Just (chan, Frequency freqi),
                             ahChanListener=Just (threadId, freqListener) } )
      aPutStrLn $ "There. You are tuned into " ++ designation ++ " now."
    Nothing ->
      aPutStrLn "Frequency is not in use"
  where
    findFreq :: [(Frequency, Designation, Chan ATCCommand)] -> Frequency -> Maybe (Chan ATCCommand, Designation)
    findFreq ((f,d,c):fs) wf = if f == wf
                               then Just (c, d)
                               else findFreq fs wf
    findFreq [] _            = Nothing
