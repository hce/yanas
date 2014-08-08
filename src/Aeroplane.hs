module Aeroplane where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Control.Monad.State.Strict as SM
import qualified Data.HashMap.Strict as Map
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Color
import qualified Graphics.UI.SDL.Image as Img
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.TTF.Management as TTFM
import Graphics.UI.SDL.TTF.Render as TTFR
import Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Types
import qualified Graphics.UI.SDL.Primitives as GFX

import Prelude hiding (flip)

import AVMisc
import Draw
import Types
import Network
import Movement

import Example


distributeCommands :: State -> ATCState -> IO State
distributeCommands state server = do
  commands <- getAllCommands server
  let newstate = foldl assignCmd state commands
--  when (newstate /= state) $ print $ stAirspace newstate
  return newstate
  where
    assignCmd :: State -> (Frequency, ATCCommand) -> State
    assignCmd s fc = s { stAirspace=map (assignToAC fc) (stAirspace s) }
    assignToAC :: (Frequency, ATCCommand) -> Element -> Element
    assignToAC (f,c) (AC aeroplane) = AC $
      if acfrequency aeroplane == f
      then aeroplane { acatccommands=acatccommands aeroplane ++ [c] }
      else aeroplane
    assignToAC _ element = element
    
acsay :: Aeroplane -> String -> Aeroplane
acsay a s = a { acatcresponses=responses++[s] }
  where
    responses = acatcresponses a
    
handleAirspace :: State -> (State, [(Frequency, String)])
handleAirspace s = (s { stAirspace=airspace'' }, concat responses)
  where
    airspace                 = stAirspace s
    airspace'                = map (handleAP s) airspace
    (airspace'', responses)  = unzip $ map handleAPResponses airspace'

handleAPResponses :: Element -> (Element, [(Frequency, String)])
handleAPResponses (AC aeroplane) = (AC aeroplane', responses)
  where
    aeroplane' = aeroplane { acatcresponses=[] }
    responses = map ((,) (acfrequency aeroplane)) $ acatcresponses aeroplane
handleAPResponses e = (e, [])

handleAP :: State -> Element -> Element
handleAP s (AC aeroplane) = AC $ handleAeroplane s aeroplane
handleAP _ (BC beacon)    = BC beacon
handleAP _ (RWY runway)   = RWY runway
handleAP _ (OBS obstacle) = OBS obstacle
handleAP _ (WP waypoint)  = WP waypoint
handleAP _ (Air airspace) = Air airspace

-- TODO: Handle validity and clearance limit!!
handleAeroplane :: State -> Aeroplane -> Aeroplane
handleAeroplane s a = a' { acatccommands=[] }
  where
    a' = foldl (handleCmd s) a commands
    commands = [cmd | cmd@(ACCmd {}) <- acatccommands a]

handleCmd :: State -> Aeroplane -> ATCCommand -> Aeroplane
handleCmd s theplane thecommand
  | applicable      = handleAeroplaneATCCommand s theplane $ cmdCommand thecommand
  | otherwise       = theplane
  where
    applicable = broadcast || callsignmatch || registrationmatch
    callsignmatch = accallsign theplane `elem` callsigns
    registrationmatch = acregistration theplane `elem` callsigns
    callsigns = cmdCallsign thecommand
    broadcast = cmdBroadcast thecommand
        

handleAeroplaneATCCommand :: State -> Aeroplane -> ACCommand -> Aeroplane
handleAeroplaneATCCommand _ a (Turn (TurnLeft (Heading h))) = a''
  where
    a' = a { acturnrate=(-180), acturnto=fromIntegral h }
    a'' = acsay a' $ "Left heading " ++ show h ++ " " ++ accallsign a'
handleAeroplaneATCCommand _ a (Turn (TurnRight (Heading h))) = a''
  where
    a' = a { acturnrate=180,    acturnto=fromIntegral h }
    a'' = acsay a' $ "Right heading " ++ show h ++ " " ++ accallsign a'
    
handleAeroplaneATCCommand s a (Climb climbto climbrate) = a''
  where
    a' = calcCOD s a True climbto climbrate
    a'' = acsay a' $ "Climb " ++ show climbto ++ " " ++ accallsign a'

handleAeroplaneATCCommand s a (Descend climbto climbrate) = a''
  where
    a' = calcCOD s a False climbto climbrate
    a'' = acsay a' $ "Down to " ++ show climbto ++ " " ++ accallsign a'
    
handleAeroplaneATCCommand s a (QNH qnh) = a''
  where
    a' = a { acqnh=qnh }
    a'' = acsay a' $ "QNH " ++ show qnh ++ " " ++ accallsign a'

calcCOD :: State -> Aeroplane -> Bool -> VPos -> Rate -> Aeroplane
calcCOD s a climb climbto climbrate = a'
  where
    a' = a { acvclearedaltitude=climbto,
             acvspeed=cod * fromIntegral clearedspd }
    cod = 1 -- Always 1, for both climb and descent!!
    maxclimbrate = acclimbrate a
    clearedspd = case climbrate of
      OwnRate -> maxclimbrate
      (Rate _ (Just OrMore)) -> maxclimbrate
      (Rate rate _) -> rate -- TODO: handle performance limits
      
