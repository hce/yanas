module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
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


docoInitScreen =
  setVideoMode 800 600 24 [DoubleBuf]

initState :: Surface -> Font -> [Element] -> IO State
initState screen mainfont airspace = do
  sfcs <- zip (map snd imgs) <$> mapM (Img.load . ("dist/resources/img/" ++) . fst) imgs
  return State {
    stScreen=screen,
    stScreenSize=(surfaceGetWidth screen, surfaceGetHeight screen),
    stMainfont=mainfont,
    stAirspace=airspace,
    stView=viewScreen,
    stSurfaces=Map.fromList sfcs
    }
    
  where 
--    viewScreen = (mkPos 8 20, mkPos 50 30, mkPos 8 55, mkPos 49 30)
    viewScreen = (mkPos 8 7, mkPos 50 20, mkPos 9 0, mkPos 49 40)
    imgs = [("ndb.gif", "ndb"),
            ("vor.gif", "vor"),
            ("crp.gif", "crp"),
            ("orrp.gif", "orrp")]

main :: IO ()
main = do  
  screen <- docoInitScreen
  TTFG.init
  mainfont <- openFont "dist/resources/FreeSans.ttf" 12
  server <- atcServer  
  state <- initState screen mainfont airspace
  calcit state server
  
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
    
handleAirspace :: State -> State
handleAirspace s = s { stAirspace=map handleAP (stAirspace s) }

handleAP :: Element -> Element
handleAP (AC aeroplane) = AC $ handleAeroplane aeroplane
handleAP (BC beacon)    = BC beacon
handleAP (RWY runway)   = RWY runway
handleAP (OBS obstacle) = OBS obstacle
handleAP (WP waypoint)  = WP waypoint
handleAP (Air airspace) = Air airspace

-- TODO: Handle validity and clearance limit!!
handleAeroplane :: Aeroplane -> Aeroplane
handleAeroplane a = a' { acatccommands=[] }
  where
    a' = foldl handleAeroplaneATCCommand a simpleCommands
    simpleCommands = map cmdCommand commands
    commands = acatccommands a

handleAeroplaneATCCommand :: Aeroplane -> ACCommand -> Aeroplane
handleAeroplaneATCCommand a (Turn (TurnLeft (Heading h))) =
  a { acturnrate=(-180), acturnto=fromIntegral h }
handleAeroplaneATCCommand a (Turn (TurnRight (Heading h))) =
  a { acturnrate=180,    acturnto=fromIntegral h }

calcit :: State -> ATCState -> IO ()
calcit state server = do
  fillRect screen (Just $ Rect 0 0 800 600) (Pixel 0)
  drawAirspace state
  flip screen
  delay 100
  state' <- distributeCommands state server
  let state'' = state' { stAirspace=moveAeroplanes 0.100 (stAirspace state') }
      state''' = handleAirspace state''
  calcit state''' server
  where
    screen = stScreen state

