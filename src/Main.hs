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

import Aeroplane
import AVMisc
import Draw
import Types
import Network
import Movement

import Example


docoInitScreen =
  setVideoMode 800 600 24 [DoubleBuf]

initState :: Surface -> Font -> [Element] -> IO YanasState
initState screen mainfont airspace = do
  sfcs <- zip (map snd imgs) <$> mapM (Img.load . ("dist/resources/img/" ++) . fst) imgs
  return YanasState {
    stScreen=screen,
    stScreenSize=(surfaceGetWidth screen, surfaceGetHeight screen),
    stMainfont=mainfont,
    stAirspace=airspace,
    stView=viewScreen,
    stSurfaces=Map.fromList sfcs,
    stSurfaceTemp=30,
    stGndElev=380,
    stQNH=1029 -- I like a high QNH :)
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
  

drawHostInfo state server = do
  sockaddr <- atcGetSockaddr server
  let text = "Listening on " ++ show sockaddr ++ " for ATC commands!"
  printText state (surfaceGetWidth screen,4) (Color 255 255 255) AlignRight [text]
  where
    screen = stScreen state

calcit :: YanasState -> ATCState -> IO ()
calcit state server = do
  fillRect screen (Just $ Rect 0 0 800 600) (Pixel 0)
  drawAirspace state
  drawHostInfo state server
  flip screen
  delay 25
  state' <- distributeCommands state server
  let state'' = state' { stAirspace=moveAeroplanes (stQNH state) 0.100 (stAirspace state') }
      (state''', responses) = handleAirspace state''
  mapM_ (uncurry (atcSay server)) responses
  calcit state''' server
  where
    screen = stScreen state

