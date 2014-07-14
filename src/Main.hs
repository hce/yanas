module Main where

import Control.Applicative
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
import Example
import Movement

docoInitScreen = do
  setVideoMode 800 600 24 [DoubleBuf]

initState :: Surface -> Font -> [Element] -> IO State
initState screen mainfont airspace = do
  sfcs <- (zip (map snd imgs)) <$> mapM (Img.load . ((++) "dist/resources/img/") . fst) imgs
  return $ State {
    stScreen=screen,
    stScreenSize=(surfaceGetWidth screen, surfaceGetHeight screen),
    stMainfont=mainfont,
    stAirspace=airspace,
    stView = viewScreen,
    stSurfaces=Map.fromList sfcs
    }
    
  where 
    viewScreen = (mkPos 8 20, mkPos 50 30, mkPos 8 55, mkPos 49 30)
    imgs = [("ndb.gif", "ndb"),
            ("vor.gif", "vor"),
            ("crp.gif", "crp"),
            ("orrp.gif", "orrp")]

main :: IO ()
main = do
  screen <- docoInitScreen
  TTFG.init
  mainfont <- openFont "dist/resources/FreeSans.ttf" 12
  state <- initState screen mainfont airspace
  calcit state 600

calcit state count = when (count > 0) $ do
  fillRect screen (Just $ Rect 0 0 800 600) (Pixel 0)
  drawAirspace state
  flip screen
  delay 100
  calcit state {stAirspace=(moveAeroplanes 0.100 aspc)} (count - 1)
  where
    aspc = stAirspace state
    screen = stScreen state
