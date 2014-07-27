module Draw where

import Control.Applicative
import Control.Arrow
import Control.Monad
import GHC.Int
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

import Types
import AVMisc

coordToScreen :: Integral n => State -> LonLat -> (n, n)
coordToScreen  s (plon, plat) = (x, y)
  where
    (gl, gt, gr, gb) = stView s
    (width, height) = stScreenSize s
    gw = gr - gl
    gh = gt - gb
    aprellon = plon - gl
    aprellat = plat - gb
    griddep = cos $ (gt+gh/2) * pi / 180
    x = truncate $ aprellon * fromIntegral width / gw
    y = truncate $ (gh - aprellat) * fromIntegral height / gh

acScreenPos :: State -> Aeroplane -> (Int, Int)
acScreenPos s ap = coordToScreen s (aclon ap, aclat ap)

putPixel :: Surface -> Pixel -> (Int, Int) -> IO Bool
putPixel s c (x, y) = fillRect s (Just r) c
  where
    r = Rect {rectX=x, rectY=y, rectW=1, rectH=1}
    
drawCircle :: Surface -> (Int, Int) -> Int -> Pixel -> IO ()
drawCircle s (x, y) r c =
  void (mapM_ (draw . fromIntegral) [0, 1..359])
  where
    tr :: Double
    tr = fromIntegral r
    draw :: Double -> IO Bool
    draw a = putPixel s c (x + sin' a, y + cos' a)
    sin' :: Double -> Int
    sin' a = truncate (sin $ a * pi / 180 * tr)
    cos' :: Double -> Int
    cos' a = truncate (cos $ a * pi / 180 * tr)

printText :: State -> (Int, Int) -> Color -> [String] -> IO ()
printText s (x,y) c lines = do
  fntsfcs <- mapM (\line -> renderTextSolid fnt line c) lines
  let maxheight = maximum $ map surfaceGetHeight fntsfcs
      ypos = [y, y+maxheight..]      
  mapM_ (\(ly,s) -> blitSurface s Nothing sfc
            (Just Rect {rectX=x, rectY=ly,
                        rectW=0, rectH=0} )) $ zip ypos fntsfcs
  where
    sfc = stScreen s
    fnt = stMainfont s
    
drawPicture :: State -> (Int, Int) -> String -> IO Bool
drawPicture state (x,y) img = blitSurface s Nothing (stScreen state)
                              (Just Rect {rectX=cx, rectY=cy,
                                          rectW=0, rectH=0})
  where
    Just s = Map.lookup img (stSurfaces state)
    width = surfaceGetWidth s
    height = surfaceGetHeight s
    cx = x - (width `quot` 2)
    cy = y - (height `quot` 2)
  
drawAC :: State -> Aeroplane -> IO ()
drawAC state ap = do
  fillRect sc (Just Rect {rectX=acX-2, rectY=acY-2,
                          rectW=4, rectH=4}) (Pixel 255)
  printText state (acX+10,acY-2) (Color 255 255 255) str
  return ()
  where
    sc = stScreen state
    (acX, acY) = acScreenPos state ap
    str = [accallsign ap,
           actype ap ++ " FL" ++ show (truncate $ actruealt ap / 100),
           "V" ++ show (truncate $ acvspeed ap) ++ " S" ++ show squawkcode,
           (show . truncate . acspeed $ ap) ++ "KTAS",
           show . acvclearedaltitude $ ap]
    (squawkmodes, squawkcode) = actransponder ap
    
drawBC :: State -> Beacon -> IO ()
drawBC state bcn =  do
  fillRect sc (Just Rect {rectX=bcnX-2, rectY=bcnY-2,
                          rectW=4, rectH=4}) (Pixel 65535)
  printText state (bcnX+10, bcnY-2) (Color 200 200 200) str
  when (bcntype bcn == VOR) $ do
    drawCircle sc (bcnX, bcnY) 60 (Pixel 65535)
    GFX.polygon sc (map (fromIntegral Control.Arrow.*** fromIntegral)
                    [(bcnX-10, bcnY+5),
                     (bcnX-10, bcnY-5),
                     (bcnX-5, bcnY-10),
                     (bcnX+5, bcnY-10),
                     (bcnX+10, bcnY-5),
                     (bcnX+10, bcnY+5),
                     (bcnX+5, bcnY+10),
                     (bcnX-5, bcnY+10)]) (Pixel 65535)
    return ()
  return ()
  where
    sc = stScreen state
    (bcnX, bcnY) = coordToScreen state (bcnlon bcn, bcnlat bcn)
    str = [bcnid bcn, freq]
    rawfreq = bcnfreq bcn
    freq = case bcntype bcn of
      VOR -> show (rawfreq `quot` 1000000) ++ "." ++
             show (rawfreq `quot` 10000 `mod` 100)
      NDB -> show rawfreq
      ILS -> show rawfreq
      DME -> show rawfreq

drawRWY :: State -> Runway -> IO ()
drawRWY = undefined

drawOBS :: State -> Obstacle -> IO ()
drawOBS = undefined

drawWP :: State -> Waypoint -> IO ()
drawWP s (VFRRP lon lat comp dsg dsgl ctr) = do
  drawPicture s coords (if comp then "crp" else "orrp")  
  printText s coords (Color 255 0 0) [dsg]
  return ()
  where
    coords = coordToScreen s (lon, lat)
    
drawAir :: State -> Airspace -> IO Bool
drawAir s spc = GFX.polygon screen coords color
  where
    screen = stScreen s
    coords = map (coordToScreen s) poly
    color = Pixel (255 + 255 * 256 + 255 * 65536)
    poly = airPolygone spc

drawAirspace :: State -> IO ()
drawAirspace state = mapM_ drawAirspaceElement aspc
  where
    aspc = stAirspace state
    drawAirspaceElement :: Element -> IO ()
    drawAirspaceElement elem = case elem of
      AC aircraft   -> drawAC   state aircraft
      BC beacon     -> drawBC   state beacon
      RWY runway    -> drawRWY  state runway
      OBS obstacle  -> drawOBS  state obstacle
      WP waypoint   -> drawWP   state waypoint
      Air airspace  -> void (drawAir  state airspace)
