module Movement where

import Types
import AVMisc

moveAC :: Double -> Aeroplane -> Aeroplane
moveAC dt ap = ap { aclon=lon, aclat=lat, actruealt=height, acheading=newheading, acturnrate=newturnrate, acvspeed=newacvspeed }
  where
    height = actruealt ap + (acvspeed ap * dt / 60)
    newacvspeed = if (abs (fromIntegral $ acvclearedaltitude ap) - height) < 50 then 0 else acvspeed ap
    angle = acheading ap * pi / 180
    acarcspeed = acspeed ap / 60 -- One knot == one "arc minute" per hour
    dx = acarcspeed * sin angle * dt / (3600 * dep)
    dy = acarcspeed * cos angle * dt / 3600
    lat = aclat ap + dy
    dep = cos depangle
    depangle = (aclat ap + dy / 2) * pi / 180
    lon = aclon ap + dx
    turnrate = acturnrate ap
    newturnrate = if truncate (acturnto ap) == truncate (acheading ap)
                  then 0
                  else turnrate
    curheading = acheading ap
    newheading = normaliseHeading $ curheading + acturnrate ap / 60 * dt

moveAeroplanes :: Double -> [Element] -> [Element]
moveAeroplanes dt (AC aircraft:as) = AC (moveAC dt aircraft):moveAeroplanes dt as
moveAeroplanes dt (a:as) = a:moveAeroplanes dt as
moveAeroplanes _ []      = []

normaliseHeading :: (Num a, Ord a) => a -> a
normaliseHeading heading
  | heading > 360    = normaliseHeading $ heading - 360
  | heading < 0      = normaliseHeading $ heading + 360
  | otherwise        = heading
                       
qnhtofl :: Int -> Int -> Int
qnhtofl qnh alt = pressalt `quot` 100
  where
    pressalt = alt + 27 * qnhdiff
    qnhdiff = qnh - 1013
    
fltoqnh :: Int -> Int -> Int
fltoqnh qnh fl = fl * 100 - 27 * qnhdiff
  where
    qnhdiff = qnh - 1013

qnhtotrue :: Int -> Int -> Int
qnhtotrue temp alt = alt + round altCorrection                     
  where
    altCorrection = fromIntegral alt * 0.04 * tempdiff / 10
    tempdiff = fromIntegral temp - stdtemp alt
    
fltotrue :: Int -> Int -> Int -> Int
fltotrue temp qnh fl = truealt
  where
    truealt = qnhtotrue temp qnhalt
    qnhalt  = fltoqnh qnh fl

vpostoqnh :: Int -> VPos -> Int
vpostoqnh qnh (Flightlevel fl)     = fltoqnh qnh fl
vpostoqnh _   (Altitude alt)       = alt 

vpostotrue :: Int -> Int -> VPos -> Int
vpostotrue temp qnh (Flightlevel fl) = fltotrue temp qnh fl
vpostotrue temp _   (Altitude alt)   = qnhtotrue temp alt

truetofl :: Int -> Int -> Int -> Int
truetofl temp qnh talt = qnhtofl qnh qnhalt
  where
    qnhalt = round $ fromIntegral talt / altCorrection
    altCorrection = 1 + (tempdiff * 0.04 / 10)
    tempdiff = fromIntegral temp - stdtemp talt

stdtemp :: Int -> Double
stdtemp truealt = 15 - (fromIntegral truealt * 0.65 / 328.084)
