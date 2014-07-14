module Movement where

import Types
import AVMisc

moveAC :: Double -> Aeroplane -> Aeroplane
moveAC dt ap = ap { aclon=lon, aclat=lat, actruealt=height }
  where
    height = actruealt ap + (acvspeed ap * dt / 60)
    angle = acheading ap * pi / 180
    acarcspeed = acspeed ap / 60 -- One knot == one "arc minute" per hour
    dx = acarcspeed * sin angle * dt / (3600 * dep)
    dy = acarcspeed * cos angle * dt / 3600
    lat = aclat ap + dy
    dep = cos depangle
    depangle = (aclat ap + dy / 2) * pi / 180
    lon = aclon ap + dx

moveAeroplanes :: Double -> [Element] -> [Element]
moveAeroplanes dt (AC aircraft:as) = AC (moveAC dt aircraft):moveAeroplanes dt as
moveAeroplanes dt (a:as) = a:moveAeroplanes dt as
moveAeroplanes _ []      = []

