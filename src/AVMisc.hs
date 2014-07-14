module AVMisc where


mkPos :: Double -> Double -> Double
mkPos deg min = deg + min / 60

mkPos' :: Double -> Double -> Double -> Double
mkPos' deg min sec = deg + min / 60 + sec / 3600

