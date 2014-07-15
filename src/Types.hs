module Types where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Time
import Graphics.UI.SDL.TTF.Management as TTFM
import Graphics.UI.SDL.TTF.Render as TTFR
import Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Types
import qualified Data.HashMap.Strict as Map
import qualified Graphics.UI.SDL.Image as Img
import qualified Graphics.UI.SDL.Primitives as GFX

type LonLat = (Double, Double)

data State = State {
  stScreen :: Surface,
  stMainfont :: Font,
  stAirspace :: [Element],
  stView :: (Double, Double, Double, Double),
  stScreenSize :: (Int, Int),
  stSurfaces :: Map.HashMap String Surface
  }

data Squawk = SquawkA | SquawkC | SquawkS | SquawkIdent
            | Squawk Int | Reset Squawk
              deriving (Eq, Show)

data ACCategory = AC_A | AC_B | AC_C | AC_D | AC_E
                deriving (Eq, Ord, Show)

data ACPCategory = AP_A | AP_B | AP_C | AP_D
                 deriving (Eq, Ord, Show)

data ApchType = APCHVisual | APCHIFR | APCHVOR | APCHNDB |
                APCHCircleToLand
              deriving (Eq, Show)

data RateFlag = OrMore | OrLess
              deriving (Eq, Show)
data Rate = Rate Int (Maybe RateFlag)
          deriving (Eq, Show)

data TurnDirection = TurnLeft | TurnRight
                   deriving (Eq, Show)
                            
type Designation = String
data Frequency = Frequency Int
                 deriving (Eq)

data RelVPos = Below | Same | Above
             deriving (Eq, Show)
data RelMovement = LeftToRight | RightToLeft
                 deriving (Eq, Show)
data ACReport = InSight
              | Vacated
              deriving (Eq, Show)
data ACSay = Again
           | Altitude
           | Intentions
           | Position
           | Type
           deriving (Eq, Show) 

data ACCommand = Turn TurnDirection Int 
               | Climb Int Rate
               | Descend Int Rate
               | Speed Int
               | FinalApproachSpeed
               | OwnSpeed
               | Approach ApchType Runway
               | ClearedToLand Runway
               | ClearedForTakeoff Runway
               | QNH Int
               | ReportConditions
               | GoAround
               | Cancel ACCommand
               | MaintainOwnSeparation Aeroplane
               | LineupAndWait Runway
               | Cross Runway
               | Report ACReport
               | Say ACSay
               | Stop
               | ACSquawk [Squawk]
               | Traffic {
                 trafficWhat :: Aeroplane, 
                 trafficMilesAhead :: Int,
                 trafficHour :: Int,
                 trafficRelVPos :: RelVPos,
                 trafficMovement :: Maybe RelMovement
                 }
               | Contact Designation Frequency
               deriving (Eq, Show)

data ConditionalClearanceObservation = Landing | Departing | Crossing
                                     deriving (Eq, Show)
data ACCondition = WhicheverIsLater ACCondition ACCondition
                 | WhenAirbourne | WhenPassing Int | Overhead Element
                 | Behind ConditionalClearanceObservation Aeroplane
                 deriving (Eq, Show)
                   
newtype ZuluTime = ZuluTime Integer
                 deriving (Eq, Show)
                        
data ATCCommand = ACCmd {
  cmdCallsign   :: [String],
  cmdCondition  :: ACCondition,
  cmdCommand    :: ACCommand,
  cmdLimit      :: Maybe Element,
  cmdValidity   :: (Maybe ZuluTime, Maybe ZuluTime)
  } | ACBroadcast {
  cmdCommand    :: ACCommand
  }
                deriving (Eq, Show)

data Equipment = ETransponder | EVHF | EUHF | EADF
               deriving (Eq, Show)

data Aeroplane = Aeroplane {
  acregistration :: String,
  accallsign :: String,
  acicao :: String,
  actype :: String,
  accategory :: ACCategory,
  acpcategory :: ACPCategory,
  aclon :: Double,
  aclat :: Double,
  -- KTAS
  acspeed :: Double,
  -- Degrees *true*
  acheading :: Double,
  -- fpm
  acvspeed :: Double,
  -- *true* altitude AMSL!
  actruealt :: Double,
  actransponder :: ([Squawk], Int),
  acequipment :: [Equipment],
  acflightplan :: String,
  acfrequency :: Int,
  acatccommands :: [ATCCommand]
  }
                 deriving (Show, Eq)

data BeaconType = VOR | NDB | ILS | DME
                deriving (Show, Eq)
data Beacon = Beacon {
  bcntype :: BeaconType,
  bcnlon :: Double,
  bcnlat :: Double,
  bcnfreq :: Integer,
  bcnname :: String,
  bcnid :: String,
  bcnvar :: Double,
  bcninop :: Bool,
  bcnrange :: Int
  }
              deriving (Show, Eq)

data RWYSurface = ASPH | CONC | GRASS
                deriving (Show, Eq)

data Runway = Runway {
  rwylon :: Double,
  rwylat :: Double,
  rwyqfu :: Int,
  rwyvar :: Double,
  rwyelev :: Int,
  rwydesignation :: String,
  rwydisplacement :: Double,
  rwytora :: Int,
  rwylda :: Int,
  rwyasda :: Int,
  rwywidth :: Int,
  rwyals :: Bool,
  rwycenterline :: Bool,
  rwysurface :: RWYSurface,
  rwystrength :: Int,
  rwyad :: String
  }
            deriving (Show, Eq)
                     
data Obstacle = Obstacle {
  obslon :: Double,
  obslat :: Double,
  obsrad :: Double,
  obsmslelev :: Int,
  obsdesignation :: String
  }
                deriving (Show, Eq)

data Waypoint = VFRRP {
  vfrlon :: Double,
  vfrlat :: Double,
  vfrcompulsory :: Bool,
  vfrdesignation :: String,
  vfrdesignationletter :: Maybe Char,  
  vfrctr :: String}
                deriving (Show, Eq)
                         
data AirspaceClassification = AirspaceA | AirspaceB | AirspaceC |
                              AirspaceD | AirspaceE | AirspaceF |
                              AirspaceG | AirspaceEDR | AirspaceEDD
                            deriving (Show, Eq)
                                                                  
data AirspaceFlags = TMZ | CTR | RVSR
                   deriving (Show, Eq)
                         
data VerticalPosition = GND | FL Int | AMSLQNH Int |
                        BelowA AirspaceClassification |
                        AboveA AirspaceClassification
                      deriving (Show, Eq)
                               
data OperatingHours = H24 | HJ | HN | HS | HT | HX
                    deriving (Show, Eq)
                        
data Airspace = Airspace {
  airClassification :: AirspaceClassification,
  airFlags :: AirspaceFlags,
  airVBottom :: VerticalPosition,
  airVTop :: VerticalPosition,
  airActive :: OperatingHours,
  airPolygone :: [LonLat]
  }
                deriving (Show, Eq)

data Element = AC Aeroplane | BC Beacon | RWY Runway
             | OBS Obstacle | WP Waypoint | Air Airspace
             deriving (Show, Eq)


instance Show Frequency where
  show (Frequency f)
    | zeroes f >= 5   = show mhz ++ "." ++ show hkhz
    | zeroes f >= 3   = show mhz ++ "." ++ show khz
    | otherwise       = show f
    where
      mhz = f `quot` 1000000
      hkhz = f `mod` 1000000 `quot` 100000
      khz = f `mod` 1000000 `quot` 1000

zeroes n
  | (n `mod` 100000) == 0 = 5
  | (n `mod` 1000) == 0 = 3
  | otherwise = 0
  
           
           
                       
    