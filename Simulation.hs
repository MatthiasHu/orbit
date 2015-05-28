{-# LANGUAGE TemplateHaskell #-}

module Simulation
  ( Direction
  , degrees
  , Sim(..)
  , cogPosition, craftPosition, craftHeading
  , sim0
  , Input(..)
  , keysDown
  , input0
  , physicsTick
  , inputApplication
  )
  where

import Linear.V2
import Linear.Metric
import Linear.Vector
import Control.Lens
import Control.Lens.TH
import Data.Set hiding (map)
import Graphics.UI.GLUT (Key(..), SpecialKey(..))


type Direction = Float -- in rad

degrees :: Iso' Direction Float
degrees = iso (\rad -> 180*rad/pi)
              (\deg -> pi*deg/180)

data Sim = Sim
  { _cogPosition :: V2 Float
  , _craftPosition :: V2 Float
  , _craftHeading :: Direction
  , _craftVelocity :: V2 Float
  }
  deriving (Eq, Ord, Show)
makeLenses ''Sim

sim0 :: Sim
sim0 = Sim
  { _cogPosition = V2 0 0
  , _craftPosition = V2 0.7 0
  , _craftHeading = 0
  , _craftVelocity = V2 0 0.003
  }


data Input = Input
  { _keysDown :: Set Key
  }
  deriving (Eq, Ord, Show)
makeLenses ''Input

input0 :: Input
input0 = Input empty


physicsTick :: Sim -> Sim
physicsTick sim = craftPosition %~ (+ sim^.craftVelocity) $ sim


inputApplication :: Input -> Sim -> Sim
inputApplication input sim =
    (craftVelocity %~ maybeAcc)
  . (craftHeading %~ maybeSteer)
  . gravity
    $ sim
  where
    maybeAcc = accelerate $ keysFactor forwardKey backwardKey
    accelerate sign = (+) ((angle $ sim ^. craftHeading)^*sign*thrust)
    maybeSteer = steer $ keysFactor ccwKey clockwiseKey
    steer sign = (+) (sign*angularSpeed)
    keysFactor :: (Num a) => Key -> Key -> a
    keysFactor a b = case keysdown a b of
                     (True, False) -> 1
                     (False, True) -> -1
                     _             -> 0
    
    keysdown a b = (a `member` (input^.keysDown)
                   ,b `member` (input^.keysDown))
    -- parameters
    thrust = 0.0001
    angularSpeed = 0.10
    -- key bindings
    forwardKey = SpecialKey KeyUp
    backwardKey = SpecialKey KeyDown
    ccwKey = SpecialKey KeyLeft
    clockwiseKey = SpecialKey KeyRight

gravity :: Sim -> Sim
gravity sim = craftVelocity %~ (+force) $ sim
  where force = scale (sim ^. cogPosition - sim ^. craftPosition)
        scale r = factor *^ r ^/ (norm r ** 3)
        -- parameter (mass of cog)
        factor = 0.00001
