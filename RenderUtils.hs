module RenderUtils
  ( translate'
  , rotate'
  , color'
  , vertex'
  , scale'
  ) where

import Linear.V2
import Graphics.UI.GLUT
import Simulation
import Control.Lens
import Foreign.C.Types (CFloat(..))


translate' :: V2 Float -> IO ()
translate' (V2 x y) = translate $ Vector3 (CFloat x) (CFloat y) 0

rotate' :: Direction -> IO ()
rotate' dir = rotate (CFloat $ dir ^. degrees) $ Vector3 0 0 1

color' :: Float -> Float -> Float -> IO ()
color' r g b = color $ Color4 (CFloat r) (CFloat g) (CFloat b) 1

vertex' :: V2 Float -> IO ()
vertex' (V2 x y) = vertex $ Vertex3 (CFloat x) (CFloat y) 0

scale' :: Float -> IO ()
scale' s = scale (CFloat s) (CFloat s) 1
