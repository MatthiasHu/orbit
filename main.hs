
import Graphics.UI.GLUT
import Simulation as S
import Data.IORef
import RenderUtils
import Control.Lens
import Control.Concurrent (threadDelay)
import Linear.V2
import Data.Set (delete, insert)


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  createWindow "Orbit"
  glSetup
  simRef <- newIORef S.sim0
  inputRef <- newIORef S.input0
  keyboardMouseCallback $= Just (keyboardMouse inputRef)
  displayCallback       $= (get simRef >>= display)
  idleCallback          $= Just (get inputRef >>= idle simRef)
  mainLoop

glSetup :: IO ()
glSetup = do
  windowSize     $= Size 800 800
  windowPosition $= Position 0 0
  clearColor     $= Color4 0 0 0.2 1
  depthFunc      $= Just Always

idle :: IORef S.Sim -> Input -> IdleCallback
idle simRef input = do
  threadDelay 20000
  modifyIORef simRef $ S.physicsTick
                       . inputApplication input
  postRedisplay Nothing

display :: Sim -> DisplayCallback
display sim = do
  clear [ColorBuffer]
  -- render the centre of gravity
  loadIdentity
  translate' $ sim ^. S.cogPosition
  preservingMatrix renderCog
  -- render space craft
  loadIdentity
  translate' $ sim ^. S.craftPosition
  rotate'    $ sim ^. S.craftHeading
  preservingMatrix renderCraft
  flush

keyboardMouse :: IORef Input -> KeyboardMouseCallback
keyboardMouse inputRef k s m p = modifyIORef inputRef
                                   $ recordInput k s m p

recordInput :: Key -> KeyState -> Modifiers -> Position -> Input -> Input
recordInput key Up   _ _ = keysDown %~ delete key
recordInput key Down _ _ = keysDown %~ insert key


renderCog :: IO ()
renderCog = do
  color' 0.8 0.6 0.3
  scale' 0.10
  circle

renderCraft :: IO ()
renderCraft = do
  scale' 0.02
  color' 0.5 0.9 0.8
  circle
  color' 0.0 0.5 0.0
  arrow


circle :: IO ()
circle = renderPrimitive TriangleFan $ mapM_ vertex' vertices
  where vertices = V2 0.0 0.0 :
                   [V2 (sin $ i*pi*2/verts) (cos $ i*pi*2/verts)
                     | i<-[0..verts] ]
        verts = 20

arrow :: IO ()
arrow = renderPrimitive Triangles $ mapM_ vertex' arrowVertices

arrowVertices = map (\(x, y) -> V2 x y) $
  [( 1,  0 )
  ,( 0,-0.5)
  ,( 0, 0.5)
  ]
