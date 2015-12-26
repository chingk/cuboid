module Graphics (initGL, draw) where

import FRP.Yampa
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

import Types
import Utils

-- Helpful OpenGL constants for rotation
xAxis = G.Vector3 1 0 0 :: G.Vector3 R
yAxis = G.Vector3 0 1 0 :: G.Vector3 R
zAxis = G.Vector3 0 0 1 :: G.Vector3 R

initGL :: IO ()
initGL = do
    getArgsAndInitialize
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
    createWindow "Cuboid!"
    depthFunc          $= Just Less
    clearColor         $= Color4 0 0 0 0
    light (Light 0)    $= Enabled
    lighting           $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    reshapeCallback    $= Just resizeScene
    return ()

-- Copied from reactive-glut
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  -- putStrLn "resizeScene"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (w2/h2) 1 1000
  matrixMode $= Modelview 0
 where
   w2 = half width
   h2 = half height
   half z = realToFrac z / 2

-- Rendering Code:

renderGame :: GameState -> IO ()
renderGame (Game l rotX rotY pPos) = do
    loadIdentity
    translate $ G.Vector3 (0 :: R) 0 (-2*(fromInteger $ size l))
    -- TODO: calculate rotation axis based on rotX/Y
    rotate (rotX * 10) xAxis --旋轉的角度繞x
    rotate (rotY * 10) yAxis
    color $ Color3 (1 :: R) 1 1 
    position (Light 0) $= Vertex4 0 0 0 1   --light打光,劃線的function也是＋在這邊
    renderObject Wireframe (Cube $ fromInteger $ size l)
    renderPlayer pPos
    renderGoal (p3DtoV3 $ endPoint l)
    mapM_ (renderObstacle . p3DtoV3) $ obstacles l
    (color red >>) .renderPrimitive Lines $ do
                              vertex $ Vertex3 (0.0::R) 0.0 0.0
                              vertex $ Vertex3 (3.0::R) 0.0 0.0 
    (color blue >>) .renderPrimitive Lines $ do
                              vertex $ Vertex3 (0.0::R) 0.0 0.0 
                              vertex $ Vertex3 (0.0::R) 3.0 0.0 

    (color green >>) .renderPrimitive Lines $ do
                              vertex $ Vertex3 (0.0::R) 0.0 0.0 
                              vertex $ Vertex3 (0.0::R) 0.0 3.0 
                              --vertex $ Vertex3 size2 0.0 0.0 
 

    where size2 :: R
          size2 = (fromInteger $ size l)/2
          green  = Color4 0.8 1.0 0.7 0.9 :: Color4 R
          greenG = Color4 0.8 1.0 0.7 1.0 :: Color4 R
          red    = Color4 1.0 0.7 0.8 1.0 :: Color4 R
          blue  = Color4 0.7 0.7 1.0 1.0 :: Color4 R
          renderShapeAt s p = preservingMatrix $ do
            translate $ G.Vector3 (0.5 - size2 + vector3X p)
                                  (0.5 - size2 + vector3Y p)
                                  (0.5 - size2 + vector3Z p)
            renderObject Solid s
          renderObstacle = (color green >>) . (renderShapeAt $ Cube 1)
          renderPlayer   = (color red >>) . (renderShapeAt $ Sphere' 0.5 20 20)
          renderGoal     = (color greenG >>) . (renderShapeAt $ Sphere' 0.5 20 20)
          
  

draw :: SF GameState (IO ())
draw = arr $ (\gs -> do
        clear [ ColorBuffer, DepthBuffer ]
        renderGame gs
        swapBuffers)
