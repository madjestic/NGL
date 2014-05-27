{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module NGL.Rendering where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import NGL.LoadShaders
import NGL.Shape as N



data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

class DrawIn a where
    drawIn :: N.Color -> Window -> a -> IO ()
instance DrawIn Drawable where
    drawIn = draw
instance DrawIn [Drawable] where
    drawIn :: N.Color -> Window -> [Drawable] -> IO ()
    drawIn windowColor window ds = draw windowColor window (fromDrawables ds)
                       where
                          fromDrawables ds = (concat $ map fst ds, concat $ map snd ds)


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()


draw :: N.Color -> Window -> Drawable -> IO ()
draw clr window xs = do
    descriptor <- initResources xs
    onDisplay clr window descriptor

-- | Descriptor would have to be updated in responce to some events

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


initResources :: ([Color4 Float],[Vertex4 Float]) -> IO Descriptor
initResources (cs, vs) = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vertices = vs
        numVertices = length vertices
   
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled
    
    let rgba = cs
    
    colorBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just colorBuffer
    withArray rgba $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head rgba))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)    
    
    let firstIndex = 0
        vertexColor = AttribLocation 1
    vertexAttribPointer vertexColor $=
        (ToFloat, VertexArrayDescriptor 4 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vertexColor $= Enabled

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "NGL/Shaders/triangles.vert"),
        ShaderInfo FragmentShader (FileSource "NGL/Shaders/triangles.frac")]
    currentProgram $= Just program
    
    return $ Descriptor triangles firstIndex (fromIntegral numVertices)


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow window w h =
    do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


createWindow :: String -> (Int, Int) -> IO GLFW.Window
createWindow title (sizex,sizey) = do
    GLFW.init
    GLFW.defaultWindowHints
    Just window <- GLFW.createWindow sizex sizey title Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    GLFW.setWindowSizeCallback window (Just resizeWindow)
    return window


closeWindow :: GLFW.Window -> IO ()
closeWindow window = do
    GLFW.destroyWindow window
    GLFW.terminate


onDisplay :: N.Color -> GLFW.Window -> Descriptor -> IO ()
onDisplay clr window descriptor@(Descriptor triangles firstIndex numVertices) = do
  GL.clearColor $= getColor clr
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers window

  forever $ do
     GLFW.pollEvents
     onDisplay clr window descriptor

-- | Descriptor seems like a good candidate to pass the change to
