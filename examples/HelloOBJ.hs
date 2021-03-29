{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Monad             (forM, unless, (>=>))
import qualified Data.ByteString           as SB
import           Data.List                 (groupBy, nub)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Text                 (Text, unpack)
import qualified Data.Vector               as V
import           "GLFW-b" Graphics.UI.GLFW as GLFW
import           System.Environment        (getArgs)

import           LambdaCube.GL             as LambdaCubeGL
import           LambdaCube.GL.Mesh        as LambdaCubeGL

import           Codec.Picture             as Juicy (DynamicImage (ImageRGB8),
                                                     PixelRGB8 (PixelRGB8),
                                                     generateImage, readImage)
import           Codec.Wavefront
import           Data.Aeson                (decodeStrict)

import           LambdaCube.MtlParser      (MtlLib, ObjMaterial (..), readMtl)
import           LambdaCube.OBJ            (addOBJToObjectArray, loadOBJToGPU,
                                            uploadMtlLib)

----------------------------------------------------
--  See:  http://lambdacube3d.com/getting-started
----------------------------------------------------

main :: IO ()
main = do
    Just pipelineDesc <- decodeStrict <$> SB.readFile "examples/hello_obj.json"

    win <- initWindow "LambdaCube 3D DSL OBJ viewer" 640 640

    -- setup render data
    let inputSchema = makeSchema $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V4F
            "normal"    @: Attribute_V3F
            "uvw"       @: Attribute_V3F
          defUniforms $ do
            "time"            @: Float
            "diffuseTexture"  @: FTexture2D
            "diffuseColor"    @: V4F

    storage <- LambdaCubeGL.allocStorage inputSchema

    objName <- head . (++ ["examples/cube.obj"]) <$> getArgs
    -- load OBJ geometry and material descriptions
    (objMesh,mtlLib) <- loadOBJToGPU objName >>= \case
        Right ok -> return ok
        Left err -> fail err
    -- load materials textures
    gpuMtlLib <- uploadMtlLib mtlLib
    -- add OBJ to pipeline input
    addOBJToObjectArray storage "objects" objMesh gpuMtlLib

    -- allocate GL pipeline
    renderer <- LambdaCubeGL.allocRenderer pipelineDesc
    LambdaCubeGL.setStorage renderer storage >>= \case -- check schema compatibility
      Just err -> putStrLn err
      Nothing  -> loop
        where loop = do
                -- update graphics input
                GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
                LambdaCubeGL.updateUniforms storage $ do
                  "time" @= do
                              Just t <- GLFW.getTime
                              return (realToFrac t :: Float)
                -- render
                LambdaCubeGL.renderFrame renderer
                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = (==KeyState'Pressed) <$> GLFW.getKey win k
                escape <- keyIsPressed Key'Escape
                unless escape loop

    LambdaCubeGL.disposeRenderer renderer
    LambdaCubeGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win
