{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
module LambdaCNC.Viewer where

import qualified Codec.Picture             as Pic
import           Control.Monad             (unless)
import           Data.Aeson.Encode.Pretty  (encodePretty)
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map                  as Map
import           Data.Vect                 ((&-))
import qualified Data.Vect                 as Vect
import qualified Data.Vector               as V
import           Graphics.Formats.STL      (STL (..))
import qualified Graphics.Formats.STL      as STL
import           "GLFW-b" Graphics.UI.GLFW (Key (..), KeyState (..),
                                            OpenGLProfile (..), WindowHint (..))
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import           System.Directory          (getModificationTime)

import qualified LambdaCube.Compiler       as L (Backend (OpenGL33),
                                                 compileMain, ppShow)
import           LambdaCube.GL             as LGL (FetchPrimitive (Triangles),
                                                   GLRenderer, GLStorage,
                                                   InputType (FTexture2D, Float, V3F),
                                                   Object,
                                                   StreamType (Attribute_V3F),
                                                   TextureData, V3 (V3), (@:),
                                                   (@=))
import qualified LambdaCube.GL             as LGL
import           LambdaCube.GL.Mesh        as LGL (Mesh (..),
                                                   MeshAttribute (A_V3F),
                                                   MeshPrimitive (P_Triangles))
import qualified LambdaCube.GL.Mesh        as LGL

lcFile :: FilePath
lcFile = "data/engine/lambdacnc.lc"

loadRenderer :: GLStorage -> IO (Maybe GLRenderer)
loadRenderer storage = do
    L.compileMain ["."] L.OpenGL33 lcFile >>= \case
      Left err  -> do
        putStrLn $ "compile error:\n" ++ L.ppShow err
        return Nothing
      Right pipelineDesc -> do
        let json = encodePretty pipelineDesc
        LBS.writeFile "data/engine/lambdacnc.json" json
        renderer <- LGL.allocRenderer pipelineDesc
        LGL.setStorage renderer storage >>= \case -- check schema compatibility
          Just err -> do
            putStrLn $ "setStorage error: " ++ err
            LGL.disposeRenderer renderer
            return Nothing
          Nothing -> do
            putStrLn "setStorage ok"
            return $ Just renderer


data Machine = Machine
    { bed   :: LGL.Object
    , xaxis :: LGL.Object
    , yaxis :: LGL.Object
    , zaxis :: LGL.Object
    }


data MachinePosition = MachinePosition
    { xPos :: Int
    , yPos :: Int
    , zPos :: Int
    }


xMax, yMax, zMax :: Int
(xMax, yMax, zMax) = (40000, 61500, 5600)

fps :: Double
fps = 24

mainLoop :: GLFW.Window -> GLStorage -> TextureData -> Machine -> GLRenderer -> IO ()
mainLoop win storage textureData Machine{..} r = lcModificationTime >>= loop r startPos
  where
    startPos = MachinePosition 0 0 0

    lcModificationTime = getModificationTime lcFile

    loop renderer0 pos0 lcTime0 = do
        -- Possibly reload the rendering pipeline.
        lcTime <- lcModificationTime
        let reload = lcTime /= lcTime0
        renderer <- if not reload then return renderer0 else do
          putStrLn "compiling renderer"
          loadRenderer storage >>= \case
            Nothing -> return renderer0
            Just newRenderer -> do
              putStrLn "reloading renderer"
              LGL.disposeRenderer renderer0
              return newRenderer

        -- Update machine position.
        pos@MachinePosition{..} <- reposition pos0

        -- Update graphics input.
        GLFW.getWindowSize win >>= \(w,h) ->
            LGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
        LGL.updateUniforms storage $ do
          "diffuseTexture" @= return textureData
          "time" @= do
              Just t <- GLFW.getTime
              return (realToFrac t :: Float)

        -- Render machine in the current position.
        let x = toFloat (xPos - (xMax `div` 2))
            y = toFloat (yPos - (yMax `div` 2))
            z = toFloat (zPos - (zMax `div` 2))
        LGL.updateObjectUniforms xaxis $ do
          "position" @= return (V3 x (y + 4000)      24500 )
        LGL.updateObjectUniforms yaxis $ do
          "position" @= return (V3 0  y               5000 )
        LGL.updateObjectUniforms zaxis $ do
          "position" @= return (V3 x (y - 4000) (z + 24500))

        -- render
        LGL.renderFrame renderer
        GLFW.swapBuffers win
        GLFW.waitEventsTimeout (1/fps)

        escape <- keyIsPressed Key'Escape
        unless escape $ loop renderer pos lcTime

    keyIsPressed :: Key -> IO Bool
    keyIsPressed k = (==KeyState'Pressed) <$> GLFW.getKey win k

    reposition :: MachinePosition -> IO MachinePosition
    reposition pos@MachinePosition{..} = do
      lr <- moveMult <$> keyIsPressed Key'Left <*> keyIsPressed Key'Right
      fb <- moveMult <$> keyIsPressed Key'Down <*> keyIsPressed Key'Up
      ud <- moveMult <$> keyIsPressed Key'PageDown <*> keyIsPressed Key'PageUp
      return pos { xPos = max 0 . min xMax $ (xPos + (xMax `div` 50) * lr)
                 , yPos = max 0 . min yMax $ (yPos + (yMax `div` 50) * fb)
                 , zPos = max 0 . min zMax $ (zPos + (zMax `div` 50) * ud)
                 }


moveMult :: Bool -> Bool -> Int
moveMult True _      = -1
moveMult _ True      = 1
moveMult False False = 0


toFloat :: Int -> Float
toFloat = fromIntegral


uploadModel :: GLStorage -> FilePath -> IO Object
uploadModel storage path = do
    putStrLn $ "Loading model: " ++ path
    STL.mustLoadSTL path
        >>= LGL.uploadMeshToGPU . stlToMesh
        >>= LGL.addMeshToObjectArray storage "objects" ["position"]


main :: IO ()
main = do
    win <- initWindow "LambdaCNC" 960 540

    -- setup render data
    let inputSchema = LGL.makeSchema $ do
          LGL.defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V3F
            "normal"    @: Attribute_V3F
          LGL.defUniforms $ do
            "time"           @: Float
            "position"       @: V3F
            "diffuseTexture" @: FTexture2D

    storage <- LGL.allocStorage inputSchema

    -- upload geometry to GPU and add to pipeline input
    mach@Machine{..} <- Machine
        <$> uploadModel storage "data/models/Bed.stl"
        <*> uploadModel storage "data/models/XAxis.stl"
        <*> uploadModel storage "data/models/YAxis.stl"
        <*> uploadModel storage "data/models/ZAxis.stl"

    LGL.enableObject bed True
    LGL.enableObject yaxis True
    LGL.enableObject xaxis True
    LGL.enableObject zaxis True

    -- load image and upload texture
    Right img <- Pic.readImage "examples/logo.png"
    textureData <- LGL.uploadTexture2DToGPU img

    -- compile hello.lc to graphics pipeline description
    loadRenderer storage >>= \case
      Nothing -> return ()
      Just renderer -> do
        mainLoop win storage textureData mach renderer
        LGL.disposeRenderer renderer

    LGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate


-- geometry data: triangles
stlToMesh :: STL -> LGL.Mesh
stlToMesh STL{triangles} = Mesh
    { mAttributes = Map.fromList
        [ ("position",  A_V3F $ V.fromList positions)
        , ("normal",    A_V3F $ V.fromList normals)
        ]
    , mPrimitive  = P_Triangles
    }
  where
    uncurry3 f (a,b,c) = f a b c

    positions =
        concatMap (\STL.Triangle{STL.vertices=(v1, v2, v3)} ->
            [uncurry3 V3 v1, uncurry3 V3 v2, uncurry3 V3 v3]) triangles

    normal (a, b, c) =
        let
            va = uncurry3 Vect.Vec3 a
            vb = uncurry3 Vect.Vec3 b
            vc = uncurry3 Vect.Vec3 c
            edge1 = vb &- va
            edge2 = vc &- vb
            Vect.Vec3 x y z = Vect.normalize (Vect.crossprod edge1 edge2)
        in
        V3 x y z

    normals = (`concatMap` triangles) $ \case
        STL.Triangle{STL.normal=Just (x, y, z)} -> replicate 3 $ V3 z y x
        STL.Triangle{STL.vertices=v}            -> replicate 3 $ normal v


initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    True <- GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      , WindowHint'RefreshRate (Just 10)
      , WindowHint'Samples (Just 4)
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win
