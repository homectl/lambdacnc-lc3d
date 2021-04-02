{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
module LambdaRay.Schwarzschild where

import qualified Codec.Picture             as Pic
import           Control.Arrow             ((&&&))
import           Control.Monad             (replicateM, unless)
import qualified Data.Aeson.Encode.Pretty  as Aeson (encodePretty)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Int                  (Int32)
import qualified Data.Map                  as Map
import           Data.Vect                 (Vec3 (..), (&-))
import qualified Data.Vect                 as Vect
import qualified Data.Vector               as V
import           Graphics.Formats.STL      (STL (..))
import qualified Graphics.Formats.STL      as STL
import           "GLFW-b" Graphics.UI.GLFW (Key (..), KeyState (..),
                                            OpenGLProfile (..), WindowHint (..))
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import           System.Directory          (getModificationTime)
import           System.FilePath           ((</>))
import qualified System.IO                 as IO

import           LambdaCube.Compiler       (prettyShowUnlines)
import qualified LambdaCube.Compiler       as L (Backend (OpenGL33),
                                                 compileMain, ppShow)
import           LambdaCube.GL             as LGL (FetchPrimitive (Triangles),
                                                   GLRenderer, GLStorage,
                                                   InputType (FTexture2D, Float, Int, V2F, V3F),
                                                   Object,
                                                   StreamType (Attribute_V3F),
                                                   TextureData, V2 (V2),
                                                   V3 (V3), (@:), (@=))
import qualified LambdaCube.GL             as LGL
import           LambdaCube.GL.Mesh        as LGL (Mesh (..),
                                                   MeshAttribute (A_V3F),
                                                   MeshPrimitive (P_Triangles))
import qualified LambdaCube.GL.Mesh        as LGL
import qualified LambdaCube.IR             as IR
import           LambdaCube.Linear         (V2F, V3F)

---------------------------------------------

enginePrefix, shadersPrefix, lcFile, pplFile, jsonFile :: FilePath
enginePrefix = "data" </> "engine" </> "lambdaray"
shadersPrefix = enginePrefix </> "shaders"
lcFile = enginePrefix </> "lambdaray.lc"
pplFile = enginePrefix </> "lambdaray.ppl"
jsonFile = enginePrefix </> "lambdaray.json"

loadRenderer :: GLStorage -> IO (Maybe GLRenderer)
loadRenderer storage = do
    putStrLn $ "Compiling renderer: " ++ lcFile
    L.compileMain ["."] L.OpenGL33 lcFile >>= \case
      Left err  -> do
        putStrLn $ "compile error:\n" ++ L.ppShow err
        return Nothing
      Right ppl -> do
        dumpPipeline ppl
        putStrLn "Allocating renderer"
        renderer <- LGL.allocRenderer ppl
        putStrLn "Assigning storage to new renderer"
        LGL.setStorage renderer storage >>= \case -- check schema compatibility
          Just err -> do
            putStrLn $ "setStorage error: " ++ err
            LGL.disposeRenderer renderer
            return Nothing
          Nothing -> do
            return $ Just renderer

  where
    dumpPipeline ppl = do
      mapM_ writeShaders (zip [0..] . map (IR.fragmentShader &&& IR.vertexShader) . V.toList . IR.programs $ ppl)
      putStrLn $ "Generating pipeline text dump: " ++ pplFile
      let pplPretty = prettyShowUnlines ppl
          pplSize = length pplPretty
      putStrLn $ "=> Pipeline text dump is " ++ show pplSize ++ " bytes"
      IO.writeFile pplFile pplPretty
      putStrLn $ "Generating JSON: " ++ jsonFile
      LBS.writeFile jsonFile $ Aeson.encodePretty ppl


writeShaders :: (Int, (String, String)) -> IO ()
writeShaders (n, (frag, vert)) = do
    let program = shadersPrefix </> show n
    putStrLn $ "Writing shaders: " ++ program ++ ".{frag,vert}"
    IO.writeFile (program ++ ".frag") frag
    IO.writeFile (program ++ ".vert") vert

---------------------------------------------

fps :: Double
fps = 24

screenSize :: (Int, Int)
screenSize = (640, 480)

---------------------------------------------

mainLoop :: GLFW.Window -> GLStorage -> TextureData -> GLRenderer -> IO ()
mainLoop win storage textureData r = lcModificationTime >>= loop r
  where
    lcModificationTime = getModificationTime lcFile

    loop renderer0 lcTime0 = do
      -- Possibly reload the rendering pipeline.
      lcTime <- lcModificationTime
      let reload = lcTime /= lcTime0
      renderer <- if not reload then return renderer0 else
        loadRenderer storage >>= \case
          Nothing -> return renderer0
          Just newRenderer -> do
            putStrLn "Reloading renderer"
            LGL.disposeRenderer renderer0
            return newRenderer

      -- Update graphics input.
      GLFW.getWindowSize win >>= \(w, h) -> do
        LGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
        LGL.updateUniforms storage $ do
          "screenSize" @= return (V2 (fromIntegral w) (fromIntegral h) :: V2F)
          "diffuseTexture" @= return textureData
          "time" @= do
            Just t <- GLFW.getTime
            return (realToFrac t :: Float)

      -- render
      LGL.renderFrame renderer
      GLFW.swapBuffers win
      GLFW.waitEventsTimeout (1/fps)

      escape <- keyIsPressed Key'Escape
      unless escape $ loop renderer lcTime

    keyIsPressed :: Key -> IO Bool
    keyIsPressed k = (==KeyState'Pressed) <$> GLFW.getKey win k

---------------------------------------------

uploadTexture :: FilePath -> IO TextureData
uploadTexture path = do
    putStrLn $ "Loading texture: " ++ path
    Right img <- Pic.readImage path
    LGL.uploadTexture2DToGPU img


main :: IO ()
main = do
    win <- uncurry (initWindow "LambdaRay") screenSize

    -- setup render data
    let inputSchema = LGL.makeSchema $ do
          LGL.defUniforms $ do
            "time"              @: Float
            "screenSize"        @: V2F
            "diffuseTexture"    @: FTexture2D

    storage <- LGL.allocStorage inputSchema

    -- load image and upload texture
    textureData <- uploadTexture "examples/logo.png"

    -- compile hello.lc to graphics pipeline description
    loadRenderer storage >>= \case
      Nothing -> return ()
      Just renderer -> do
        mainLoop win storage textureData renderer
        LGL.disposeRenderer renderer

    LGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate


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
