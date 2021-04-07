{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
module LambdaCNC.Viewer where

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
enginePrefix = "data" </> "engine" </> "lambdacnc"
shadersPrefix = enginePrefix </> "shaders"
lcFile = enginePrefix </> "lambdacnc.lc"
pplFile = enginePrefix </> "lambdacnc.ppl"
jsonFile = enginePrefix </> "lambdacnc.json"

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

data Machine = Machine
    { bed    :: LGL.Object
    , xaxis  :: LGL.Object
    , yaxis  :: LGL.Object
    , zaxis  :: LGL.Object
    , ground :: LGL.Object
    , bulbs  :: [LGL.Object]
    }


data MachinePosition a = MachinePosition
    { xPos :: a
    , yPos :: a
    , zPos :: a
    }
    deriving (Functor)

---------------------------------------------

machMax :: MachinePosition Int
machMax = MachinePosition 40000 61500 5600

startPos :: MachinePosition Int
startPos = fmap (`div` 2) machMax

fps :: Double
fps = 240

screenSize :: (Int, Int)
screenSize = (1500, 800)

---------------------------------------------

mainLoop :: GLFW.Window -> GLStorage -> TextureData -> Machine -> GLRenderer -> IO ()
mainLoop win storage textureData Machine{..} r = lcModificationTime >>= loop r startPos
  where
    lcModificationTime = getModificationTime lcFile

    loop renderer0 pos0 lcTime0 = do
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

      -- Update machine position.
      pos <- reposition pos0

      -- Update graphics input.
      GLFW.getWindowSize win >>= \(w, h) -> do
        LGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
        LGL.updateUniforms storage $ do
          "screenSize" @= return (V2 (fromIntegral w) (fromIntegral h) :: V2F)
          "diffuseTexture" @= return textureData
          "time" @= do
            Just t <- GLFW.getTime
            return (realToFrac t :: Float)

      -- Render machine in the current position.
      let x = toFloat (xPos pos - (xMax `div` 2))
          y = toFloat (yPos pos - (yMax `div` 2))
          z = toFloat (zPos pos - (zMax `div` 2))
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

    MachinePosition xMax yMax zMax = machMax

    reposition :: MachinePosition Int -> IO (MachinePosition Int)
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

---------------------------------------------

uploadModel :: String -> [String] -> (IO Object -> IO b) -> GLStorage -> [Char] -> IO b
uploadModel slotName uniformNames f storage path = do
    putStrLn $ "Loading model: " ++ path
    mesh <- STL.mustLoadSTL path >>= LGL.uploadMeshToGPU . stlToMesh
    f $ LGL.addMeshToObjectArray storage slotName uniformNames mesh


uploadObject :: GLStorage -> FilePath -> IO Object
uploadObject = uploadModel "objects" ["position"] id

uploadLights :: Int -> GLStorage -> FilePath -> IO [Object]
uploadLights = uploadModel "lights" ["index"] . replicateM

uploadTexture :: FilePath -> IO TextureData
uploadTexture path = do
    putStrLn $ "Loading texture: " ++ path
    Right img <- Pic.readImage path
    LGL.uploadTexture2DToGPU img


main :: IO ()
main = do
    win <- uncurry (initWindow "LambdaCNC (LambdaCube)") screenSize

    -- setup render data
    let inputSchema = LGL.makeSchema $ do
          LGL.defObjectArray "objects" Triangles $ do
            "position"          @: Attribute_V3F
            "normal"            @: Attribute_V3F
          LGL.defObjectArray "lights" Triangles $ do
            "position"          @: Attribute_V3F
            "normal"            @: Attribute_V3F
          LGL.defUniforms $ do
            "time"              @: Float
            "index"             @: Int
            "position"          @: V3F
            "screenSize"        @: V2F
            "diffuseTexture"    @: FTexture2D

    storage <- LGL.allocStorage inputSchema

    -- upload geometry to GPU and add to pipeline input
    mach@Machine{..} <- Machine
        <$> uploadObject storage "data/models/Bed.stl"
        <*> uploadObject storage "data/models/XAxis.stl"
        <*> uploadObject storage "data/models/YAxis.stl"
        <*> uploadObject storage "data/models/ZAxis.stl"
        <*> uploadObject storage "data/models/Ground.stl"
        <*> uploadLights 2 storage "data/models/lightbulb.stl"

    LGL.enableObject bed True
    LGL.enableObject yaxis True
    LGL.enableObject xaxis True
    LGL.enableObject zaxis True
    LGL.enableObject ground True
    mapM_ (`LGL.enableObject` True) bulbs

    mapM_ (\(i, b) -> LGL.updateObjectUniforms b $ do
      "index" @= return (i :: Int32)) $ zip [0..] bulbs

    LGL.updateObjectUniforms ground $ do
      "position" @= return (V3 0 0 (-500) :: V3F)

    -- load image and upload texture
    textureData <- uploadTexture "examples/logo.png"

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
            va = uncurry3 Vec3 a
            vb = uncurry3 Vec3 b
            vc = uncurry3 Vec3 c
            edge1 = vb &- va
            edge2 = vc &- vb
            Vec3 x y z = Vect.normalize (Vect.crossprod edge1 edge2)
        in
        V3 x y z

    normals = (`concatMap` triangles) $ \case
        STL.Triangle{STL.normal=Just n} -> replicate 3 $ uncurry3 V3 n
        STL.Triangle{STL.vertices=v}    -> replicate 3 $ normal v


initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    True <- GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      -- , WindowHint'RefreshRate (Just 10)
      -- , WindowHint'Samples (Just 4)
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win
