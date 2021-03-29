module LambdaCube.MtlParser
  ( ObjMaterial (..)
  , MtlLib
  , parseMtl
  , readMtl
  ) where

import           Control.Monad.State.Strict (MonadState (put), State, evalState,
                                             gets, modify')
import           Control.Monad.Writer       (MonadWriter (tell), WriterT,
                                             execWriterT)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (maybeToList)
import           Data.Text                  (Text, pack)

type Vec3 = (Float,Float,Float)

type MtlLib = Map Text ObjMaterial

data ObjMaterial
  = ObjMaterial
  { mtl_Name   :: Text
  , mtl_Ka     :: Vec3   -- ambient color
  , mtl_Kd     :: Vec3   -- diffuse color
  , mtl_Ks     :: Vec3   -- specular color
  , mtl_illum  :: Int
  , mtl_Tr     :: Float  -- transparency
  , mtl_Ns     :: Float  -- specular exponent
  , mtl_map_Kd :: Maybe String -- diffuse texture file name
  }
  deriving (Eq,Show)

newMaterial :: Text -> ObjMaterial
newMaterial name = ObjMaterial
  { mtl_Name    = name
  , mtl_Ka      = (1, 1, 1)
  , mtl_Kd      = (1, 1, 1)
  , mtl_Ks      = (0, 0, 0)
  , mtl_illum   = 1
  , mtl_Tr      = 1
  , mtl_Ns      = 0
  , mtl_map_Kd  = Nothing
  }

type Mtl = WriterT [ObjMaterial] (State (Maybe ObjMaterial))

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

readVec3 :: String -> String -> String -> Maybe Vec3
readVec3 r g b = (,,) <$> readMaybe r <*> readMaybe g <*> readMaybe b

setAttr :: (ObjMaterial -> ObjMaterial) -> WriterT [ObjMaterial] (State (Maybe ObjMaterial)) ()
setAttr = modify' . fmap

addMaterial :: WriterT [ObjMaterial] (State (Maybe ObjMaterial)) ()
addMaterial = gets maybeToList >>= tell

parseLine :: String -> Mtl ()
parseLine s = case words $ takeWhile (/='#') s of
  ["newmtl",name] -> do
                      addMaterial
                      put $ Just $ newMaterial $ pack name
  ["map_Kd",textureName]                      -> setAttr (\s -> s {mtl_map_Kd = Just textureName})
  ["Ka",r,g,b] | Just rgb <- readVec3 r g b   -> setAttr (\s -> s {mtl_Ka = rgb})
  ["Kd",r,g,b] | Just rgb <- readVec3 r g b   -> setAttr (\s -> s {mtl_Kd = rgb})
  ["Ks",r,g,b] | Just rgb <- readVec3 r g b   -> setAttr (\s -> s {mtl_Ks = rgb})
  ["illum",a]  | Just v <- readMaybe a        -> setAttr (\s -> s {mtl_illum = v})
  ["Tr",a]     | Just v <- readMaybe a        -> setAttr (\s -> s {mtl_Tr = v})
  ["Ns",a]     | Just v <- readMaybe a        -> setAttr (\s -> s {mtl_Ns = v})
  _ -> return ()

parseMtl :: String -> MtlLib
parseMtl src = Map.fromList [(mtl_Name m,m) | m <- evalState (execWriterT (mapM_ parseLine (lines src) >> addMaterial)) Nothing]

readMtl :: String -> IO MtlLib
readMtl "" = return $ Map.singleton (pack "") $ newMaterial (pack "")
readMtl fname = do
  putStrLn $ "reading mtl: " ++ fname
  parseMtl <$> readFile fname
