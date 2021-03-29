{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.MVar    (MVar, newEmptyMVar, putMVar,
                                             readMVar)
import           Control.DeepSeq
import           Control.Exception          (ErrorCall, PatternMatchFail,
                                             evaluate)
import           Control.Monad              (forever, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Array.IO              (IOArray, getBounds, newArray,
                                             readArray, writeArray)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.UTF8  (fromString)
import           Data.Char                  (digitToInt)
import           Data.Digest.Pure.MD5       (MD5Digest, md5)
import           Data.List                  (sort)
import qualified Data.Vector                as V
import           LambdaCube.Compiler        as C
import           LambdaCube.Compiler.Pretty as C (Doc, vcat)
import           LambdaCube.TypeInfo        as T (CompileResult (..),
                                                  ErrorInfo (..), Range (..),
                                                  TypeInfo (..),
                                                  WarningInfo (..))
import qualified Network.WebSockets         as WS
import qualified Network.WebSockets.Snap    as WS
import qualified Snap.Core                  as Snap
import qualified Snap.Http.Server           as Snap
import qualified Snap.Snaplet.Config        as Snap
import qualified System.IO                  as IO

-------------------------

data Cache a
    = Cache
        { array         :: IOArray Int [CacheEntry a]
        , cacheLineSize :: Int  -- length of the lists
        }

data CacheEntry a
    = CacheEntry
        { question :: Hash
        , answer   :: MVar a
        }


newCache :: Int -> IO (Cache a)
newCache x = do
    a <- newArray (0,255) []
    return $ Cache a x


clearCache :: Cache a -> IO ()
clearCache c = do
    (a,b) <- getBounds $ array c
    mapM_ (\i -> writeArray (array c) i []) [a..b]


lookupCache :: Cache a -> Hash -> IO (Either a (a -> IO ()))
lookupCache ch e = modifyCacheLine (array ch) (getIndex e) $ \vv ->
    case lookupIA (cacheLineSize ch) (\x -> e == question x) vv of
        (Just x_, c) -> do
            x <- readMVar (answer x_)
            return (x_ : c, Left x)
        (Nothing, c) -> do
            v <- newEmptyMVar
            return (CacheEntry e v: c, Right $ putMVar v)
 where
    lookupIA :: Int -> (a -> Bool) -> [a] -> (Maybe a, [a])
    lookupIA i p l = f i l  where
        f _ (x: xs) | p x = (Just x, xs)
        f 1 _  = (Nothing, [])
        f i (x: xs) = case f (i-1) xs of
            (a, b) -> (a, x:b)
        f _ [] = (Nothing, [])

    modifyCacheLine ch i f = do
        x <- readArray ch i
        (x', r) <- f x
        writeArray ch i x'
        return r

    getIndex :: Hash -> Int
    getIndex e = 16 * digitToInt a + digitToInt b where (a:b:_) = show e

--------------------------------------------------------------------------------

type Hash = MD5Digest

mkHash :: String -> Hash
mkHash = md5 . fromString

--------------------------------------------------------------------------------

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  config <- Snap.commandLineAppConfig Snap.defaultConfig
  compiler <- preCompile [] ["exercises"] WebGL1 "Prelude.lc"
  ch <- newCache 10
  Snap.httpServe config $ app compiler ch

--runApp = WS.runWebSocketsSnapWith (WS.ConnectionOptions $ putStrLn "pong received")
runApp :: WS.ServerApp -> Snap.Snap ()
runApp = WS.runWebSocketsSnap

--app :: (String -> IO (Either String Pipeline, Infos)) -> Snap ()
app :: (String -> IO (Either Doc Pipeline, ([Info], String))) -> Cache ByteString -> Snap.Snap ()
app compiler ch = Snap.route
    [ (,) "compile"      $ runApp compileApp
    ]
  where

    compileApp :: WS.ServerApp
    compileApp pending = do
        putStrLn "compileApp"
        c <- WS.acceptRequest pending
        WS.forkPingThread c 5
        forever $ do
            WS.sendPing c ("hello" :: B.ByteString)
            bs <- BC.unpack <$> WS.receiveData c
            --print bs
            let h = mkHash bs
            r <- lookupCache ch h
            json <- case r of
              Left json -> return json
              Right add -> do
                json <- catchErr er $ encodePretty . ff <$> compiler bs
                add json
                return json
            WS.sendTextData c json
        putStrLn "compileApp ended"
      where
        cvtRange (C.Range _ (SPos r c) (SPos r' c')) = T.Range r c r' c'

        ff (Left err, (infos, _))    = CompileError (plainShow err) (convertInfos infos) (convertWarnings infos) (convertErrors infos)
        ff (Right ppl, (infos, dsg)) = Compiled dsg (prettyShowUnlines ppl) ppl (convertInfos infos) (convertWarnings infos)

        er e = return $ encodePretty $ CompileError ("\n!FAIL\n" ++ e) mempty mempty mempty

        convertInfos is = V.fromList [TypeInfo (cvtRange r) $ C.plainShow $ C.vcat c | (r, c) <- listTypeInfos is ]
        convertErrors is = V.fromList [ErrorInfo (cvtRange r) $ C.plainShow $ C.vcat c | (r, c) <- listErrors is ]
        convertWarnings is = V.fromList [WarningInfo (cvtRange r) $ C.plainShow $ C.vcat c | (r, c) <- listWarnings is ]

catchErr :: (MonadCatch m, NFData a, MonadIO m) => (String -> m a) -> m a -> m a
catchErr er m = (m >>= liftIO . evaluate . force) `catch` getErr `catch` getPMatchFail
  where
    getErr (e :: ErrorCall) = catchErr er $ er $ show e
    getPMatchFail (e :: PatternMatchFail) = catchErr er $ er $ show e

