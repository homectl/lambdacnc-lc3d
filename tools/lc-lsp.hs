{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeInType            #-}

{- |
This is an example language server built with haskell-lsp using a 'Reactor'
design. With a 'Reactor' all requests are handled on a /single thread/.
A thread is spun up for it, which repeatedly reads from a 'TChan' of
'ReactorInput's.
The `lsp` handlers then simply pass on all the requests and
notifications onto the channel via 'ReactorInput's.
This way there is the option of executing requests on multiple threads, without
blocking server communication.

To try out this server, install it with
> cabal install lsp-demo-reactor-server -fdemo
and plug it into your client of choice.
-}
module Main (main) where
import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.MVar        (MVar)
import qualified Control.Concurrent.MVar        as MVar
import           Control.Concurrent.STM.TChan   (TChan, newTChan, readTChan,
                                                 writeTChan)
import qualified Control.Exception              as E
import           Control.Lens                   (to, (^.))
import           Control.Monad                  (forM, forever, void)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.STM              (atomically)
import qualified Data.Aeson                     as J
import qualified Data.HashMap.Strict            as H
import qualified Data.Text                      as T
import           GHC.Generics                   (Generic)
import qualified LambdaCube.Compiler            as C
import qualified LambdaCube.Compiler.InferMonad as C
import qualified LambdaCube.Compiler.Pretty     as C (Doc, vcat)
import           LambdaCube.TypeInfo            as T (CompileResult (..),
                                                      ErrorInfo (..),
                                                      Range (..), TypeInfo (..),
                                                      WarningInfo (..))
import           Language.LSP.Diagnostics       (partitionBySource)
import           Language.LSP.Server
import qualified Language.LSP.Types             as J
import qualified Language.LSP.Types.Lens        as J
import           Language.LSP.VFS               (VirtualFile (VirtualFile))
import           System.Exit                    (ExitCode (ExitFailure),
                                                 exitSuccess, exitWith)
import           System.Log.Logger              (Priority (DEBUG), debugM,
                                                 errorM, removeAllHandlers)


-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

type Types = [(C.Range, [C.Doc])]

--

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

data Config = Config { fooTheBar :: Bool, wibbleFactor :: Int }
  deriving (Generic, J.ToJSON, J.FromJSON)

run :: IO Int
run = flip E.catches handlers $ do
  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  types <- MVar.newMVar []

  -- Right (_, (_, Right (_, (res, _), _))) <- C.loadFile "C:\\Users\\Pippijn\\Documents\\code\\lambdacnc\\examples\\hello_stl.lc"
  -- print . C.getMostRelevant 4 62 . C.sortInfos . C.listTypeInfos $ res

  let
    serverDefinition = ServerDefinition
      { onConfigurationChange = \v -> case J.fromJSON v of
          J.Error e -> pure $ Left (T.pack e)
          J.Success cfg -> do
            sendNotification J.SWindowShowMessage $
              J.ShowMessageParams J.MtInfo $ "Wibble factor set to " <> T.pack (show (wibbleFactor cfg))
            pure $ Right cfg
      , doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env)
      , staticHandlers = lspHandlers rin types
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = lspOptions
      }

  flip E.finally finalProc $ do
    setupLogger Nothing ["reactor"] DEBUG
    runServer serverDefinition

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
  }

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  , executeCommandCommands = Just ["lsp-hello-command"]
  }

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput
  = ReactorAction (IO ())

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.NormalizedUri -> Maybe Int -> [T.Text] -> LspM Config ()
sendDiagnostics fileUri version errs = do
  let
    diags = [J.Diagnostic
              (J.Range (J.Position 0 1) (J.Position 0 5))
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "lsp-hello") -- source
              err
              Nothing -- tags
              (Just (J.List []))
              | err <- errs
            ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debugM "reactor" "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TChan ReactorInput -> MVar Types -> Handlers (LspM Config)
lspHandlers rin = mapHandlers goReq goNot . handle
  where
    goReq :: forall (a :: J.Method J.FromClient J.Request). Handler (LspM Config) a -> Handler (LspM Config) a
    goReq f = \msg k -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)

    goNot :: forall (a :: J.Method J.FromClient J.Notification). Handler (LspM Config) a -> Handler (LspM Config) a
    goNot f = \msg -> do
      env <- getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle :: MVar Types -> Handlers (LspM Config)
handle types = mconcat
  [ notificationHandler J.STextDocumentDidOpen $ \msg -> do
      let doc  = msg ^. J.params . J.textDocument . J.uri
          fileName =  J.uriToFilePath doc
      liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
      recompile types doc

  , notificationHandler J.STextDocumentDidChange $ \msg -> do
      let doc  = msg ^. J.params
                      . J.textDocument
                      . J.uri
                      . to J.toNormalizedUri
      liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
      mdoc <- getVirtualFile doc
      case mdoc of
        Just (VirtualFile _version str _) -> do
          liftIO $ debugM "reactor.handle" $ "Found the virtual file: " ++ show str
        Nothing -> do
          liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc

  , notificationHandler J.STextDocumentDidSave $ \msg -> do
      let doc = msg ^. J.params . J.textDocument . J.uri
          fileName = J.uriToFilePath doc
      liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
      recompile types doc

  , requestHandler J.STextDocumentHover $ \req responder -> do
      liftIO $ debugM "reactor.handle" "Processing a textDocument/hover request"
      tys <- liftIO $ MVar.readMVar types
      let J.HoverParams _doc pos _workDone = req ^. J.params
          sposToPos (C.SPos l c) = J.Position (l - 1) (c - 1)
          posToSPos (J.Position l c) = (l + 1, c + 1)
          (range, tyText) = case uncurry C.getMostRelevant (posToSPos pos) tys of
            Nothing -> (Nothing, "Unknown type")
            Just (r@C.Range{C.rangeStart=s, C.rangeStop=e}, ty) ->
              (Just $ J.Range (sposToPos s) (sposToPos e), T.pack . show $ ty)
          ms = J.HoverContents $ J.markedUpContent "lsp-hello" tyText
          rsp = J.Hover ms range
      responder (Right $ Just rsp)
  ]

-- ---------------------------------------------------------------------

recompile :: MVar Types -> J.Uri -> LspM Config ()
recompile types doc =
  let fileName = J.uriToFilePath doc in
  case fileName of
    Nothing -> return ()
    Just path -> liftIO (C.loadFile path) >>= \case
      Right (fi, (_, Right (_, (res, _), _))) -> do
        liftIO $ debugM "reactor.handle" $ "Compilation succeeded: " ++ show fi
        liftIO $ debugM "reactor.handle" $ "Got type infos: " ++ show (length $ C.listTypeInfos res)
        liftIO $ MVar.swapMVar types . C.sortInfos . C.listTypeInfos $ res
        sendDiagnostics (J.toNormalizedUri doc) (Just 0) []
      Left err ->
        sendDiagnostics (J.toNormalizedUri doc) (Just 0) [T.pack $ show err]
