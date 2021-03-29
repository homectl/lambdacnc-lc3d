{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance MonadMask m => MonadMask (ExceptT e m)
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module LambdaCube.Compiler
    ( IR.Backend(..)
    , IR.Pipeline
    , module Exported

    , MMT, runMMT, mapMMT
    , MM, runMM
    , ioFetch, decideFilePath
    , loadModule, loadFile, getDef, compileMain, parseModule, preCompile
    , removeFromCache

    , compilePipeline
    , ppShow
    , plainShow
    , prettyShowUnlines

    , typecheckModule
    ) where

import           Control.Arrow                       (Arrow (first, second, (&&&)),
                                                      ArrowChoice (right))
import           Control.Monad.Catch                 (MonadCatch, MonadMask,
                                                      MonadThrow)
import           Control.Monad.Except                (runExcept, runExceptT)
import           Control.Monad.Reader                (MonadReader (ask),
                                                      ReaderT (..))
import           Control.Monad.State.Strict          (MonadState, StateT,
                                                      evalStateT, gets, modify)
import           Control.Monad.Writer                (MonadIO (..), forM,
                                                      runWriter)
import           Data.Function                       (on)
import qualified Data.IntMap.Strict                  as IM
import           Data.List                           (intercalate, nubBy)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import           System.FilePath                     (dropExtension, normalise,
                                                      splitFileName, splitPath,
                                                      takeDirectory,
                                                      takeExtension, (</>))
--import Debug.Trace

import           LambdaCube.Compiler.CoreToIR        (compilePipeline)
import           LambdaCube.Compiler.DesugaredSource (Export (..),
                                                      ImportItems (..),
                                                      Module_ (..), Stmt)
import           LambdaCube.Compiler.Infer           (inference)
import           LambdaCube.Compiler.InferMonad      (GlobalEnv, closeGlobalEnv,
                                                      initEnv)
import           LambdaCube.Compiler.Parser          (DesugarInfo, Module,
                                                      parseLC, runDefParser)
import           LambdaCube.Compiler.Pretty          (Doc, PShow (pShow), hsep,
                                                      plainShow, ppShow, text,
                                                      (<+>))
import qualified LambdaCube.IR                       as IR

import           LambdaCube.Compiler.Core            as Exported (Exp,
                                                                  ExpType (..),
                                                                  Type,
                                                                  boolType,
                                                                  closeExp,
                                                                  closeExpType,
                                                                  hnf, mkDoc,
                                                                  outputType,
                                                                  pattern ET,
                                                                  trueExp)
import           LambdaCube.Compiler.DesugaredSource as Exported (FileInfo (..),
                                                                  Range (..),
                                                                  SI (..),
                                                                  SIName (..),
                                                                  SPos (..),
                                                                  pattern SIName,
                                                                  pattern SPos,
                                                                  sName)
import           LambdaCube.Compiler.InferMonad      as Exported (Info (..),
                                                                  Infos,
                                                                  errorRange,
                                                                  listAllInfos,
                                                                  listAllInfos',
                                                                  listErrors,
                                                                  listTraceInfos,
                                                                  listTypeInfos,
                                                                  listWarnings)
import           LambdaCube.Compiler.Utils           (prettyShowUnlines,
                                                      readFileIfExists, (<&>))
--import LambdaCube.Compiler.Infer as Exported ()

-- include path for: Builtins, Internals and Prelude
import           Paths_lambdacnc                     (getDataDir)

--------------------------------------------------------------------------------

type MName = String
type SName = String
type SourceCode = String

-- file name or module name?
decideFilePath :: FilePath -> Either FilePath FilePath
decideFilePath n
    | takeExtension n == ".lc" = Left n
    | otherwise = Right n

dropExtension' :: String -> FilePath -> FilePath
dropExtension' e f
    | takeExtension f == e = dropExtension f
    | otherwise = error $ "dropExtension: expected extension: " ++ e ++ " ; filename: " ++ f

fileNameToModuleName :: FilePath -> [Char]
fileNameToModuleName n
    = intercalate "." $ remDot $ (\(a, b) -> map takeDirectory (splitPath a) ++ [b]) $ splitFileName $ dropExtension' ".lc" $ normalise n
  where
    remDot (".": xs) = xs
    remDot xs        = xs

moduleNameToFileName :: [Char] -> [Char]
moduleNameToFileName n = hn n ++ ".lc"
  where
    hn = h []
    h acc []       = reverse acc
    h acc ('.':cs) = reverse acc </> hn cs
    h acc (c: cs)  = h (c: acc) cs

type ModuleFetcher m = Maybe FilePath -> Either FilePath MName -> m (Either Doc (FilePath, MName, m SourceCode))

ioFetch :: MonadIO m => [FilePath] -> ModuleFetcher (MMT m x)
ioFetch paths' imp n = do
    preludePath <- (</> "data" </> "lc") <$> liftIO getDataDir
    let paths = map (id &&& id) paths' ++ [(preludePath, "<<installed-prelude-path>>")]
        find ((x, (x', mn)): xs) = liftIO (readFileIfExists x) >>= maybe (find xs) (\src -> return $ Right (x, mn, liftIO src))
        find [] = return $ Left $ "can't find" <+> either (("lc file" <+>) . text) (("module" <+>) . text) n
                                  <+> "in path" <+> hsep (text . snd <$> paths)
    find $ nubBy ((==) `on` fst) $ map (first normalise . lcModuleFile) paths
  where
    lcModuleFile (path, path') = case n of
        Left n  -> (path </> n, (path' </> n, fileNameToModuleName n))
        Right n -> (path </> moduleNameToFileName n, (path' </> moduleNameToFileName n, n))

--------------------------------------------------------------------------------

type MMTReader m x a = ReaderT (ModuleFetcher (MMT m x)) (StateT (Modules x) m) a

newtype MMT m x a = MMT { runMMT :: MMTReader m x a }
    deriving (Functor, Applicative, Monad, MonadReader (ModuleFetcher (MMT m x)), MonadState (Modules x), MonadIO, MonadThrow, MonadCatch, MonadMask)

type MM = MMT IO Infos

mapMMT
    :: (MMTReader m1 x1 a1 -> MMTReader m2 x2 a2)
    -> MMT m1 x1 a1
    -> MMT m2 x2 a2
mapMMT f (MMT m) = MMT $ f m

runMM :: Monad m => ModuleFetcher (MMT m x) -> MMT m x a -> m a
runMM fetcher
    = flip evalStateT (Modules mempty mempty 1)
    . flip runReaderT fetcher
    . runMMT

-- TODO: remove dependent modules from cache too?
removeFromCache :: Monad m => FilePath -> MMT m x ()
removeFromCache f = modify $ \m@(Modules nm im ni) -> case Map.lookup f nm of
    Nothing -> m
    Just i  -> Modules (Map.delete f nm) (IM.delete i im) ni

type Module' x = (SourceCode, Either Doc{-error msg-} (Module, x, Either Doc{-error msg-} (DesugarInfo, GlobalEnv)))

data Modules x = Modules
    { moduleIds :: !(Map FilePath Int)
    , modules   :: !(IM.IntMap (FileInfo, Module' x))
    , nextMId   :: !Int
    }

loadModule :: MonadMask m => ((Infos, [Stmt]) -> x) -> Maybe FilePath -> Either FilePath MName -> MMT m x (Either Doc (FileInfo, Module' x))
loadModule ex imp mname_ = do
  r <- ask >>= \fetch -> fetch imp mname_
  case r of
   Left err -> return $ Left err
   Right (fname, mname, srcm) -> do
    c <- gets $ Map.lookup fname . moduleIds
    case c of
      Just fid -> gets $ Right . (IM.! fid) . modules
      _ -> do
        src <- srcm
        fid <- gets nextMId
        modify $ \(Modules nm im ni) -> Modules (Map.insert fname fid nm) im $ ni+1
        let fi = FileInfo fid fname mname
        res <- case parseLC fi of
          Left e -> return $ Left $ text $ show e
          Right e -> do
            modify $ \(Modules nm im ni) -> Modules nm (IM.insert fid (fi, (src, Right (e, ex mempty, Left $ "cycles in module imports:" <+> pShow mname <+> pShow (fst <$> moduleImports e)))) im) ni
            ms <- forM (moduleImports e) $ \(m, is) -> loadModule ex (Just fname) (Right $ sName m) <&> \case
                      Left err -> Left $ pShow m <+> "is not found"
                      Right (fb, (src, dsge)) ->
                         either (Left . (\errm-> pShow m <+> "couldn't be parsed:\n" <+> errm))
                                (\(pm, x, e) -> either
                                    (Left .  (\errm-> pShow m <+> "couldn't be typechecked:\n" <+> errm))
                                    (\(ds, ge) -> Right (ds{-todo: filter-}, Map.filterWithKey (\k _ -> filterImports is k) ge))
                                    e)
                                dsge
            let (res, err) = case sequence ms of
                  Left err -> (ex mempty, Left $ pShow err)
                  Right ms@(mconcat -> (ds, ge)) -> case runExcept $ runDefParser ds $ definitions e of
                    Left err -> (ex mempty, Left $ pShow err)
                    Right (defs, warnings, dsinfo) -> (ex (map ParseWarning warnings ++ is, defs), res_1)
                     where
                        (res, is) = runWriter . flip runReaderT (extensions e, initEnv <> ge) . runExceptT $ inference defs

                        res_1 = case res of
                              Left err -> Left $ pShow err
                              Right (mconcat -> newge) ->
                                right mconcat $ forM (fromMaybe [ExportModule $ SIName mempty mname] $ moduleExports e) $ \case
                                    ExportId (sName -> d) -> case Map.lookup d newge of
                                        Just def -> Right (mempty{-TODO-}, Map.singleton d def)
                                        Nothing  -> Left $ text d <+> "is not defined"
                                    ExportModule (sName -> m) | m == mname -> Right (dsinfo, newge)
                                    ExportModule m -> case [ x | ((m', _), x) <- zip (moduleImports e) ms, m' == m] of
                                        [x] -> Right x
                                        []  -> Left $ "empty export list in module" <+> text fname -- m, map fst $ moduleImports e, mname)
                                        _   -> error "export list: internal error"
            return (Right (e, res, err))
        modify $ \(Modules nm im ni) -> Modules nm (IM.insert fid (fi, (src, res)) im) ni
        return $ Right (fi, (src, res))
  where
    filterImports (ImportAllBut ns) = not . (`elem` map sName ns)
    filterImports (ImportJust ns)   = (`elem` map sName ns)


loadFile :: FilePath -> IO (Either Doc (FileInfo, Module' (Infos, [Stmt])))
loadFile mname = runMM (ioFetch []) $ loadModule id Nothing (Left mname)

-- used in runTests
getDef :: MonadMask m => FilePath -> SName -> Maybe Exp -> MMT m (Infos, [Stmt]) ((Infos, [Stmt]), Either Doc (FileInfo, Either Doc ExpType))
getDef = getDef_ id

getDef_ :: (MonadMask m, Monoid a) => ((Infos, [Stmt]) -> a) -> FilePath -> SName -> Maybe Type -> MMT m a (a, Either Doc (FileInfo, Either Doc ExpType))
getDef_ ex m d ty = loadModule ex Nothing (Left m) <&> \case
    Left err -> (mempty, Left err)
    Right (fname, (src, Left err)) -> (mempty, Left err)
    Right (fname, (src, Right (pm, infos, Left err))) -> (,) infos $ Left err
    Right (fname, (src, Right (pm, infos, Right (_, ge)))) -> (,) infos $ Right
        ( fname
        , case Map.lookup d ge of
          Just (e, thy, si)
            | Just False <- (== thy) <$> ty          -- TODO: better type comparison
                -> Left $ "type of" <+> text d <+> "should be" <+> pShow ty <+> "instead of" <+> pShow thy
            | otherwise -> Right (ET e thy)
          Nothing -> Left $ text d <+> "is not found"
        )

compilePipeline' :: (MonadMask m, Monoid a) => ((Infos, [Stmt]) -> a) -> IR.Backend -> FilePath -> MMT m a (a, Either Doc IR.Pipeline)
compilePipeline' ex backend m
    = second (either Left (fmap (compilePipeline backend) . snd)) <$> getDef_ ex m "main" (Just outputType)

-- | most commonly used interface for end users
compileMain :: [FilePath] -> IR.Backend -> MName -> IO (Either Doc IR.Pipeline)
compileMain path backend fname
    = snd <$> runMM (ioFetch path) (compilePipeline' (const ()) backend fname)

parseModule :: [FilePath] -> MName -> IO (Either Doc String)
parseModule path fname = runMM (ioFetch path) $ loadModule snd Nothing (Left fname) <&> \case
    Left err                                   -> Left err
    Right (fname, (src, Left err))             -> Left err
    Right (fname, (src, Right (pm, infos, _))) -> Right $ pPrintStmts infos

-- used by the compiler-service of the online editor
preCompile :: (MonadMask m, MonadIO m) => [FilePath] -> [FilePath] -> IR.Backend -> FilePath -> IO (String -> m (Either Doc IR.Pipeline, (Infos, String)))
preCompile paths paths' backend mod = do
  res <- runMM (ioFetch paths) $ loadModule ex Nothing $ Left mod
  case res of
    Left err -> error $ "Prelude could not compiled:" ++ show err
    Right (fi, prelude) -> return compile
      where
        compile src = runMM fetch $ do
            let pname = "." </> "Prelude.lc"
            modify $ \(Modules nm im ni) -> Modules (Map.insert pname ni nm) (IM.insert ni (FileInfo ni pname "Prelude", prelude) im) (ni+1)
            (snd &&& fst) <$> compilePipeline' ex backend "Main"
          where
            fetch imp = \case
                Left "Prelude" -> return $ Right ("./Prelude.lc", "Prelude", undefined)
                Left "Main"    -> return $ Right ("./Main.lc", "Main", return src)
                n -> ioFetch paths' imp n
  where
    ex = second pPrintStmts

pPrintStmts :: PShow a => [a] -> String
pPrintStmts = unlines . map ((++"\n") . plainShow)

-- basic interface
type Program = Map FilePath (DesugarInfo, GlobalEnv)

typecheckModule :: [FilePath] -> MName -> IO (Either [Doc] Program)
typecheckModule path fname = runMM (ioFetch path) $ loadModule (const ()) Nothing (Left fname) >> do
  fileInfoModules <- gets (IM.elems . modules)
  let collect (FileInfo{..}, (sourceCode, errorOrGlobalEnv)) = case errorOrGlobalEnv of
        Left error -> ([error],mempty)
        Right (module_, (), Left error) -> ([error], mempty)
        Right (module_, (), Right (desugarInfo, globalEnv)) -> (mempty, Map.singleton filePath (desugarInfo, closeGlobalEnv globalEnv))
      (error, program) = mconcat $ map collect fileInfoModules
  pure $ case error of
    [] -> Right program
    _  -> Left error
