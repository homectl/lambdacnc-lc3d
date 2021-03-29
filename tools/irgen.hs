{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy             as LText
import           Text.EDE                   (eitherParseFile, eitherRenderWith,
                                             fromPairs, (.=))
import           Text.EDE.Filters           (Quote, Term, Unquote, (@:))

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.Text                  (Text)

import           System.Directory           (copyFile, createDirectoryIfMissing,
                                             doesFileExist, removeFile)
import           System.FilePath            (takeDirectory)

import           Control.Monad.Writer       (execWriter, forM_)
import           Data.Time.Clock            (getCurrentTime)

import           LambdaCube.DDL.Definitions (modules)
import           LambdaCube.DDL.Language

instance Unquote [Field]
instance Unquote [Char]
instance Quote [Char]
instance Quote [Instance]
instance Unquote DataDef
instance Unquote Type
instance Unquote [(Target,Instance)]

main :: IO ()
main = do
  dataSwift <- eitherParseFile "data/templates/data.swift.ede"
  dataJava <- eitherParseFile "data/templates/data.java.ede"
  jsonJava <- eitherParseFile "data/templates/json.java.ede"
  dataHpp <- eitherParseFile "data/templates/data.hpp.ede"
  dataHpp2 <- eitherParseFile "data/templates/data.hpp2.ede"
  dataCpp <- eitherParseFile "data/templates/data.cpp.ede"
  dataCs <- eitherParseFile "data/templates/data.cs.ede"
  dataHs <- eitherParseFile "data/templates/data.hs.ede"
  dataPs <- eitherParseFile "data/templates/data.purs.ede"
  let generate mod@(ModuleDef name imports def) = do
        dt <- getCurrentTime
        let env = fromPairs
              [ "dataAndType" .= def
              , "definitions" .= [a | a@DataDef{} <- def ]
              , "moduleName"  .= name
              , "dateTime"    .= dt
              , "imports"     .= imports
              , "usedTypes"   .= collectTypes aliasMap mod
              , "usedCSTypes" .= (Set.fromList . Map.elems . Map.fromList $ [ (csType name aliasMap t,t) | t <- Set.toList $ collectTypes aliasMap mod])
              ]
            aliasMap = Map.fromList [(n,t) | TypeAlias n t <- def]
            mylib :: HashMap Text Term
            mylib = HashMap.fromList
                [ "hasFieldNames"   @: hasFieldNames
                , "parens"          @: parens
                , "constType"       @: constType
                , "hsType"          @: hsType aliasMap
                , "psType"          @: psType aliasMap
                , "cppType"         @: cppType aliasMap
                , "csType"          @: csType name aliasMap
                , "typeEnum"        @: typeEnum aliasMap
                , "javaType"        @: javaType aliasMap
                , "swiftType"       @: swiftType aliasMap
                , "hasEnumConstructor" @: hasEnumConstructor
                , "psInstances"     @: filterInstances PureScript
                , "hsInstances"     @: filterInstances Haskell
                ]

            toPath a = flip map a $ \case
              '.' -> '/'
              c   -> c

            writeFileIfDiffer fname txt = doesFileExist fname >>= \case
              False -> do
                        createDirectoryIfMissing True $ takeDirectory fname
                        writeFile fname txt
              True  -> do
                        oldTxt <- readFile fname
                        case (lines oldTxt, lines txt) of
                          (_ : oldTime : old, _ : newTime : new) | old == new -> return () -- NOTE: timestamp is always in the second line
                          _ -> removeFile fname >> writeFile fname txt

        -- Haskell
        either error (writeFileIfDiffer ("out/haskell/" ++ toPath name ++ ".hs") . LText.unpack) $ dataHs >>= (\t -> eitherRenderWith mylib t env)
        -- Purescript
        either error (writeFileIfDiffer ("out/purescript/" ++ toPath name ++ ".purs") . LText.unpack) $ dataPs >>= (\t -> eitherRenderWith mylib t env)
        -- C++
        either error (writeFileIfDiffer ("out/cpp/" ++ name ++ "2.hpp") . LText.unpack) $ dataHpp2 >>= (\t -> eitherRenderWith mylib t env)
        either error (writeFileIfDiffer ("out/cpp/" ++ name ++ ".hpp") . LText.unpack) $ dataHpp >>= (\t -> eitherRenderWith mylib t env)
        either error (writeFileIfDiffer ("out/cpp/" ++ name ++ ".cpp") . LText.unpack) $ dataCpp >>= (\t -> eitherRenderWith mylib t env)
        -- Java
        forM_ [a | a@DataDef{} <- def {-TODO-}] $ \d -> do
          let env = fromPairs
                [ "def"         .= d
                , "moduleName"  .= name
                , "dateTime"    .= dt
                , "imports"     .= imports
                ]
              fname = "out/java/" ++ toPath name ++ "/" ++ dataName d ++ ".java"
          either error (writeFileIfDiffer fname . LText.unpack) $ dataJava >>= (\t -> eitherRenderWith mylib t env)
        either error (writeFileIfDiffer ("out/java/" ++ toPath name ++ "/JSON.java") . LText.unpack) $ jsonJava >>= (\t -> eitherRenderWith mylib t env)
        -- C#
        either error (writeFileIfDiffer ("out/csharp/" ++ name ++ ".cs") . LText.unpack) $ dataCs >>= (\t -> eitherRenderWith mylib t env)
        -- Swift
        either error (writeFileIfDiffer ("out/swift/" ++ name ++ ".swift") . LText.unpack) $ dataSwift >>= (\t -> eitherRenderWith mylib t env)
  mapM_ generate $ execWriter modules

  -- install files
  copyFile "out/haskell/LambdaCube/IR.hs"             "src-generated/LambdaCube/IR.hs"
  copyFile "out/haskell/LambdaCube/Mesh.hs"           "src-generated/LambdaCube/Mesh.hs"
  copyFile "out/haskell/LambdaCube/PipelineSchema.hs" "src-generated/LambdaCube/PipelineSchema.hs"
  copyFile "out/haskell/LambdaCube/TypeInfo.hs"       "src-generated/LambdaCube/TypeInfo.hs"
