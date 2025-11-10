module Main where

import Arborist.Reexports (runDeleteEmptyHidingImports, runDeleteEmptyImports, runReplaceReexports)
import BuildGraph.Directory
  ( BuildGraphOutput (..)
  , DirName (..)
  , TargetKeyOutput (..)
  , TargetOutput (..)
  , buildGraphFromDirectoriesWithRecursiveTargets
  , graphToOutput
  , renderBuildGraphError
  )
import BuildGraph.GroupCandidates (groupOutputCandidates)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Diagnostics.Fixes (runAllFixes)
import Data.List.NonEmpty qualified as NE
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opt
import Scripts.DirCycles (runDetectCycles, runRenameModule, runRenameModulePrefix)
import Scripts.DumpRenamedAst (DumpRenamedAstOptions (..), runDumpRenamedAst)
import Scripts.ModuleFiles (ModuleFilesOptions (..), runModuleFiles)
import Scripts.RequiredTargetFiles (RequiredTargetFilesOptions (..), runRequiredTargetFiles)
import System.Exit (die)

data Command
  = DetectCycles
  | ReplaceReexports
  | AllFixes
  | RenameModule
  | RenameModulePrefix
  | DeleteEmptyImports
  | DeleteEmptyHidingImports
  | DumpRenamedAst DumpRenamedAstOptions
  | DumpTargetGraph DumpTargetGraphOptions
  | GroupCandidates GroupCandidatesOptions
  | ModuleFiles ModuleFilesOptions
  | RequiredTargetFiles RequiredTargetFilesOptions

data DumpTargetGraphOptions = DumpTargetGraphOptions
  { srcRootDir :: FilePath
  , rootBuckDir :: FilePath
  , srcDirs :: [FilePath]
  , recursiveTargetDirs :: [FilePath]
  , overrideMapPath :: Maybe FilePath
  }

data GroupCandidatesOptions = GroupCandidatesOptions
  { candidatesInput :: Maybe FilePath
  , candidatesDirectories :: [DirName]
  , groupRecursive :: Bool
  , groupAll :: Bool
  }

main :: IO ()
main = Opt.execParser parserInfo >>= runCommand

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  DetectCycles -> runDetectCycles
  ReplaceReexports -> runReplaceReexports
  AllFixes -> runAllFixes
  RenameModule -> runRenameModule
  RenameModulePrefix -> runRenameModulePrefix
  DeleteEmptyImports -> runDeleteEmptyImports
  DeleteEmptyHidingImports -> runDeleteEmptyHidingImports
  DumpRenamedAst opts -> runDumpRenamedAst opts
  DumpTargetGraph opts -> runDumpTargetGraph opts
  GroupCandidates opts -> runGroupCandidates opts
  ModuleFiles opts -> runModuleFiles opts
  RequiredTargetFiles opts -> runRequiredTargetFiles opts

parserInfo :: ParserInfo Command
parserInfo =
  Opt.info (commandParser Opt.<**> Opt.helper) $
    Opt.fullDesc
      <> Opt.progDesc "Run Arborist maintenance scripts"
      <> Opt.header "haskell-arborist"

commandParser :: Parser Command
commandParser =
  Opt.hsubparser $
    command "detect-cycles" DetectCycles "Detect directory cycles using default configuration"
      <> command "replace-reexports" ReplaceReexports "Rewrite modules to avoid reexports"
      <> command "all-fixes" AllFixes "Apply all available diagnostics fixes"
      <> command "rename-module" RenameModule "Rename a module via interactive prompt"
      <> command "rename-module-prefix" RenameModulePrefix "Rename a module prefix via interactive prompt"
      <> command "delete-empty-imports" DeleteEmptyImports "Remove empty import declarations"
      <> command "delete-empty-hiding-imports" DeleteEmptyHidingImports "Remove import declarations with empty hiding clauses"
      <> Opt.command "dump-renamed-ast"
        ( Opt.info (DumpRenamedAst <$> dumpRenamedAstOptionsParser)
            (Opt.progDesc "Write the renamed AST for a source file to disk")
        )
      <> Opt.command "dump-target-graph"
        ( Opt.info (DumpTargetGraph <$> dumpTargetGraphOptionsParser)
            (Opt.progDesc "Emit the maximal acyclic directory target graph as JSON")
        )
      <> Opt.command "group-candidates"
        ( Opt.info (GroupCandidates <$> groupCandidatesOptionsParser)
            (Opt.progDesc "Merge eligible module targets in an existing BuildGraphOutput")
        )
      <> Opt.command "module-files"
        ( Opt.info (ModuleFiles <$> moduleFilesOptionsParser)
            (Opt.progDesc "Resolve modules to source files and emit a JSON object mapping them")
        )
      <> Opt.command "required-target-files"
        ( Opt.info (RequiredTargetFiles <$> requiredTargetFilesOptionsParser)
            (Opt.progDesc "List unique source files directly imported by the provided modules")
        )
      <> Opt.metavar "COMMAND"
  where
    command name cmd desc =
      Opt.command name $
        Opt.info (pure cmd) (Opt.progDesc desc)

    dumpRenamedAstOptionsParser :: Parser DumpRenamedAstOptions
    dumpRenamedAstOptionsParser =
      DumpRenamedAstOptions
        <$> Opt.strOption
          ( Opt.long "source"
              <> Opt.short 's'
              <> Opt.metavar "FILE"
              <> Opt.help "Path to the Haskell source file to process"
          )
        <*> Opt.optional
          ( Opt.strOption
              ( Opt.long "output"
                  <> Opt.short 'o'
                  <> Opt.metavar "FILE"
                  <> Opt.help "Destination file for the renamed AST dump (defaults to stdout)"
              )
          )
        <*> Opt.strOption
          ( Opt.long "config"
              <> Opt.metavar "FILE"
              <> Opt.help "Path to the Arborist configuration JSON"
          )

    dumpTargetGraphOptionsParser :: Parser DumpTargetGraphOptions
    dumpTargetGraphOptionsParser =
      DumpTargetGraphOptions
        <$> Opt.strOption
          ( Opt.long "src-root"
              <> Opt.metavar "DIR"
              <> Opt.help "Root directory containing the listed source directories"
          )
        <*> Opt.strOption
          ( Opt.long "root"
              <> Opt.metavar "DIR"
              <> Opt.help "Directory containing the root-level BUCK file"
          )
        <*> Opt.many
          ( Opt.strArgument
              ( Opt.metavar "[DIRECTORY...]"
                  <> Opt.help "Optional source directories containing Haskell modules (defaults to the root directory)"
              )
          )
        <*> Opt.many
          ( Opt.strOption
              ( Opt.long "recursive-target"
                  <> Opt.metavar "DIR"
                  <> Opt.help "Directory to treat as a recursive target (may be provided multiple times)"
              )
          )
        <*> Opt.optional
          ( Opt.strOption
              ( Opt.long "module-target-overrides"
                  <> Opt.metavar "FILE"
                  <> Opt.help "JSON file containing module target overrides"
              )
          )

    moduleFilesOptionsParser :: Parser ModuleFilesOptions
    moduleFilesOptionsParser =
      ModuleFilesOptions
        <$> Opt.optional
          ( Opt.strOption
              ( Opt.long "config"
                  <> Opt.metavar "FILE"
                  <> Opt.help "Optional path to the Arborist JSON configuration"
              )
          )
        <*> ( NE.fromList . map T.pack
                <$> Opt.some
                  ( Opt.strArgument
                      ( Opt.metavar "MODULE..."
                          <> Opt.help "One or more module names to resolve"
                      )
                  )
            )

    requiredTargetFilesOptionsParser :: Parser RequiredTargetFilesOptions
    requiredTargetFilesOptionsParser =
      RequiredTargetFilesOptions
        <$> Opt.optional
          ( Opt.strOption
              ( Opt.long "config"
                  <> Opt.metavar "FILE"
                  <> Opt.help "Optional path to the Arborist JSON configuration"
              )
          )
        <*> ( NE.fromList . map T.pack
                <$> Opt.some
                  ( Opt.strArgument
                      ( Opt.metavar "MODULE..."
                          <> Opt.help "One or more module names to analyze"
                      )
                  )
            )

    groupCandidatesOptionsParser :: Parser GroupCandidatesOptions
    groupCandidatesOptionsParser =
      GroupCandidatesOptions
        <$> Opt.optional
          ( Opt.strOption
              ( Opt.long "input"
                  <> Opt.short 'i'
                  <> Opt.metavar "FILE"
                  <> Opt.help "Path to BuildGraphOutput JSON (defaults to stdin)"
              )
          )
        <*> Opt.many
          ( DirName . T.pack
              <$> Opt.strOption
                ( Opt.long "directory"
                    <> Opt.short 'd'
                    <> Opt.metavar "TARGET"
                    <> Opt.help "Directory target to restrict merging (may be provided multiple times)"
                )
          )
        <*> Opt.switch
          ( Opt.long "group-recursive"
              <> Opt.help "Automatically group directories that were emitted as recursive targets"
          )
        <*> Opt.switch
          ( Opt.long "group-all"
              <> Opt.help "Attempt to group every directory in the BuildGraphOutput"
          )

runDumpTargetGraph :: DumpTargetGraphOptions -> IO ()
runDumpTargetGraph DumpTargetGraphOptions {srcRootDir, rootBuckDir, srcDirs, recursiveTargetDirs, overrideMapPath} = do
  let effectiveSrcDirs = if null srcDirs then [srcRootDir] else srcDirs
  overrides <-
    case overrideMapPath of
      Nothing -> pure Nothing
      Just path -> do
        bytes <- BL8.readFile path
        case Aeson.eitherDecode bytes of
          Left err -> die ("Failed to parse module target overrides from " <> path <> ": " <> err)
          Right parsed -> pure (Just parsed)
  when (isJust overrides && not (null recursiveTargetDirs)) $
    die "Recursive target directories cannot be provided when module target overrides are in use"

  result <- buildGraphFromDirectoriesWithRecursiveTargets srcRootDir rootBuckDir effectiveSrcDirs recursiveTargetDirs overrides
  case result of
    Left err -> die (renderBuildGraphError err)
    Right graph -> BL8.putStrLn (Aeson.encode (graphToOutput graph))

runGroupCandidates :: GroupCandidatesOptions -> IO ()
runGroupCandidates GroupCandidatesOptions {candidatesInput, candidatesDirectories, groupRecursive, groupAll} = do
  bytes <-
    case candidatesInput of
      Nothing -> BL8.getContents
      Just path -> BL8.readFile path
  case Aeson.eitherDecode bytes of
    Left err -> die ("Failed to parse BuildGraphOutput: " <> err)
    Right graph ->
      let recursiveDirs =
            if groupRecursive
              then map DirName (mapMaybe recursiveTargetDirectory graphTargets)
              else []
          allDirs =
            if groupAll
              then map DirName (Set.toList (Set.fromList (mapMaybe moduleTargetDirectory graphTargets)))
              else []
          requestedDirs =
            Set.toList $ Set.fromList (candidatesDirectories <> recursiveDirs <> allDirs)
       in BL8.putStrLn (Aeson.encode (groupOutputCandidates graph requestedDirs))
     where
      graphTargets = case graph of
        BuildGraphOutput {targets = ts} -> ts
      recursiveTargetDirectory TargetOutput {key = RecursiveDirectoryTargetOutput dir} = Just dir
      recursiveTargetDirectory _ = Nothing
      moduleTargetDirectory TargetOutput {key = ModuleTargetOutput _, directory = dir} = Just dir
      moduleTargetDirectory _ = Nothing
