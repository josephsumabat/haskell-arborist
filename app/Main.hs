module Main where

import Arborist.Reexports (runDeleteEmptyHidingImports, runDeleteEmptyImports, runReplaceReexports)
import BuildGraph.Directory
  ( DirName (..)
  , buildGraphFromDirectoriesWithRecursiveTargets
  , graphToOutput
  , renderBuildGraphError
  )
import BuildGraph.GroupCandidates (groupOutputCandidates)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text qualified as T
import Diagnostics.Fixes (runAllFixes)
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opt
import Scripts.DirCycles (runDetectCycles, runRenameModule, runRenameModulePrefix)
import Scripts.DumpRenamedAst (DumpRenamedAstOptions (..), runDumpRenamedAst)
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

data DumpTargetGraphOptions = DumpTargetGraphOptions
  { rootDir :: FilePath
  , srcDirs :: [FilePath]
  , recursiveTargetDirs :: [FilePath]
  }

data GroupCandidatesOptions = GroupCandidatesOptions
  { candidatesInput :: Maybe FilePath
  , candidatesDirectories :: [DirName]
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
        <*> Opt.optional
          ( Opt.strOption
              ( Opt.long "config"
                  <> Opt.metavar "FILE"
                  <> Opt.help "Override path to the Arborist configuration file"
              )
          )

    dumpTargetGraphOptionsParser :: Parser DumpTargetGraphOptions
    dumpTargetGraphOptionsParser =
      DumpTargetGraphOptions
        <$> Opt.strOption
          ( Opt.long "root"
              <> Opt.metavar "DIR"
              <> Opt.help "Root directory containing the listed source directories"
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

runDumpTargetGraph :: DumpTargetGraphOptions -> IO ()
runDumpTargetGraph DumpTargetGraphOptions {rootDir, srcDirs, recursiveTargetDirs} = do
  result <- buildGraphFromDirectoriesWithRecursiveTargets rootDir srcDirs recursiveTargetDirs
  case result of
    Left err -> die (renderBuildGraphError err)
    Right graph -> BL8.putStrLn (Aeson.encode (graphToOutput graph))

runGroupCandidates :: GroupCandidatesOptions -> IO ()
runGroupCandidates GroupCandidatesOptions {candidatesInput, candidatesDirectories} = do
  bytes <-
    case candidatesInput of
      Nothing -> BL8.getContents
      Just path -> BL8.readFile path
  case Aeson.eitherDecode bytes of
    Left err -> die ("Failed to parse BuildGraphOutput: " <> err)
    Right graph ->
      BL8.putStrLn (Aeson.encode (groupOutputCandidates graph candidatesDirectories))
