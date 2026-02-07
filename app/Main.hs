module Main where

import Arborist.Reexports (runDeleteEmptyHidingImports, runDeleteEmptyImports, runReplaceReexports)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Diagnostics.Fixes (DiagnosticsEnvironment (..), runAllFixes)
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opt
import Scripts.DirCycles (runDetectCycles, runRenameModule, runRenameModulePrefix)
import Scripts.DumpRenamedAst (DumpRenamedAstOptions (..), runDumpRenamedAst)
import Scripts.ModuleFiles (ModuleFilesOptions (..), runModuleFiles)
import Scripts.PrintDeps (PrintDepsOptions (..), runPrintDeps)
import Scripts.RequiredTargetFiles (RequiredTargetFilesOptions (..), runRequiredTargetFiles)

data Command
  = DetectCycles
  | ReplaceReexports
  | AllFixes DiagnosticsEnvironment
  | RenameModule
  | RenameModulePrefix
  | DeleteEmptyImports
  | DeleteEmptyHidingImports
  | DumpRenamedAst DumpRenamedAstOptions
  | ModuleFiles ModuleFilesOptions
  | RequiredTargetFiles RequiredTargetFilesOptions
  | PrintDeps PrintDepsOptions

main :: IO ()
main = Opt.execParser parserInfo >>= runCommand

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  DetectCycles -> runDetectCycles
  ReplaceReexports -> runReplaceReexports
  AllFixes opts -> runAllFixes opts
  RenameModule -> runRenameModule
  RenameModulePrefix -> runRenameModulePrefix
  DeleteEmptyImports -> runDeleteEmptyImports
  DeleteEmptyHidingImports -> runDeleteEmptyHidingImports
  DumpRenamedAst opts -> runDumpRenamedAst opts
  ModuleFiles opts -> runModuleFiles opts
  RequiredTargetFiles opts -> runRequiredTargetFiles opts
  PrintDeps opts -> runPrintDeps opts

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
      <> Opt.command
        "all-fixes"
        ( Opt.info
            (AllFixes <$> diagnosticsOptionsParser)
            (Opt.progDesc "Apply diagnostics-driven fixes using a ghcid log")
        )
      <> command "rename-module" RenameModule "Rename a module via interactive prompt"
      <> command "rename-module-prefix" RenameModulePrefix "Rename a module prefix via interactive prompt"
      <> command "delete-empty-imports" DeleteEmptyImports "Remove empty import declarations"
      <> command "delete-empty-hiding-imports" DeleteEmptyHidingImports "Remove import declarations with empty hiding clauses"
      <> Opt.command
        "dump-renamed-ast"
        ( Opt.info
            (DumpRenamedAst <$> dumpRenamedAstOptionsParser)
            (Opt.progDesc "Write the renamed AST for a source file to disk")
        )
      <> Opt.command
        "module-files"
        ( Opt.info
            (ModuleFiles <$> moduleFilesOptionsParser)
            (Opt.progDesc "Resolve modules to source files and emit a JSON object mapping them")
        )
      <> Opt.command
        "required-target-files"
        ( Opt.info
            (RequiredTargetFiles <$> requiredTargetFilesOptionsParser)
            (Opt.progDesc "List unique source files directly imported by the provided modules")
        )
      <> Opt.command
        "print-deps"
        ( Opt.info
            (PrintDeps <$> printDepsOptionsParser)
            (Opt.progDesc "Print the modules imported by a Haskell source file")
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

  diagnosticsOptionsParser :: Parser DiagnosticsEnvironment
  diagnosticsOptionsParser =
    DiagnosticsEnvironment
      <$> Opt.strOption
        ( Opt.long "repo-root"
            <> Opt.metavar "DIR"
            <> Opt.value "."
            <> Opt.showDefault
            <> Opt.help "Repository root used to resolve diagnostic file paths"
        )
      <*> Opt.strOption
        ( Opt.long "ghcid-log"
            <> Opt.metavar "FILE"
            <> Opt.value "ghcid.txt"
            <> Opt.showDefault
            <> Opt.help "Path to the ghcid output log"
        )
      <*> Opt.strOption
        ( Opt.long "main-file"
            <> Opt.metavar "FILE"
            <> Opt.value "app/Main.hs"
            <> Opt.showDefault
            <> Opt.help "Entry-point file referenced by ghcid"
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

  printDepsOptionsParser :: Parser PrintDepsOptions
  printDepsOptionsParser =
    PrintDepsOptions
      <$> Opt.strArgument
        ( Opt.metavar "FILE"
            <> Opt.help "Path to the Haskell source file to analyze"
        )
