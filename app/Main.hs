module Main where

import Arborist.Reexports (runDeleteEmptyHidingImports, runDeleteEmptyImports, runReplaceReexports)
import Diagnostics.Fixes (runAllFixes)
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opt
import Scripts.DirCycles (runDetectCycles, runRenameModule, runRenameModulePrefix)
import Scripts.DumpRenamedAst (DumpRenamedAstOptions (..), runDumpRenamedAst)

data Command
  = DetectCycles
  | ReplaceReexports
  | AllFixes
  | RenameModule
  | RenameModulePrefix
  | DeleteEmptyImports
  | DeleteEmptyHidingImports
  | DumpRenamedAst DumpRenamedAstOptions

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
