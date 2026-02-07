# haskell-arborist

**haskell-arborist is not ready for production use - the API may change at any time and the command line interface is not stable/intended for use**

haskell-arborist is a library for analysing and refactoring large Haskell codebases. It parses modules with `tree-sitter`, runs its own renaming pipeline to model scopes, and produces precise source edits for managing imports, exports, and module layouts.
