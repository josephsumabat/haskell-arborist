# Keep this up to date with cabal.project
let
  tree-sitter-simple-repo = {
    url = "https://github.com/josephsumabat/tree-sitter-simple";
    sha256 = "sha256-fY777r1TvqS17Gz5Ew2syWO7gYGmXRLaCMzvYWJwWYk=";
    rev = "a97da04ac45ff4199fe845d1debd56a4a3d4f038";
    fetchSubmodules = true;
  };

in
  self: super: {
    ghcVersion = "ghc963";

    all-cabal-hashes =
        # Update revision to match required hackage
        super.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/59b02844f778d7cc71d2a62ee05c37e17b396051.tar.gz";
          sha256 = "sha256-NUuQW59vzpXufNpAq4qwx5R0/2TwgDgtapjDSdIybhQ=";
    };

    # Generate parser.c from grammar.js for tree-sitter-haskell.
    # The upstream tek/tree-sitter-haskell repo gitignores parser.c since it's
    # auto-generated, so we must produce it before the Haskell build.
    tree-sitter-haskell-src = let
      base = super.fetchgit tree-sitter-simple-repo;
    in super.runCommand "tree-sitter-haskell-src" {
      nativeBuildInputs = [ super.nodejs super.tree-sitter ];
    } ''
      cp -r ${base}/tree-sitter-haskell $out
      chmod -R u+w $out
      cd $out/vendor/tree-sitter-haskell
      tree-sitter generate --no-bindings
    '';

    haskellPackages = super.haskell.packages.${self.ghcVersion}.override {
      overrides = haskellSelf: haskellSuper: {
        tree-sitter-haskell =
          haskellSuper.callCabal2nix
          "tree-sitter-haskell" self.tree-sitter-haskell-src {};

        tree-sitter-simple =
          haskellSuper.callCabal2nix
          "tree-sitter-simple" "${(super.fetchgit tree-sitter-simple-repo)}/tree-sitter-simple" {};

        tree-sitter-ast =
          haskellSuper.callCabal2nix
          "tree-sitter-ast" "${(super.fetchgit tree-sitter-simple-repo)}/tree-sitter-ast" {};

        haskell-ast =
          self.haskellPackages.callCabal2nix
          "haskell-ast" "${(super.fetchgit tree-sitter-simple-repo)}/haskell-ast" {};

        text-range =
          haskellSuper.callCabal2nix
          "text-range" "${(super.fetchgit tree-sitter-simple-repo)}/text-range" {};

        tasty-expect = haskellSuper.callCabal2nix "tasty-expect" (super.fetchgit {
          url = "https://github.com/oberblastmeister/tasty-expect.git";
          sha256 = "sha256-KxunyEutnwLeynUimiIkRe5w/3IdmbD/9hGVi68UVfU=";
          rev = "ec14d702660c79a907e9c45812958cd0df0f036f";

        }) {};

      };
    };
  }
