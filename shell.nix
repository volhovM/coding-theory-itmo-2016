with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "coding-theory-itmo2016";
     ghc = hsPkgs.ghc;
     buildInputs = [ zlib git openssh ];
     LANG = "en_US.UTF-8";
  }
