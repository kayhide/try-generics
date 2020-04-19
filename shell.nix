# { pkgs ? import <nixpkgs> {}
# }:

# let
#   ghc = pkgs.haskell.compiler.ghc865;

# in

# pkgs.mkShell {
#   buildInputs = with pkgs; [
#     ghc
#     stack
#     gmp
#   ];
# }
let
  pkgs = import ./. {};

in

pkgs.shellFor {
  buildInputs = [
    pkgs.doctest.components.exes.doctest
  ];
}
