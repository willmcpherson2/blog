{ pkgs ? import <nixpkgs> { } }:
let
  nixture =
    (import
      (builtins.fetchTarball
        "https://github.com/willmcpherson2/nixture/archive/e77d4ef8afd94fe0acddf689fde2d3d59f3b612a.tar.gz")
      { });
in
pkgs.symlinkJoin {
  name = "blog";
  paths = [
    (nixture.ghc.callCabal2nix "server" ./server { })
    (nixture.compileNixture ./client)
  ];
}
