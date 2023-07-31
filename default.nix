let
  inherit (import <nixpkgs> { }) symlinkJoin;
  inherit
    (import
      (builtins.fetchTarball
        "https://github.com/willmcpherson2/nixture/archive/aa5a7df9d51f55823ccba0755ff3026598e1f905.tar.gz"))
    ghc compileNixture;
in
symlinkJoin {
  name = "willmcpherson2.com";
  paths = [
    (ghc.callCabal2nix "server" ./server { })
    (compileNixture ./client)
  ];
}
