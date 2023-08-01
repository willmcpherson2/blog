let
  inherit (import <nixpkgs> { }) symlinkJoin;
  inherit
    (import
      (builtins.fetchTarball
        "https://github.com/willmcpherson2/nixture/archive/fd8d95467564540b2d766fdc94d2340d11568a68.tar.gz"))
    ghc compileNixture;
in
symlinkJoin {
  name = "willmcpherson2.com";
  paths = [
    (ghc.callCabal2nix "server" ./server { })
    (compileNixture ./client)
  ];
}
