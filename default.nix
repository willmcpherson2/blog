let
  inherit (import <nixpkgs> { }) symlinkJoin;
  inherit
    (import
      (builtins.fetchTarball
        "https://github.com/willmcpherson2/nixture/archive/aa5a7df9d51f55823ccba0755ff3026598e1f905.tar.gz"))
    ghc ghcjs optimiseHaskellJs compileNixture;
  parss =
    ghcjs.callCabal2nix
      "parss"
      (builtins.fetchTarball "https://github.com/willmcpherson2/parss/archive/4673e197e5036e370e53fffe962fcea72c28af62.tar.gz")
      { };
  two-hand =
    ghcjs.callCabal2nix
      "two-hand"
      (builtins.fetchTarball "https://github.com/willmcpherson2/two-hand/archive/01f00f61bc5a7c4774f22a57dbc796867677ef5f.tar.gz")
      { inherit parss; };
in
symlinkJoin {
  name = "willmcpherson2.com";
  paths = [
    (ghc.callCabal2nix "server" ./server { })
    (optimiseHaskellJs (ghcjs.callCabal2nix "hs" ./client/hs { inherit two-hand; }))
    (compileNixture ./client)
  ];
}
