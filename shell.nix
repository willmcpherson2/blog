let
  reflex = import
    (builtins.fetchTarball {
      url = "https://github.com/reflex-frp/reflex-platform/archive/123a6f487ca954fd983f6d4cd6b2a69d4c463d10.tar.gz";
    })
    { };
  haskell-language-server = (import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/bed08131cd29a85f19716d9351940bdc34834492.tar.gz";
    })
    { }).haskell-language-server.override { supportedGhcVersions = [ "865" ]; };
in
reflex.project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    server = ./server;
    client = ./client;
  };

  shells = {
    ghc = [ "common" "server" "client" ];
    ghcjs = [ "common" "client" ];
  };

  shellToolOverrides = ghc: super: {
    inherit haskell-language-server;
    inherit (pkgs) entr;
    server-reload = pkgs.writeShellScriptBin "server-reload" ''
      ghcid \
        --run \
        --warnings \
        --command "cabal new-repl -fdev server" \
        --setup ":set args $(find "$PWD/dist-newstyle" -type d -name client.jsexe)"
    '';
    client-reload = pkgs.writeShellScriptBin "client-reload" ''
      find client | entr cabal new-build --ghcjs -fdev client
    '';
  };

  useWarp = true;
})
