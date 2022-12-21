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
    parss = ./parss;
    tulip = ./tulip;
  };

  shells = {
    ghc = [ "common" "server" "client" "parss" "tulip" ];
    ghcjs = [ "common" "client" "parss" "tulip" ];
  };

  shellToolOverrides = ghc: super: {
    inherit haskell-language-server;
    inherit (pkgs) entr;
    server-run = pkgs.writeShellScriptBin "server-run" ''
      cabal new-run -fdev server -- static
    '';
    client-build = pkgs.writeShellScriptBin "client-build" ''
      cabal new-build --ghcjs -fdev client
      alljs="$(find dist-newstyle -type f -name all.js)"
      mkdir -p static
      cp -r www/* static
      cp $alljs static
    '';
    server-reload = pkgs.writeShellScriptBin "server-reload" ''
      find common server www | entr -rs server-run
    '';
    client-reload = pkgs.writeShellScriptBin "client-reload" ''
      find common client www | entr -s client-build
    '';
  };

  useWarp = true;
})
