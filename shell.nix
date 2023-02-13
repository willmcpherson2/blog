let
  reflex = import
    (builtins.fetchTarball {
      url = "https://github.com/reflex-frp/reflex-platform/archive/123a6f487ca954fd983f6d4cd6b2a69d4c463d10.tar.gz";
    })
    { };
  haskell-language-server = (import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/0756b8a7bf25e7a5b4fbfa578e95a28cb5ef6380.tar.gz";
    })
    { }).haskell-language-server.override { supportedGhcVersions = [ "865" ]; };
in
reflex.project ({ pkgs, ... }: {
  packages = {
    markup = ./markup;
    server = ./server;
    client = ./client;
    parss = ./parss;
    tulip = ./tulip;
  };

  shells = {
    ghc = [ "markup" "server" "client" "parss" "tulip" ];
    ghcjs = [ "markup" "client" "parss" "tulip" ];
  };

  shellToolOverrides = ghc: super: {
    inherit haskell-language-server;
    inherit (pkgs) entr;
    server-run = pkgs.writeShellScriptBin "server-run" ''
      cabal new-run -fdev server -- static
    '';
    client-build = pkgs.writeShellScriptBin "client-build" ''
      cabal new-build --ghcjs -fdev client
      mkdir -p static
      cp -r www/* static
      for dir in $(find dist-newstyle -name "*.jsexe"); do
        filename="$(basename "$dir" .jsexe).js"
        cp "$dir/all.js" "static/$filename"
      done
    '';
    server-reload = pkgs.writeShellScriptBin "server-reload" ''
      find server | entr -rs server-run
    '';
    client-reload = pkgs.writeShellScriptBin "client-reload" ''
      find markup client www parss tulip | entr -s client-build
    '';
  };

  useWarp = true;
})
