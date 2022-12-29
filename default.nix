let
  shell = import ./shell.nix;
  inherit (import <nixpkgs> { }) stdenv closurecompiler;
  server = shell.ghc.server;
  client = shell.ghcjs.client;
in
stdenv.mkDerivation {
  name = "willmcpherson2.com";
  src = ./www;
  dontUnpack = true;
  buildInputs = [ closurecompiler ];
  installPhase = ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    cp -r $src/* $out/static
    closure-compiler \
      --warning_level QUIET \
      --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/client.jsexe/all.js.externs \
      ${client}/bin/client.jsexe/all.js > $out/static/all.js
  '';
}
