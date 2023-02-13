let
  shell = import ./shell.nix;
  inherit (import <nixpkgs> { }) stdenv closurecompiler;
  server = shell.ghc.server;
  client = shell.ghcjs.client;
in
stdenv.mkDerivation {
  name = "willmcpherson2";
  src = ./www;
  dontUnpack = true;
  buildInputs = [ closurecompiler ];
  installPhase = ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    cp -r $src/* $out/static
    for dir in $(find ${client} -name "*.jsexe"); do
      filename="$(basename "$dir" .jsexe).js"
      closure-compiler \
        --warning_level QUIET \
        --compilation_level ADVANCED_OPTIMIZATIONS \
        --jscomp_off=checkVars \
        --externs=$dir/all.js.externs \
        $dir/all.js > $out/static/$filename
    done
  '';
}
