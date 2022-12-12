with (import <nixpkgs> { });
with (import ./. { });
let
  server = ghc.server;
  client = ghcjs.client;
  index = concatText "index.html" [ ./index.html ];
in
runCommand "blog" { } ''
  mkdir -p $out/{bin,static}
  cp ${server}/bin/* $out/bin
  cp ${index} $out/static/index.html
  ${closurecompiler}/bin/closure-compiler \
    --warning_level QUIET \
    --compilation_level ADVANCED_OPTIMIZATIONS \
    --jscomp_off=checkVars \
    --externs=${client}/bin/client.jsexe/all.js.externs \
    ${client}/bin/client.jsexe/all.js > $out/static/all.js
''
