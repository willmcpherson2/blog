# You need:
# heroku
# docker
# heroku login
# heroku container:login

let
  willmcpherson2 = import ./docker.nix;
  inherit (import <nixpkgs> { }) writeShellScriptBin;
in
writeShellScriptBin "deploy" ''
  docker load -i "${willmcpherson2}"
  docker push registry.heroku.com/willmcpherson2/web
  heroku container:release -a willmcpherson2 web
''
