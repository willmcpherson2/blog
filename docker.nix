let
  willmcpherson2 = import ./.;
  inherit (import <nixpkgs> { }) dockerTools pkgs;
in
dockerTools.buildImage {
  name = "registry.heroku.com/willmcpherson2/web";
  tag = "latest";
  copyToRoot = pkgs.buildEnv {
    name = "willmcpherson2";
    paths = [
      willmcpherson2
      pkgs.bashInteractive
    ];
  };
  config = {
    Cmd = [ "/bin/server" "/static" ];
  };
}
