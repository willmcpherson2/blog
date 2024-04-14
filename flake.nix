{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      server = pkgs.static-web-server;
      static = pkgs.runCommand "build.sh" {
        buildInputs = [
          pkgs.emacs29
          (pkgs.writeShellScriptBin "build.sh" (builtins.readFile ./build.sh))
        ];
      } ''
        build.sh ${./init.el} ${./client} $out/static
      '';
    in {
      packages.x86_64-linux.default = pkgs.symlinkJoin {
        name = "blog";
        paths = [ server static ];
      };
    };
}
