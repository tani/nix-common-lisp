{
  description = "A flake for fibonacci";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    systems,
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import systems;
      perSystem = {pkgs, ...}: let
        pname = "fibonacci";
        version = "0.0.0";
        lispLibs = with pkgs.sbcl.pkgs; [
          parachute
        ];
        lispMainLib = pkgs.sbcl.buildASDFSystem {
          inherit pname version lispLibs;
          src = ./.;
          systems = [pname "${pname}/test"];
        };
        lispMainApp = pkgs.sbcl.withPackages (ps: lispLibs);
        lispMainExe = pkgs.stdenv.mkDerivation {
          inherit pname version;
          src = ./.;
          nativeBuildInputs = [ lispMainApp ];
          dontStrip = true;
          buildPhase = ''
            export HOME=$TMPDIR
            export CL_SOURCE_REGISTRY="$src"
            export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}`
            ${lispMainApp}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:make :${pname})"
          '';
          installPhase = ''
            install -D $CL_BUILD_PATHNAME $out/bin/${pname}
          '';
        };
        lispTestApp = pkgs.sbcl.withPackages (ps: [lispMainLib]);
        lispTestExe = pkgs.writeShellScriptBin "${pname}-test" ''
          ${lispTestApp}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :${pname})"
        '';
      in {
        packages.default = lispMainLib;
        devShells.default = pkgs.mkShell {
          packages = [lispTestApp lispMainExe];
        };
        apps = {
          default = {
            type = "app";
            program = lispMainExe;
          };
          test = {
            type = "app";
            program = lispTestExe;
          };
        };
      };
    };
}
