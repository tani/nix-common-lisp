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
        lispLib = pkgs.sbcl.buildASDFSystem {
          inherit pname version lispLibs;
          src = ./.;
          systems = [pname "${pname}/test"];
        };
        lispMainApp = pkgs.sbcl.withPackages (ps: lispLibs);
        lispMainExe = pkgs.stdenv.mkDerivation {
          inherit pname version;
           src = ./.;
           nativeBuildInputs = [ lispMainApp pkgs.makeWrapper ];
      	   dontStrip = true;
           buildPhase = ''
             ${lispMainApp}/bin/sbcl --eval '(require :asdf)' --eval '(in-package :asdf)' --load "$src/${pname}.asd" --eval "(asdf:make :${pname})"
           '';
           installPhase = ''
	           install -D ${pname} $out/bin/${pname}
	         '';
        };
        lispTestApp = pkgs.sbcl.withPackages (ps: [lispLib]);
        lispTestExe = pkgs.writeShellScriptBin "${pname}-test" ''
          ${lispMainApp}/bin/sbcl --noinform --non-interactive --eval '(require :asdf)' --eval "(asdf:test-system :${pname})" "$@"
        '';
      in {
        packages.default = lispLib;
        devShells.default = pkgs.mkShell {
          packages = [lispTestApp];
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
