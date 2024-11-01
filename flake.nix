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
  flake-parts.lib.mkFlake { inherit inputs; } {
    systems = import systems;
    perSystem = { pkgs, ... }: let
      ############ Settings ############
      ## Project name
      pname = "fibonacci";
      ## Project version
      version = "0.0.0";
      ## Source directory
      src = ./.;
      ## Exported systems
      systems = [pname "${pname}/test"];
      ## Dependencies
      mkLispLibs = lisp: with lisp.pkgs; [
        parachute
      ];
      ##################################
      sbcl = rec {
        lispLibs = mkLispLibs pkgs.sbcl;
        mainLib = pkgs.sbcl.buildASDFSystem { inherit pname version src systems lispLibs; };
        mainExe = let app = pkgs.sbcl.withPackages (ps: lispLibs); in
          pkgs.stdenv.mkDerivation {
            inherit pname version src;
            nativeBuildInputs = [app];
            dontStrip = true;
            buildPhase = ''
              export HOME=$TMPDIR
              export CL_SOURCE_REGISTRY="$src"
              export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}`
              ${app}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:make :${pname})"
            '';
            installPhase = ''
              install -D $CL_BUILD_PATHNAME $out/bin/${pname}
            '';
          };
        testExe = let app = pkgs.sbcl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin "${pname}-test" ''${app}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :${pname})"'';
      };
      abcl = rec {
        lispLibs = mkLispLibs pkgs.abcl;
        mainLib = pkgs.abcl.buildASDFSystem { inherit pname version src systems lispLibs; };
        mainExe = let app = pkgs.abcl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin pname ''${app}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)"'';
        testExe = let app = pkgs.abcl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin "${pname}-test" ''${app}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"'';
      };
      ecl = rec {
        lispLibs = mkLispLibs pkgs.ecl;
        mainLib = pkgs.ecl.buildASDFSystem { inherit pname version src systems lispLibs; };
        mainExe = let app = pkgs.ecl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin pname ''${app}/bin/ecl --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)"'';
        testExe = let app = pkgs.ecl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin "${pname}-test" ''${app}/bin/ecl --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"'';
      };
      apps = lisp: pkg: {
        ${"main-" + lisp} = {
          type = "app";
          program = pkg.mainExe;
        };
        ${"test-" + lisp} = {
          type = "app";
          program = pkg.testExe;
        };
      };
      packages = lisp: pkg: {
        ${"lib-" + lisp} = pkg.mainLib;
      };
    in {
      devShells.default = pkgs.mkShell {
        packages = [
          pkgs.rlwrap
          (pkgs.sbcl.withPackages (ps: sbcl.lispLibs))
          (pkgs.abcl.withPackages (ps: abcl.lispLibs))
          (pkgs.ecl.withPackages (ps: ecl.lispLibs))
        ];
        shellHook = ''
          export CL_SOURCE_REGISTRY=$PWD
        '';
      };
      packages =
        (packages "sbcl" sbcl)
        // (packages "abcl" abcl)
        // (packages "ecl" ecl);
      apps =
        (apps "sbcl" sbcl)
        // (apps "abcl" abcl)
        // (apps "ecl" ecl);
    };
  };
}
