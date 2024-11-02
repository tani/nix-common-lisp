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
      ## Non-Lisp dependencies
      nativeLibs = with pkgs; [
      ];
      ##################################
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
      sbcl = rec {
        lispLibs = mkLispLibs pkgs.sbcl;
        mainLib = pkgs.sbcl.buildASDFSystem { inherit pname version src systems lispLibs nativeLibs; };
        mainExe = let app = pkgs.sbcl.withPackages (ps: lispLibs); in
          pkgs.stdenv.mkDerivation {
            inherit pname version src;
            meta.mainProgram = pname;
            nativeBuildInputs = [app];
            dontStrip = true;
            buildPhase = ''
              export HOME=$TMPDIR
              export CL_SOURCE_REGISTRY=$src
              export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}_raw`
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${app}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:make :${pname})"
            '';
            installPhase = ''
              install -D $CL_BUILD_PATHNAME $out/bin/${pname}_raw
              cat > $out/bin/${pname} <<-EOF
                #!/bin/sh
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                exec $out/bin/${pname}_raw "\$@"
              EOF
              chmod +x $out/bin/${pname}
            '';
          };
        testExe = let app = pkgs.sbcl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            ${app}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :${pname})"
          '';
      };
      abcl = rec {
        lispLibs = mkLispLibs pkgs.abcl;
        mainLib = pkgs.abcl.buildASDFSystem { inherit pname version src systems lispLibs nativeLibs; };
        mainExe = let app = pkgs.abcl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin pname ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            ${app}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)" -- "$@"
          '';
        testExe = let app = pkgs.abcl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            ${app}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"
          '';
      };
      ecl = rec {
        lispLibs = mkLispLibs pkgs.ecl;
        mainLib = pkgs.ecl.buildASDFSystem { inherit pname version src systems lispLibs nativeLibs; };
        mainExe = let app = pkgs.ecl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin pname ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            ${app}/bin/ecl --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)" -- "$@"
          '';
        testExe = let app = pkgs.ecl.withPackages (ps: [mainLib]); in
          pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            ${app}/bin/ecl --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"
          '';
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
      devPackages = lisp: pkg:
        pkgs.${lisp}.withPackages (ps: [pkg.mainLib]);
    in {
      devShells.default = pkgs.mkShell {
        inherit LD_LIBRARY_PATH;
        shellHook = ''
          ## Add the current directory to the CL_SOURCE_REGISTRY
          ## so that the Lisp implementation can find the source files
          export CL_SOURCE_REGISTRY=$PWD
        '';
        packages = [
          (devPackages "sbcl" sbcl)
          (devPackages "abcl" abcl)
          (devPackages "ecl" ecl)
        ];
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
