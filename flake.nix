{
  description = "A flake for fibonacci";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };
  outputs = inputs @ { self, nixpkgs, flake-parts, systems }:
  flake-parts.lib.mkFlake { inherit inputs; } {
    systems = import systems;
    perSystem = { pkgs, system, ... }: let
      ############ Settings ############
      ## Project name
      pname = "fibonacci";
      ## Project version
      version = "0.0.0";
      ## Source directory
      src = ./.;
      ## Exported systems
      systems = [
        pname
        "${pname}/test"
      ];
      ## Dependencies
      lispLibs = lisp: with lisp.pkgs; [
        fiveam
      ];
      ## Non-Lisp dependencies
      nativeLibs = with pkgs; [
      ];
      ## Supported Lisp implementations
      lispImpls = [
        "abcl"
        "sbcl"
        "ecl"
        "ccl"
        "mkcl"
        "clisp"
        "cmucl_binary"
        "clasp-common-lisp"
      ];
      ##################################
      isAvailable = impl: let lisp = pkgs.${impl}; in
        (builtins.tryEval lisp).success && (builtins.elem system lisp.meta.platforms) && (!lisp.meta.broken);
      availableLispImpls = builtins.filter isAvailable lispImpls;
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
      recipe = {
        sbcl = rec {
          mainLib = pkgs.sbcl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.sbcl;
          };
          lisp = pkgs.sbcl.withPackages (ps: [mainLib]);
          mainExe = pkgs.stdenv.mkDerivation {
            inherit pname version src;
            meta.mainProgram = pname;
            dontStrip = true;
            buildPhase = ''
              export HOME=$TMPDIR
              export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}_raw`
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${lisp}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:make :${pname})"
            '';
            installPhase = ''
              install -D $CL_BUILD_PATHNAME $out/bin/${pname}_raw
              cat > $out/bin/${pname} <<-EOF
                #!/bin/sh
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                exec $out/bin/${pname}_raw -- "\$@"
              EOF
              chmod +x $out/bin/${pname}
            '';
          };
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :${pname})"
          '';
        };
        cmucl_binary = rec {
          mainLib = pkgs.cmucl_binary.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.cmucl_binary;
          };
          lisp = pkgs.cmucl_binary.withPackages (ps: [mainLib]);
          mainExe = pkgs.stdenv.mkDerivation {
            inherit pname version src;
            meta.mainProgram = pname;
            dontStrip = true;
            buildPhase = ''
              export HOME=$TMPDIR
              export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}_raw`
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${lisp}/bin/lisp -quiet -eval "(require :asdf)" -eval "(asdf:make :${pname})"
            '';
            installPhase = ''
              install -D $CL_BUILD_PATHNAME $out/bin/${pname}_raw
              cat > $out/bin/${pname} <<-EOF
                #!/bin/sh
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                exec $out/bin/${pname}_raw -- "\$@"
              EOF
              chmod +x $out/bin/${pname}
            '';
          };
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/lisp -quiet -eval "(require :asdf)" -eval "(asdf:test-system :${pname})"
          '';
        };
        ccl = rec {
          mainLib = pkgs.ccl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.ccl;
          };
          lisp = pkgs.ccl.withPackages (ps: [mainLib]);
          mainExe = pkgs.stdenv.mkDerivation {
            inherit pname version src;
            meta.mainProgram = pname;
            dontStrip = true;
            buildPhase = ''
              export HOME=$TMPDIR
              export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}_raw`
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${lisp}/bin/ccl --quiet --eval "(require :asdf)" --eval "(asdf:make :${pname})"
            '';
            installPhase = ''
              install -D $CL_BUILD_PATHNAME $out/bin/${pname}_raw
              cat > $out/bin/${pname} <<-EOF
                #!/bin/sh
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                exec $out/bin/${pname}_raw -- "\$@"
              EOF
              chmod +x $out/bin/${pname}
            '';
          };
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/ccl --quiet --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"
          '';
        };
        clisp = rec {
          mainLib = pkgs.clisp.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.clisp;
          };
          lisp = pkgs.clisp.withPackages (ps: [mainLib]);
          mainExe = pkgs.stdenv.mkDerivation {
            inherit pname version src;
            meta.mainProgram = pname;
            dontStrip = true;
            buildPhase = ''
              export HOME=$TMPDIR
              export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}_raw`
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              ${lisp}/bin/clisp --quiet -x '(require "asdf")' -x "(asdf:make :${pname})"
            '';
            installPhase = ''
              install -D $CL_BUILD_PATHNAME $out/bin/${pname}_raw
              cat > $out/bin/${pname} <<-EOF
                #!/bin/sh
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                exec $out/bin/${pname}_raw -- "\$@"
              EOF
              chmod +x $out/bin/${pname}
            '';
          };
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/clisp --quiet -x '(require "asdf")' -x "(asdf:test-system :${pname})" -x "(quit)"
          '';
        };
        abcl = rec {
          mainLib = pkgs.abcl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.abcl;
          };
          lisp = pkgs.abcl.withPackages (ps: [mainLib]);
          mainExe = pkgs.writeShellScriptBin pname ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)" -- "$@"
          '';
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/abcl --noinform --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"
          '';
        };
        clasp-common-lisp = rec {
          mainLib = pkgs.clasp-common-lisp.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.clasp-common-lisp;
          };
          lisp = pkgs.clasp-common-lisp.withPackages (ps: [mainLib]);
          mainExe = pkgs.writeShellScriptBin pname ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/clasp --noinform --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --quit -- "$@"
          '';
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/clasp --noinform --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --quit
          '';
        };
        ecl = rec {
          mainLib = pkgs.ecl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.ecl;
          };
          lisp = pkgs.ecl.withPackages (ps: [mainLib]);
          mainExe = pkgs.writeShellScriptBin pname ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/ecl --eval "(require :asdf)" --eval "(asdf:load-system :${pname})" --eval "(${pname}:main)" --eval "(quit)" -- "$@"
          '';
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/ecl --eval "(require :asdf)" --eval "(asdf:test-system :${pname})" --eval "(quit)"
          '';
        };
        mkcl = rec {
          mainLib = pkgs.mkcl.buildASDFSystem {
            inherit pname version src systems nativeLibs;
            lispLibs = lispLibs pkgs.mkcl;
          };
          lisp = pkgs.mkcl.withPackages (ps: [mainLib]);
          mainExe = pkgs.writeShellScriptBin pname ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/mkcl -eval "(require :asdf)" -eval "(asdf:load-system :${pname})" -eval "(${pname}:main)" -eval "(quit)" -- "$@"
          '';
          testExe = pkgs.writeShellScriptBin "${pname}-test" ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${lisp}/bin/mkcl -eval "(require :asdf)" -eval "(asdf:test-system :${pname})" -eval "(quit)"
          '';
        };
      };
      apps =  impl: [
        { name = "main-" + impl; value = { type = "app"; program = recipe.${impl}.mainExe; }; }
        { name = "test-" + impl; value = { type = "app"; program = recipe.${impl}.testExe; }; }
      ];
      packages = impl: { name = "lib-" + impl; value = recipe.${impl}.mainLib; };
      devPackages = impl: pkgs.${impl}.withPackages (ps: [recipe.${impl}.mainLib]);
    in {
      devShells.default = pkgs.mkShell {
        inherit LD_LIBRARY_PATH;
        shellHook = ''export CL_SOURCE_REGISTRY=$PWD'';
        packages = builtins.map devPackages availableLispImpls;
      };
      packages = builtins.listToAttrs (builtins.map packages availableLispImpls);
      apps = builtins.listToAttrs (builtins.concatMap apps availableLispImpls);
    };
  };
}
