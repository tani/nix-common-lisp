{
  description = "A flake for fibonacci";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs @ { self, nixpkgs, flake-parts, systems }:
  flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      flake-parts.flakeModules.easyOverlay
    ];
    systems = nixpkgs.lib.platforms.all;
    perSystem = { config, pkgs, system, final, ... }: let
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
        (builtins.tryEval lisp).success
        && (builtins.elem system lisp.meta.platforms)
        && (!lisp.meta.broken);
      availableLispImpls = builtins.filter isAvailable lispImpls;
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
      nonBundledPackage = { pkg, mainCmd, testCmd }: rec {
        mainLib = pkg.buildASDFSystem {
          inherit pname version src systems nativeLibs;
          lispLibs = lispLibs pkg;
        };
        lisp = pkg.withPackages (ps: [mainLib]);
        mainExe = pkgs.writeShellScriptBin pname ''
          export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
          exec ${mainCmd lisp} -- "$@"
        '';
        testExe = pkgs.writeShellScriptBin "${pname}-test" ''
          export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
          exec ${testCmd lisp}
        '';
      };
      bundledPackage = { pkg, mainCmd, testCmd }: rec {
        mainLib = pkg.buildASDFSystem {
          inherit pname version src systems nativeLibs;
          lispLibs = lispLibs pkg;
        };
        lisp = pkg.withPackages (ps: [mainLib]);
        mainExe = pkgs.stdenv.mkDerivation {
          inherit pname version src;
          meta.mainProgram = pname;
          dontStrip = true;
          buildPhase = ''
            export HOME=$TMPDIR
            export CL_BUILD_PATHNAME=`realpath -s --relative-to=$src $TMPDIR/${pname}_raw`
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            ${mainCmd lisp}
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
        testExe = pkgs.writeShellScriptBin "${pname}-test" ''
          export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
          ${testCmd lisp}
        '';
      };
      coverage-sbcl =
        let
          lisp = pkgs.sbcl.withPackages (ps: lispLibs pkgs.sbcl);
          program = pkgs.writeShellScriptBin "${pname}-coverage" ''
            export CL_SOURCE_REGISTRY=$PWD
            ${lisp}/bin/sbcl --noinform --non-interactive \
              --eval '(require :asdf)' \
              --eval '(require :sb-cover)' \
              --eval '(declaim (optimize sb-cover:store-coverage-data))' \
              --eval '(asdf:compile-system :fibonacci :force t)' \
              --eval '(declaim (optimize (sb-cover:store-coverage-data 0)))' \
              --eval '(asdf:test-system :${pname})' \
              --eval '(sb-cover:report "coverage/")'
          '';
        in
          {
            type = "app";
            inherit program;
          };
      recipe = {
        sbcl = bundledPackage {
          pkg = pkgs.sbcl;
          mainCmd = lisp: "${lisp}/bin/sbcl --noinform --non-interactive --eval '(require :asdf)' --eval '(asdf:make :${pname})'";
          testCmd = lisp: "${lisp}/bin/sbcl --noinform --non-interactive --eval '(require :asdf)' --eval '(asdf:test-system :${pname})'";
        };
        ccl = bundledPackage {
          pkg = pkgs.ccl;
          mainCmd = lisp: "${lisp}/bin/ccl --quiet --eval '(require :asdf)' --eval '(asdf:make :${pname})'";
          testCmd = lisp: "${lisp}/bin/ccl --quiet --eval '(require :asdf)' --eval '(asdf:test-system :${pname})'";
        };
        clisp = bundledPackage {
          pkg = pkgs.clisp;
          mainCmd = lisp: "${lisp}/bin/clisp --quiet -x '(require \"asdf\")' -x '(asdf:make :${pname})'";
          testCmd = lisp: "${lisp}/bin/clisp --quiet -x '(require \"asdf\")' -x '(asdf:test-system :${pname})'";
        };
        cmucl_binary = nonBundledPackage {
          pkg = pkgs.cmucl_binary;
          mainCmd = lisp: "${lisp}/bin/lisp -quiet -eval '(require :asdf)' -eval '(asdf:load-system :${pname})' -eval '(${pname}:main)' -eval '(quit)'";
          testCmd = lisp: "${lisp}/bin/lisp -quiet -eval '(require :asdf)' -eval '(asdf:test-system :${pname})'";
        };
        abcl = nonBundledPackage {
          pkg = pkgs.abcl;
          mainCmd = lisp: "${lisp}/bin/abcl --noinform --eval '(require :asdf)' --eval '(asdf:load-system :${pname})' --eval '(${pname}:main)' --eval '(quit)'";
          testCmd = lisp: "${lisp}/bin/abcl --noinform --eval '(require :asdf)' --eval '(asdf:test-system :${pname})'";
        };
        clasp-common-lisp = nonBundledPackage {
          pkg = pkgs.clasp-common-lisp;
          mainCmd = lisp: "${lisp}/bin/clasp --noinform --eval '(require :asdf)' --eval '(asdf:load-system :${pname})' --eval '(${pname}:main)' --eval '(quit)'";
          testCmd = lisp: "${lisp}/bin/clasp --noinform --eval '(require :asdf)' --eval '(asdf:test-system :${pname})'";
        };
        ecl = nonBundledPackage {
          pkg = pkgs.ecl;
          mainCmd = lisp: "${lisp}/bin/ecl --eval '(require :asdf)' --eval '(asdf:load-system :${pname})' --eval '(${pname}:main)' --eval '(quit)'";
          testCmd = lisp: "${lisp}/bin/ecl --eval '(require :asdf)' --eval '(asdf:test-system :${pname})'";
        };
        mkcl = nonBundledPackage {
          pkg = pkgs.mkcl;
          mainCmd = lisp: "${lisp}/bin/mkcl --quiet -eval '(require :asdf)' -eval '(asdf:load-system :${pname})' -eval '(${pname}:main)' -eval '(quit)'";
          testCmd = lisp: "${lisp}/bin/mkcl --quiet -eval '(require :asdf)' -eval '(asdf:test-system :${pname})'";
        };
      };
      apps = impl: [
        { name = "main-" + impl; value = { type = "app"; program = recipe.${impl}.mainExe; }; }
        { name = "test-" + impl; value = { type = "app"; program = recipe.${impl}.testExe; }; }
      ];
      packages = impl: [
        { name = "main-" + impl; value = recipe.${impl}.mainExe; }
        { name = "lib-" + impl; value = recipe.${impl}.mainLib; }
      ];
      devPackages = impl: pkgs.${impl}.withPackages (ps: lispLibs pkgs.${impl});
      overlays = impl: {
        ${impl} = pkgs.${impl}.withOverrides (self: super: {
          ${pname} = config.packages."lib-${impl}";
        });
        "${pname}-${impl}" = config.packages."main-${impl}";
      };
    in {
      overlayAttrs = builtins.listToAttrs (builtins.map overlays availableLispImpls);
      devShells.default = pkgs.mkShell {
        inherit LD_LIBRARY_PATH;
        shellHook = ''
          export CL_SOURCE_REGISTRY=$PWD
        '';
        packages = builtins.map devPackages availableLispImpls;
      };
      packages = builtins.listToAttrs (builtins.concatMap packages availableLispImpls);
      apps = builtins.listToAttrs (builtins.concatMap apps availableLispImpls) //
      (if isAvailable "sbcl" then { inherit coverage-sbcl; } else {});
    };
  };
}
