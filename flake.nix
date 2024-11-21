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
      unbundledPackage = { pkg, mainCmd, testCmd }: rec {
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
        mainExe =
          let
            mainRaw = pkgs.stdenv.mkDerivation {
              inherit pname version src;
              meta.mainProgram = pname;
              dontStrip = true;
              installPhase = ''
                export HOME=$TMPDIR
                export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
                ${mainCmd lisp}
              '';
            };
          in
            pkgs.writeShellScriptBin pname ''
              export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
              exec ${mainRaw}/bin/${pname} "$@"
            '';
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
            ${lisp}/bin/sbcl --noinform --disable-debugger <<EOF
              (require :asdf)
              (require :sb-cover)
              (declaim (optimize sb-cover:store-coverage-data))
              (asdf:compile-system :fibonacci :force t)
              (declaim (optimize (sb-cover:store-coverage-data 0)))
              (asdf:test-system :${pname})
              (sb-cover:report "coverage/")
            EOF
          '';
        in
          {
            type = "app";
            inherit program;
          };
      recipe = {
        sbcl = bundledPackage {
          pkg = pkgs.sbcl;
          mainCmd = lisp: ''
            ${lisp}/bin/sbcl --noinform --disable-debugger <<EOF
              (require :asdf)
              (let ((system (asdf:find-system :${pname})))
                (setf (asdf/system:component-build-pathname system) #p"$out/bin/${pname}"))
              (asdf:make :${pname})
            EOF
          '';
          testCmd = lisp: ''
            ${lisp}/bin/sbcl --noinform --disable-debugger <<EOF
              (require :asdf)
              (asdf:test-system :${pname})
            EOF
          '';
        };
        ccl = bundledPackage {
          pkg = pkgs.ccl;
          mainCmd = lisp: ''
            ${lisp}/bin/ccl --quiet <<EOF
              (require :asdf)
              (let ((system (asdf:find-system :${pname})))
                (setf (asdf/system:component-build-pathname system) #p"$out/bin/${pname}"))
              (asdf:make :${pname})
              EOF
          '';
          testCmd = lisp: ''
            ${lisp}/bin/ccl --quiet <<EOF
              (require :asdf)
              (asdf:test-system :${pname})
            EOF
          '';
        };
        ecl = bundledPackage {
          pkg = pkgs.ecl;
          mainCmd = lisp: ''
            ${lisp}/bin/ecl <<EOF
              (require :asdf)
              (let* ((system (asdf:find-system :${pname}))
                     (entry-point (asdf/system:component-entry-point system)))
                (setf (asdf/system:component-build-pathname system) #p"$out/bin/${pname}")
                (asdf:make-build :${pname}
                  :type :program
                  :move-here #P"$out/bin/"
                  :prologue-code
                  '(require :asdf)
                  :epilogue-code
                  \`(progn
                      (asdf:load-system :${pname})
                      (setq uiop:*command-line-arguments* (cdr (uiop:raw-command-line-arguments)))
                      (funcall (uiop:ensure-function ',entry-point))
                      (quit))))
            EOF
          '';
          testCmd = lisp: ''
            ${lisp}/bin/ecl <<EOF
              (require :asdf)
              (asdf:test-system :${pname})
            EOF
          '';
        };
        clisp = bundledPackage {
          pkg = pkgs.clisp;
          mainCmd = lisp: ''
            ${lisp}/bin/clisp --quiet <<EOF
              (require "asdf")
              (let ((system (asdf:find-system :${pname})))
                (setf (asdf/system:component-build-pathname system) #p"$out/bin/${pname}"))
              (asdf:make :${pname})
            EOF
          '';
          testCmd = lisp: ''
            ${lisp}/bin/clisp --quiet <<EOF
              (require "asdf")
              (asdf:test-system :${pname})
            EOF
          '';
        };
        cmucl_binary = unbundledPackage {
          pkg = pkgs.cmucl_binary;
          mainCmd = lisp: ''
            ${lisp}/bin/lisp -quiet -eval '(require :asdf)' -eval "$(cat <<EOF
              (let* ((_ (asdf:load-system :${pname}))
                     (component (asdf:find-system :${pname}))
                     (entry-point (asdf/system:component-entry-point component))
                     (function (uiop:ensure-function entry-point)))
                (funcall function)
                (quit))
            EOF
            )" -- "$@"
          '';
          testCmd = lisp: ''
            ${lisp}/bin/lisp -quiet <<EOF
              (require :asdf)
              (asdf:test-system :${pname})
            EOF
          '';
        };
        abcl = unbundledPackage {
          pkg = pkgs.abcl;
          mainCmd = lisp: ''
            ${lisp}/bin/abcl --noinform --eval '(require :asdf)' --eval "$(cat <<EOF
              (let* ((_ (asdf:load-system :${pname}))
                     (component (asdf:find-system :${pname}))
                     (entry-point (asdf/system:component-entry-point component))
                     (function (uiop:ensure-function entry-point)))
                (funcall function)
                (quit))
            EOF
            )" -- "$@"
          '';
          testCmd = lisp: ''
            ${lisp}/bin/abcl --noinform <<EOF
              (require :asdf)
              (asdf:test-system :${pname})
            EOF
          '';
        };
        clasp-common-lisp = unbundledPackage {
          pkg = pkgs.clasp-common-lisp;
          mainCmd = lisp: ''
            ${lisp}/bin/clasp --noinform --eval '(require :asdf)' --eval "$(cat <<EOF
              (let* ((_ (asdf:load-system :${pname}))
                     (component (asdf:find-system :${pname}))
                     (entry-point (asdf/system:component-entry-point component))
                     (function (uiop:ensure-function entry-point)))
                (funcall function)
                (quit))
            EOF
            )" -- "$@"
          '';
          testCmd = lisp: ''
            ${lisp}/bin/clasp --noinform <<EOF
              (require :asdf)
              (asdf:test-system :${pname})
            EOF
          '';
        };
        mkcl = unbundledPackage {
          pkg = pkgs.mkcl;
          mainCmd = lisp: ''
            ${lisp}/bin/mkcl --quiet -eval '(require :asdf)' -eval "$(cat <<EOF
              (let* ((_ (asdf:load-system :${pname}))
                     (component (asdf:find-system :${pname}))
                     (entry-point (asdf/system:component-entry-point component))
                     (function (uiop:ensure-function entry-point)))
                (funcall function)
                (quit))
            EOF
            )" -- "$@"
          '';
          testCmd = lisp: ''
            ${lisp}/bin/mkcl --quiet <<EOF
              (require :asdf)
              (asdf:test-system :${pname})
            EOF
          '';
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
