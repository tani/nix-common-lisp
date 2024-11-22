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
      unbundledPackage = { inputs, outputs }: rec {
        mainLib = inputs.lisp.buildASDFSystem {
          inherit pname version src systems nativeLibs;
          lispLibs = lispLibs inputs.lisp;
        };
        lisp = inputs.lisp.withPackages (ps: [mainLib]) // { inherit (inputs.lisp) meta; };
        mainCode = ''
          (let* ((_ (asdf:load-system :${pname}))
                 (component (asdf:find-system :${pname}))
                 (entry-point (asdf/system:component-entry-point component))
                 (function (uiop:ensure-function entry-point)))
            (funcall function)
            (quit))
        '';
        mainExe = pkgs.writeShellScriptBin pname ''
          export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
          exec ${outputs { inherit lisp; code = mainCode; }}
        '';
        testCode = ''
        (progn
          (asdf:test-system :${pname})
          (quit))
        '';
        testExe = pkgs.writeShellScriptBin "${pname}-test" ''
          export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
          exec ${outputs { inherit lisp; code = testCode; }}
        '';
      };
      bundledPackage = { inputs, outputs }: rec {
        mainLib = inputs.lisp.buildASDFSystem {
          inherit pname version src systems nativeLibs;
          lispLibs = lispLibs inputs.lisp;
        };
        lisp = inputs.lisp.withPackages (ps: [mainLib]) // { inherit (inputs.lisp) meta; };
        mainCode = ''
          (let ((system (asdf:find-system :${pname})))
            (setf (asdf/system:component-build-pathname system) #p"$out/bin/${pname}")
            (asdf:make :${pname})
            (quit))
        '';
        mainRaw = pkgs.stdenv.mkDerivation {
          inherit pname version src;
          meta.mainProgram = pname;
          dontStrip = true;
          installPhase = ''
            export HOME=$TMPDIR
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            ${outputs { inherit lisp; code = mainCode; }}
          '';
        };
        mainExe =
          pkgs.writeShellScriptBin pname ''
            export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
            exec ${mainRaw}/bin/${pname} "$@"
          '';
        testCode = ''
          (progn
            (asdf:test-system :${pname})
            (quit))
        '';
        testExe = pkgs.writeShellScriptBin "${pname}-test" ''
          export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
          ${outputs { inherit lisp; code = testCode; }}
        '';
      };
      coverage-sbcl =
        let
          lisp = pkgs.sbcl.withPackages (ps: lispLibs pkgs.sbcl) // { inherit (pkgs.sbcl) meta; };
          program = pkgs.writeShellScriptBin "${pname}-coverage" ''
            export CL_SOURCE_REGISTRY=$PWD
            ${lisp}/bin/${lisp.meta.mainProgram} --noinform --disable-debugger <<EOF
              (require "asdf")
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
          inputs.lisp = pkgs.sbcl;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} --noinform --disable-debugger --eval '(require "asdf")' --eval "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
          '';
        };
        ccl = bundledPackage {
          inputs.lisp = pkgs.ccl;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} --quiet --eval '$(require "asdf")' --eval "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
          '';
        };
        clisp = unbundledPackage {
          inputs.lisp = pkgs.clisp;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} --quiet -x '(require "asdf")' -x "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
          '';
        };
        ecl = unbundledPackage {
          inputs.lisp = pkgs.ecl;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} --eval '(require "asdf")' --eval "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
          '';
        };
        cmucl_binary = unbundledPackage {
          inputs.lisp = pkgs.cmucl_binary;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} -quiet -eval '(require "asdf")' -eval "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
          '';
        };
        abcl = unbundledPackage {
          inputs.lisp = pkgs.abcl;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} --noinform --eval '(require "asdf")' --eval "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
          '';
        };
        clasp-common-lisp = unbundledPackage {
          inputs.lisp = pkgs.clasp-common-lisp;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} --noinform --eval '(require "asdf")' --eval "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
          '';
        };
        mkcl = unbundledPackage {
          inputs.lisp = pkgs.mkcl;
          outputs = { lisp, code }: ''
            ${lisp}/bin/${lisp.meta.mainProgram} --quiet -eval '(require "asdf")' -eval "$(cat <<EOF
              ${code}
            EOF
            )" -- "$@"
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
