# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ pkgs

, lib
, callPackage
, mkShell
, symlinkJoin
, makeBinaryWrapper
, stdenv

, nim
, nimlsp
, nimlangserver

, executable-dependencies ? callPackage ../executable-dependencies.nix {}
}:

let
  # A helper to make a generic Nim program derivation.
  #
  # Supports `executable-dependencies.nix` (see `e` argument), automatically
  # adds a wrapper attaching the dependencies needed for the Nim program
  # (which ones are needed is determined by `dependenciesStartRegex` and
  # `dependencyLineRegex`). Automatically cuts off runtime dependencies
  # checking.
  #
  # Also provides `.shell` attribute with a nix-shell configuration.
  # `lspForShell` allows to add an LSP server to the nix-shell.
  mk-nim-app =
    { name

    , src # A path to the main module source file (e.g. `./app.nim`)
    , extraSrcFiles ? [] # A list of paths of files to add (e.g. `[ ./cliargs.nim ./log.nim ]`)

    , nimLintArguments # Lint arguments must not include the source file
    , nimBuildArguments # Must include the Nim app source file name (e.g. `./app.nim`)

    # [string | {raw=string}]
    # {raw=string} for unescaped shell expression, just in case you ever need it.
    , wrapProgramArgs ? []

    # When you want to make a wrapper using a different executable name for the app.
    # Useful when you need to make multiple instances of the same app with different
    # bound command-line flags.
    # For example it is used for `scripts/pointer-setup/`.
    , renameExecutableTo ? null

    # For extracting executable dependencies from the `src` file sources.
    # Used only if `e != null`.
    , dependenciesStartRegex ? ''^[[:space:]]*# Guard dependencies( .*)?$''
    , dependencyLineRegex ? ''^[[:space:]]*needExe[(]"([^"]+)"[)]([[:space:]]*[#].*)?$''

    # Used only if `e != null`
    , cutOffRuntimeDependenciesCheckPhase ? ''(
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

        # The dependencies are already checked, no need to do it in runtime.
        sed -i '/^\s*# Guard dependencies\( .*\)\?$/,/^$/d' "$src"

        # Make sure the changes were actually made
        a=$(<"$pre_patched_src"); b=$(<"$src")
        if [[ "$a" == "$b" ]]; then
          >&2 echo 'Failed to remove runtime dependencies checking'
          exit 1
        fi
      )''

    # Add LSP server to the nix-shell configuration (`.shell`)
    # Type: null | "nimlsp" | "nimlangserver"
    , lspForShell ? null

    # Additional `buildInputs` for `.shell` nix-shell configuration
    , shellBuildInputs ? []

    # An instance of `executable-dependencies.nix`.
    # Set explicitly to `null` if you don’t provide it.
    # Makes it easier to notice without a default value if you just forget to add it.
    , e
    }:
    let
      isWrapperAdded =
        builtins.length wrapProgramArgs > 0
        || nimE != null
        || renameExecutableTo != null
        ;

      nimE = if isNull e then null else e.extend (final: prev: {
        scriptDependencies = final.dependencies dependenciesStartRegex dependencyLineRegex;
      });

      nim-app = mk-derivation {
        pname = name;
        e = nimE;
        inherit src extraSrcFiles nimLintArguments nimBuildArguments;
        inherit cutOffRuntimeDependenciesCheckPhase;
      };

      nim-app-wrapped =
        # Not adding a wrapper if there is no need for it
        if ! isWrapperAdded then nim-app else
        mk-wrapper {
          inherit nim-app wrapProgramArgs renameExecutableTo;
          dependenciesBinPath = if isNull nimE then null else nimE.scriptDependenciesBinPath src;
        };

      shell = mk-shell {
        inherit nim-app lspForShell shellBuildInputs;
        e = nimE;
      };
    in
    nim-app-wrapped // {
      unwrapped = nim-app; # Same as `nim-app-wrapped` if wrapper is not added
      inherit shell;
    };

  mk-derivation =
    { pname
    , src # A path to the main module source file (e.g. `./app.nim`)
    , extraSrcFiles # A list of paths of files to add (e.g. `[ ./cliargs.nim ./log.nim ]`)
    , nimLintArguments # Lint arguments must not include the source file
    , nimBuildArguments # Must include the Nim app source file name (e.g. `./app.nim`)
    , cutOffRuntimeDependenciesCheckPhase
    , e
    }:
    assert builtins.isString pname;
    assert builtins.isList extraSrcFiles;
    assert builtins.all builtins.isPath extraSrcFiles;
    assert builtins.isList nimLintArguments;
    assert builtins.length nimLintArguments > 0;
    assert builtins.isList nimBuildArguments;
    assert builtins.length nimBuildArguments > 0;
    assert builtins.all builtins.isString nimLintArguments;
    assert builtins.all builtins.isString nimBuildArguments;
    assert builtins.isString cutOffRuntimeDependenciesCheckPhase;
    assert e != null -> builtins.isAttrs e;
    stdenv.mkDerivation rec {
      name = pname;
      meta.mainProgram = pname;
      inherit pname src;

      dontUnpack = true;
      doCheck = true;

      nativeBuildInputs = [
        nim
      ];

      prePatch = ''
        # Check executable dependencies before building the Nim app
        ${lib.optionalString (e != null) e.checkPhase}

        # Just copy-pasting some files next to the main `src` file.
        # For example extra Nim modules (e.g. `cliargs.nim`, `log.nim`).
        ${lib.pipe extraSrcFiles [
          (map (x: ''cp -- ${lib.escapeShellArg "${x}"} ${lib.escapeShellArg (baseNameOf x)}''))
          (builtins.concatStringsSep "\n")
        ]}

        # Nim is not happy about dashes in the name but Nix adds some hashes prefix with a dash after.
        # Getting rid of them by `''${foo##*-}`.

        pre_patched_src=''${src##*/}
        pre_patched_src=original_''${pre_patched_src##*-}
        cp -- "$src" "$pre_patched_src"

        _new_src=''${src##*/}
        _new_src=''${_new_src##*-}

        cp -- "$src" "$_new_src"
        src="$_new_src"
      '';

      preConfigure = ''
        for file in "$pre_patched_src" "$src"; do
          nim check ${lib.escapeShellArgs nimLintArguments} "$file"
        done
      '';

      buildPhase = ''
        runHook preBuild
        (
          set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
          # The source file must be a part of `nimBuildArguments`
          nim compile ${lib.escapeShellArgs nimBuildArguments}
        )
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        (
          set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
          mkdir -p -- "$out"/bin
          cp -- ${lib.escapeShellArg meta.mainProgram} "$out"/bin
        )
        runHook postInstall
      '';
    };

  # A wrapper is made as a separate derivation with this `symlinkJoin` in case
  # you need to make a bunch of instances of the same application with bound
  # arguments so that your Nim program does not need to recompile for every
  # single instance of it. Useful for example for `scripts/pointer-setup/`.
  mk-wrapper =
    { nim-app

    , dependenciesBinPath # Result of `e.scriptDependenciesBinPath` call

    # [string | {raw=string}]
    # {raw=string} for unescaped shell expression, just in case you ever need it.
    , wrapProgramArgs

    # null | string
    , renameExecutableTo
    }:
    assert lib.isDerivation nim-app;
    assert dependenciesBinPath != null -> builtins.isString dependenciesBinPath;
    assert dependenciesBinPath != null -> dependenciesBinPath != "";
    assert builtins.isList wrapProgramArgs;
    assert renameExecutableTo != null -> builtins.isString renameExecutableTo;
    # Wrapper must not be added if there are no wrapper commands
    # and no executable rename.
    assert
      dependenciesBinPath != null
      || builtins.length wrapProgramArgs > 0
      || renameExecutableTo != null
      ;
    let
      name =
        if isNull renameExecutableTo
        then "${lib.getName nim-app}-wrapped"
        else renameExecutableTo;
      mainProgram =
        if isNull renameExecutableTo
        then nim-app.meta.mainProgram
        else renameExecutableTo;
      wrapArgsShellStr =
        lib.pipe (
          lib.optionals (dependenciesBinPath != null) [ "--prefix" "PATH" ":" dependenciesBinPath ]
          ++ wrapProgramArgs
        ) [
          (map (x:
            if builtins.isString x || builtins.isPath x || lib.isDerivation x
            then lib.escapeShellArg "${x}"
            else assert builtins.isString x.raw; x.raw
          ))
          (builtins.concatStringsSep " ")
        ];
    in
    symlinkJoin {
      inherit name;
      pname = name;
      meta.mainProgram = mainProgram;
      nativeBuildInputs = [ makeBinaryWrapper ];
      paths = [ nim-app ];
      postBuild = ''
        ${lib.optionalString (renameExecutableTo != null) ''
          mv -- "$out"/bin/${
            lib.escapeShellArg nim-app.meta.mainProgram
          } "$out"/bin/${
            lib.escapeShellArg mainProgram
          }
        ''}
        CMD=(
          wrapProgram
          "$out"/bin/${lib.escapeShellArg mainProgram}
          ${wrapArgsShellStr}
        )
        "''${CMD[@]}"
      '';
    };

  mk-shell =
    { nim-app
    , lspForShell
    , shellBuildInputs
    , e # An instance of `executable-dependencies.nix`
    }:
    assert lspForShell != null -> builtins.elem lspForShell [ "nimlsp" "nimlangserver" ];
    assert builtins.isList shellBuildInputs;
    assert builtins.all lib.isDerivation shellBuildInputs;
    mkShell {
      buildInputs =
        nim-app.nativeBuildInputs
        ++ nim-app.buildInputs
        ++ lib.optionals (e != null) (builtins.attrValues e.executables)
        ++ (
          if isNull lspForShell then [] else
          if lspForShell == "nimlsp" then [ nimlsp ] else
          if lspForShell == "nimlangserver" then [ nimlangserver ] else
          throw "Unexpected lspForShell value: ${builtins.toJSON lspForShell}"
        )
        ++ shellBuildInputs
        ;
    };
in

mk-nim-app
