# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, callPackage
, mkShell
, symlinkJoin
, makeBinaryWrapper
, stdenv

, nim
, nimlsp
, nimlangserver

, jq

, executable-dependencies ? callPackage ../executable-dependencies.nix {}
, __clunky-toml-json-converter ? callPackage ../../apps/clunky-toml-json-converter {}

, nimlsp-issue-180-fix-wrapper ? callPackage ./nimlsp-issue-180-fix-wrapper {}
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

    # A list of paths of files to add
    # (e.g. `[ ./nim.cfg ./cliargs.nim ./log.nim ]`).
    , extraSrcFiles ? []

    , nimLintArguments ? [] # Lint arguments (for `nim check`)
    , nimBuildArguments ? [] # Build arguments (for `nim compile`)

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
    # The dependencies are already checked, no need to do it in runtime.
    # So cutting off the runtime checking.
    , cutOffRuntimeDependenciesCheckPhase ? ''(
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

        TMPDIR=$(mktemp -d --suffix=-pre-runtime-dependencies-check-cut-off)
        found_in=
        for nim_file in *.nim; do
          pre_patched_file=$TMPDIR/$nim_file
          cp -- "$nim_file" "$pre_patched_file"

          # Sanity check
          a=$(<"$pre_patched_file"); b=$(<"$nim_file")
          if [[ "$a" != "$b" ]]; then
            >&2 printf 'Unexpectedly “%s” and “%s” are not equal\n' "$pre_patched_file" "$nim_file"
            exit 1
          fi

          sed -i -E '
            /^([[:space:]]*)# Guard dependencies( .*)?$/,/^$/ {
              s/^([[:space:]]*)# Guard dependencies( .*)?$/\1discard/
              t
              d
            }
          ' "$nim_file"

          # Check if changes were actually made
          a=$(<"$pre_patched_file"); b=$(<"$nim_file")
          if [[ "$a" != "$b" ]]; then
            found_in=$nim_file
            break
          fi
        done

        if [[ -z "$found_in" ]]; then
          >&2 echo 'Failed to remove runtime dependencies checking (could not find the checking)'
          exit 1
        fi
      )''

    # Add LSP server to the nix-shell configuration (`.shell`)
    # Type: null | "nimlsp" | "nimlangserver"
    , lspForShell ? null

    # Extra `nativeBuildInputs` and/or `buildInputs` for the Nim app derivation
    , nativeBuildInputs ? []
    , buildInputs ? []

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
        || e != null
        || renameExecutableTo != null
        ;

      dependenciesBinPath =
        if isNull e then null else lib.pipe ([ src ] ++ extraSrcFiles) [
          (builtins.filter builtins.isPath)
          (map (e.tryDependencies dependenciesStartRegex dependencyLineRegex))
          # The dependencies checking can be placed in a separate module.
          # Trying to find in all source modules.
          (lib.findFirst (x: x != null) null)
          # Executable dependencies must be found
          (x: assert x != null; x)
          e.scriptDependenciesToDerivations
          lib.makeBinPath
        ];

      nim-app = mk-derivation {
        pname = name;
        inherit e src extraSrcFiles nimLintArguments nimBuildArguments;
        inherit nativeBuildInputs buildInputs cutOffRuntimeDependenciesCheckPhase;
      };

      nim-app-wrapped =
        # Not adding a wrapper if there is no need for it
        if ! isWrapperAdded then nim-app else
        mk-wrapper {
          inherit nim-app wrapProgramArgs renameExecutableTo dependenciesBinPath;
        };

      shell = mk-shell {
        inherit e nim-app lspForShell nativeBuildInputs buildInputs shellBuildInputs;
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
    , nativeBuildInputs
    , buildInputs
    , nimLintArguments
    , nimBuildArguments
    , cutOffRuntimeDependenciesCheckPhase
    , e
    }:
    assert builtins.isString pname;
    assert builtins.isList extraSrcFiles;
    assert builtins.all (x: builtins.isPath x || lib.isDerivation x) extraSrcFiles;
    assert builtins.isList nativeBuildInputs;
    assert builtins.all lib.isDerivation nativeBuildInputs;
    assert builtins.isList buildInputs;
    assert builtins.all lib.isDerivation buildInputs;
    assert builtins.isList nimLintArguments;
    assert builtins.isList nimBuildArguments;
    assert builtins.all builtins.isString nimLintArguments;
    assert builtins.all builtins.isString nimBuildArguments;
    assert builtins.isString cutOffRuntimeDependenciesCheckPhase;
    assert e != null -> builtins.isAttrs e;
    stdenv.mkDerivation (lib.fix (self: {
      inherit pname src;
      name = pname;
      meta.mainProgram = pname;

      dontUnpack = true;
      doCheck = true;

      nativeBuildInputs = [ nim ] ++ nativeBuildInputs;
      buildInputs = buildInputs;

      prePatch = ''
        # Check executable dependencies before building the Nim app
        ${lib.optionalString (e != null) e.checkPhase}

        # Just copy-pasting some files next to the main `src` file.
        # For example extra Nim modules (e.g. `cliargs.nim`, `log.nim`).
        ${lib.pipe extraSrcFiles [
          (map (x:
            if builtins.isPath x
            then ''cp -- ${lib.escapeShellArg "${x}"} ${lib.escapeShellArg (baseNameOf x)}''
            else if lib.isDerivation x
            then ''ln -s -- ${lib.escapeShellArg "${x}"} ${lib.escapeShellArg (lib.getName x)}''
            else throw "Unexpected extraSrcFiles item type"
          ))
          (builtins.concatStringsSep "\n")
        ]}

        # Nim is not happy about dashes in the name but Nix adds some hashes prefix with a dash after.
        # Getting rid of them by `''${foo##*-}`.
        _new_src=''${src##*/}
        _new_src=''${_new_src##*-}
        cp -- "$src" "$_new_src"
        src="$_new_src"

        pre_patched_dir=$(mktemp -d --suffix=-pre-patched-sources)
        trap 'rm -rf -- "$pre_patched_dir"' EXIT
        find -mindepth 1 -maxdepth 1 -not -type d -exec cp -d -t "$pre_patched_dir" -- {} +

        ${cutOffRuntimeDependenciesCheckPhase}
      '';

      preConfigure = ''
        (
          set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
          for dir in "$pre_patched_dir" ./; do
            (cd -- "$dir"; nim check ${lib.escapeShellArgs nimLintArguments} "$src")
          done
        )
      '';

      buildPhase = ''
        runHook preBuild
        (
          set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
          nim compile -d:release --nimcache:nimcache ${lib.escapeShellArgs (
            nimBuildArguments ++ [ "-o:${self.meta.mainProgram}" ]
          )} "$src"
        )
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        (
          set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
          mkdir -p -- "$out"/bin
          cp -- ${lib.escapeShellArg self.meta.mainProgram} "$out"/bin
        )
        runHook postInstall
      '';
    }));

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
    , nativeBuildInputs
    , buildInputs
    , shellBuildInputs
    , e # An instance of `executable-dependencies.nix`
    }:
    assert lspForShell != null -> builtins.elem lspForShell [ "nimlsp" "nimlangserver" ];
    assert builtins.isList shellBuildInputs;
    assert builtins.all lib.isDerivation shellBuildInputs;
    assert builtins.isList nativeBuildInputs;
    assert builtins.all lib.isDerivation nativeBuildInputs;
    assert builtins.isList buildInputs;
    assert builtins.all lib.isDerivation buildInputs;
    mkShell (lib.fix (self: {
      buildInputs =
        # Note that by taking these only `*.dev` variants are picked.
        # And you don’t get `*.so` runtime dependencies for those.
        # Adding `[ nim ]` (which is the only default `nativeBuildInputs`)
        # and forwarding extra `nativeBuildInputs` and `buildInputs` below.
        # (nim-app.nativeBuildInputs ++ nim-app.buildInputs)
        [ nim ]

        ++ lib.optionals (e != null) (builtins.attrValues e.executables)

        ++ (
          if isNull lspForShell then [] else
          if lspForShell == "nimlsp" then [ nimlsp-issue-180-fix-wrapper ] else
          if lspForShell == "nimlangserver" then [ nimlangserver ] else
          throw "Unexpected lspForShell value: ${builtins.toJSON lspForShell}"
        )

        ++ [ jq __clunky-toml-json-converter ] # For dev.sh

        ++ nativeBuildInputs
        ++ buildInputs
        ++ shellBuildInputs
        ;

      shellHook = ''
        # Make `*.so` runtime dependencies be discoverable by Nim.
        export LD_LIBRARY_PATH=${
          lib.pipe self.buildInputs [
            (map lib.getLib)
            lib.makeLibraryPath
            lib.escapeShellArg
          ]
        }"''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
      '';
    }));
in

mk-nim-app
