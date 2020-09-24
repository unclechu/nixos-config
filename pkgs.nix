args@{ ... }:
let
  config-k = "config";

  withConfigArgs =
    let k = config-k; in if builtins.hasAttr k args then { ${k} = args.${k}; } else {};

  stable-pkgs   = (import ./picks/nixos-stable.nix   withConfigArgs).pkgs;
  unstable-pkgs = (import ./picks/nixos-unstable.nix withConfigArgs).pkgs;
  inherit (stable-pkgs) fetchFromGitHub;
in
stable-pkgs // {
  # Latest on August 26, 2020
  psi-plus = unstable-pkgs.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "1.4.1511";

    src = fetchFromGitHub {
      owner = "psi-plus";
      repo = "psi-plus-snapshots";
      rev = version;
      sha256 = "18l9r6xcnl3aabf6grhziqwv2bjs5xacvaijg7a9m4xhi0xamf2v";
    };

    cmakeFlags = [
      "-DENABLE_PLUGINS=ON"
      "-DCHAT_TYPE=BASIC"
    ];
  });

  neovim-qt =
    let
      unwrapped =
        stable-pkgs.neovim-qt.unwrapped.overrideAttrs (a: a // rec {
          version = "0.2.16.1";

          src = fetchFromGitHub {
            owner  = "equalsraf";
            repo   = "neovim-qt";
            rev    = "v${version}";
            sha256 = "0x5brrim3f21bzdmh6wyrhrislwpx1248wbx56csvic6v78hzqny";
          };

          cmakeFlags = [
            "-DUSE_SYSTEM_MSGPACK=1"
            "-DENABLE_TESTS=0"
          ];
        });
    in
      stable-pkgs.makeOverridable ({ neovim }: stable-pkgs.neovim-qt.overrideAttrs (a: a // {
        version = unwrapped.version;

        buildCommand = ''
          makeWrapper '${unwrapped}/bin/nvim-qt' "$out/bin/nvim-qt" \
            --prefix PATH : "${neovim}/bin"

          # link .desktop file
          mkdir -p "$out/share/pixmaps"
          ln -s '${unwrapped}/share/applications' "$out/share/applications"
          ln -s '${unwrapped}/share/pixmaps/nvim-qt.png' "$out/share/pixmaps/nvim-qt.png"
        '';

        passthru = {
          inherit unwrapped;
        };

        inherit (unwrapped) meta;
      })) { inherit (stable-pkgs) neovim; };
}
