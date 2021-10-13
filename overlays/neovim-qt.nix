# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
self: super:
{
  # Just a fresher version from the “master” branch
  neovim-qt =
    let
      unwrapped =
        super.neovim-qt.unwrapped.overrideAttrs (srcAttrs: srcAttrs // rec {
          version = "git-master";

          src = super.fetchFromGitHub {
            owner  = "equalsraf";
            repo   = "neovim-qt";
            rev    = "e537847860d02871b877d78cacad5866b7c3eb2e"; # 4 October 2021
            sha256 = "13d2g6rj0iw5rnrh69j441hhr4f89265b05fim6hxp03kwmwhlxg";
          };

          cmakeFlags = [
            "-DUSE_SYSTEM_MSGPACK=1"
            "-DENABLE_TESTS=0"
          ];
        });
    in
      super.makeOverridable ({ neovim }: super.neovim-qt.overrideAttrs (srcAttrs: srcAttrs // {
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
      })) { inherit (super) neovim; };
}
