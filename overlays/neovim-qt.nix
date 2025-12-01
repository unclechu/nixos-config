# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

self: super:
{
  # Just a fresher version from the “master” branch
  neovim-qt =
    let
      unwrapped =
        super.neovim-qt.unwrapped.overrideAttrs (srcAttrs: srcAttrs // rec {
          version = "git-${sources.neovim-qt.branch}";
          src = sources.neovim-qt;

          cmakeFlags = [
            "-DUSE_SYSTEM_MSGPACK=1"
            "-DENABLE_TESTS=0"
          ];
        });
    in
      super.lib.makeOverridable ({ neovim }: super.neovim-qt.overrideAttrs (srcAttrs: srcAttrs // {
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
