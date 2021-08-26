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
            rev    = "31c7f6e35d488a557209d1f81afd821bf3b73824"; # 25 August 2021
            sha256 = "03wvra8i0x7pws1dpj207g4qq1nl5j8llz79vd0hqjs8pikpp4li";
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
