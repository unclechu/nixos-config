self: super:
{
  neovim-qt =
    let
      unwrapped =
        super.neovim-qt.unwrapped.overrideAttrs (srcAttrs: srcAttrs // rec {
          version = "0.2.16.1";

          src = super.fetchFromGitHub {
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
