self: super:
{
  neovim-qt =
    let
      unwrapped =
        super.neovim-qt.unwrapped.overrideAttrs (srcAttrs: srcAttrs // rec {
          # Testing ligatures support, it’s planned to be released with 0.2.17 later.
          # See https://github.com/equalsraf/neovim-qt/issues/166#issuecomment-700994778
          version = "master";
          # version = "0.2.16.1";

          src = super.fetchFromGitHub {
            owner  = "equalsraf";
            repo   = "neovim-qt";
            # rev    = "v${version}";
            rev    = "5038ad11d27100889a69b079f1c0c2515687565a"; # September 30, 2020
            sha256 = "0vcqrbbk6p1q6vlmqa2nsf3pn46r3wmf0rpv9l75pln7xav0qbn0";
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
