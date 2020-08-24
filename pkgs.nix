args@{ ... }:
let
  config-k = "config";

  withConfigArgs =
    let k = config-k; in if builtins.hasAttr k args then { ${k} = args.${k}; } else {};

  stable-pkgs   = (import ./picks/nixos-stable.nix   withConfigArgs).pkgs;
  unstable-pkgs = (import ./picks/nixos-unstable.nix withConfigArgs).pkgs;
  inherit (stable-pkgs) fetchFromGitHub;
in
stable-pkgs // rec {
  # In the NixOS 20.03 nixpkgs "rakudo" package is 2017.01 version,
  # very old one, it's not even Raku yet but Perl6
  # (before the language has been renamed).
  # Whilst in unstable nixpkgs it's 2020.05.
  rakudo = unstable-pkgs.rakudo;

  # Latest on August 24, 2020
  psi-plus = unstable-pkgs.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "1.4.1483";

    src = fetchFromGitHub {
      owner = "psi-plus";
      repo = "psi-plus-snapshots";
      rev = version;
      sha256 = "1m40i9kbq50lasdxlv8an19i2ccn3sv5f4bzr7yc1whn01mmigbv";
    };

    cmakeFlags = [
      "-DENABLE_PLUGINS=ON"
      "-DCHAT_TYPE=BASIC"
    ];
  });

  # Released on August 6, 2020
  neovim-unwrapped =
    unstable-pkgs.neovim-unwrapped.overrideAttrs (srcAttrs: srcAttrs // rec {
      version = "0.4.4";

      src = fetchFromGitHub {
        owner = "neovim";
        repo = "neovim";
        rev = "v${version}";
        sha256 = "11zyj6jvkwas3n6w1ckj3pk6jf81z1g7ngg4smmwm7c27y2a6f2m";
      };
    });

  neovim = unstable-pkgs.wrapNeovim neovim-unwrapped {};

  guitarix = unstable-pkgs.guitarix;
}
