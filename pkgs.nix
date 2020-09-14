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
}
