self: super:
{
  # Latest on August 26, 2020
  psi-plus = super.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "1.4.1511";

    src = super.fetchFromGitHub {
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
