self: super:
{
  # Latest on September 27, 2020
  psi-plus = super.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "1.4.1514";

    src = super.fetchFromGitHub {
      owner = "psi-plus";
      repo = "psi-plus-snapshots";
      rev = version;
      sha256 = "13zkak2wxjy95rd8m2napdm0ril4r2bkgi5dmh5l8sjci469yh64";
    };

    cmakeFlags = [
      "-DENABLE_PLUGINS=ON"
      "-DCHAT_TYPE=BASIC"
    ];
  });
}
