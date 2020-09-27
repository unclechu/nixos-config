self: super:
{
  # Latest on September 27, 2020
  psi-plus = super.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "1.4.1512";

    src = super.fetchFromGitHub {
      owner = "psi-plus";
      repo = "psi-plus-snapshots";
      rev = version;
      sha256 = "1vh7ib91x719mnp2i2cfnisp2zkvkq65ngzj0d8506k399cra2yg";
    };

    cmakeFlags = [
      "-DENABLE_PLUGINS=ON"
      "-DCHAT_TYPE=BASIC"
    ];
  });
}
