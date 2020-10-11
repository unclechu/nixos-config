self: super:
{
  psi-plus = super.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "1.4.1515"; # Latest on October 10, 2020

    src = super.fetchFromGitHub {
      owner = "psi-plus";
      repo = "psi-plus-snapshots";
      rev = version;
      sha256 = "0kl9iy5ycbvbsi7izbi45wg7zss675k28ss80zvzr9pagdnc0jvr";
    };

    cmakeFlags = [
      "-DENABLE_PLUGINS=ON"
      "-DCHAT_TYPE=BASIC"
    ];
  });
}
