# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Pick latest Skim
self: super:
{
  skim = super.skim.overrideAttrs (old: rec {
    version = "4.7.0";

    src = super.fetchFromGitHub {
      owner = "skim-rs";
      repo = "skim";
      tag = "v${version}";
      hash = "sha256-ek+h/MWxvUZKfUKSYL501+qqwFKHifopj2PicvnEr0Y=";
    };

    cargoDeps = super.rustPlatform.fetchCargoVendor {
      inherit src version;
      pname = "skim-vendor";
      hash = "sha256-n+fLtinvMchjsztH5GmPIjG+2spUu0Ayw9yqHTJRxAQ=";
    };

    checkPhase =
      builtins.replaceStrings
        [" --features test-utils"]
        [""]
        old.checkPhase
        ;
  });
}
