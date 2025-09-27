# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# An overlay that fixes “qm-interpolated-string” package in nixos-25.05 release.
self: super:
{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      qm-interpolated-string =
        super.lib.pipe hsSuper.qm-interpolated-string [
          (x: x.overrideAttrs (old: { meta = old.meta // { broken = false; }; }))
          super.haskell.lib.doJailbreak
          super.haskell.lib.dontCheck
        ];
    };
  };
}
