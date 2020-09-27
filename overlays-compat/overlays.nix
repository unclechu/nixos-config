self: super:
  let
    overlays = (import <nixpkgs/nixos> {}).config.nixpkgs.overlays ++ import ../overlays;
    inherit (super.lib) foldl' flip extends;
  in
    foldl' (flip extends) (_: super) overlays self
