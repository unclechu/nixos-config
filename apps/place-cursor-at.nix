args@{ ... }:
let pkgs-k = "pkgs"; config-k = "config"; in
assert let k = pkgs-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  src = let c = "16bac15dcb045c3e066fe8e7a0582cf2e183e676"; in fetchTarball {
    url = "https://github.com/unclechu/place-cursor-at/archive/${c}.tar.gz";
    sha256 = "03624ddp87c3yql0s615hrnlvgafygx9msx270z90ayanj5sn88g";
  };

  place-cursor-at = import src { inherit pkgs; };
in
place-cursor-at // { inherit src; }
