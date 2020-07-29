args@{ ... }:
let
  config-k = "config";
  commit = "eeb91b03a5cef25c3931bdd4438f006a293adef9"; # ref "nixos-20.03", 28 July 2020

  stable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "00cqfmfry5rjhz4kyybr4jc4vzslkk3csy28w9k1qlyn8arpqv3s";
  };

  stable-nixpkgs = import stable-nixpkgs-src (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  );
in
{
  src = stable-nixpkgs-src;
  pkgs = stable-nixpkgs;
}
