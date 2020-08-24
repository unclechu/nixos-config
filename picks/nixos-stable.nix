args@{ ... }:
let
  config-k = "config";
  commit = "0c59c1296b23abc25a6383ff26db2eeb17ad8a81"; # ref "nixos-20.03", 22 August 2020

  stable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "03sifcpkc3prszaycd6snvpxam66phmj0b7m4723l5dmmsyq4bkw";
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
