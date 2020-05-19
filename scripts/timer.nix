args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile "${(import ../apps/wenzels-bash.nix args).src}/apps/${name}.pl6";

  raku = "${pkgs.rakudo}/bin/raku";
  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable raku}
    ${utils.bash.checkFileIsExecutable dzen2}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${raku}
    use v6.d;
    %*ENV{'PATH'} = q<${pkgs.dzen2}/bin> ~ ':' ~ %*ENV{'PATH'};
    ${builtins.replaceStrings ["use v6;"] [""] src}
  '';
in
{
  inherit src name pkg checkPhase;
}
