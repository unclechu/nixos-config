let
  constants = import ../../constants.nix;
  sources = import ../../nix/sources.nix;
in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}

, xbindkeys              ? import ../../apps/wenzels-xbindkeys.nix { inherit pkgs; }
, keyRepeat              ? constants.keyRepeat
, xkb                    ? constants.xkb
, xlib-keys-hack-starter ? import ../../apps/wenzels-xlib-keys-hack { inherit pkgs; }
}:
assert pkgs.lib.isDerivation xbindkeys;
assert pkgs.lib.isDerivation xlib-keys-hack-starter;
let
  keyRepeatDelay    = keyRepeat.delay;
  keyRepeatInterval = keyRepeat.interval;
  xkbLayout         = xkb.layout;
  xkbOptions        = xkb.options; # "eurosign:e"
in
assert nix-utils.valueCheckers.isPositiveNaturalNumber keyRepeatDelay;
assert nix-utils.valueCheckers.isPositiveNaturalNumber keyRepeatInterval;
assert nix-utils.valueCheckers.isNonEmptyString        xkbLayout;
assert nix-utils.valueCheckers.isNonEmptyString        xkbOptions;
let
  inherit (nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.raku;
  raku = "${pkgs.rakudo}/bin/raku";
  xset = "${pkgs.xorg.xset}/bin/xset";
  setxkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";
  numlockx = "${pkgs.numlockx}/bin/numlockx";
  pkill = "${pkgs.procps}/bin/pkill";
  xbindkeys-exe = "${xbindkeys}/bin/${xbindkeys.name}";
  xlib-keys-hack-starter-exe = "${xlib-keys-hack-starter}/bin/${xlib-keys-hack-starter.name}";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable raku}
    ${nix-utils.shellCheckers.fileIsExecutable xset}
    ${nix-utils.shellCheckers.fileIsExecutable setxkbmap}
    ${nix-utils.shellCheckers.fileIsExecutable numlockx}
    ${nix-utils.shellCheckers.fileIsExecutable pkill}
    ${nix-utils.shellCheckers.fileIsExecutable xbindkeys-exe}
    ${nix-utils.shellCheckers.fileIsExecutable xlib-keys-hack-starter-exe}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${raku}
  use v6.d;
  close $*IN;
  my \xset := q<${xset}>;
  my \keyRepeatDelay := q<${toString keyRepeatDelay}>;
  my \keyRepeatInterval := q<${toString keyRepeatInterval}>;
  my \setxkbmap := q<${setxkbmap}>;
  my \xkbLayout := q<${xkbLayout}>;
  my \xkbOptions := q<${xkbOptions}>;
  my \numlockx := q<${numlockx}>;
  my \pkill := q<${pkill}>;
  my \xbindkeys := q<${xbindkeys-exe}>;
  my \xlib-keys-hack-starter-exe := q<${xlib-keys-hack-starter-exe}>;
  my \xlib-keys-hack-starter-name := q<${baseNameOf xlib-keys-hack-starter-exe}>;
  ${src}
''
