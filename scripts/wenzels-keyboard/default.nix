let
  constants = import ../../constants.nix;
  sources = import ../../nix/sources.nix;
in
{ callPackage
, lib
, rakudo
, xorg # ‘xset’ and ‘setxkbmap’
, numlockx
, procps

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __xbindkeys ? callPackage ../../apps/wenzels-xbindkeys.nix { inherit __nix-utils; }
, __xlib-keys-hack-starter ? callPackage ../../apps/wenzels-xlib-keys-hack { inherit __nix-utils; }

# Build options
, __srcScript ? ./main.raku
, keyRepeatOptions ? constants.keyRepeat
, xkbOptions ? constants.xkb
}:
assert lib.isDerivation __xbindkeys;
assert lib.isDerivation __xlib-keys-hack-starter;
let
  keyRepeatDelay    = keyRepeatOptions.delay;
  keyRepeatInterval = keyRepeatOptions.interval;
  xkbLayout         = xkbOptions.layout;
  xkbOptions'       = xkbOptions.options; # "eurosign:e"

  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir valueCheckers shellCheckers;
in
assert valueCheckers.isPositiveNaturalNumber keyRepeatDelay;
assert valueCheckers.isPositiveNaturalNumber keyRepeatInterval;
assert valueCheckers.isNonEmptyString        xkbLayout;
assert valueCheckers.isNonEmptyString        xkbOptions';
let
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcScript;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    raku = rakudo;
    xset = xorg.xset;
    setxkbmap = xorg.setxkbmap;
    numlockx = numlockx;
    pkill = procps;

    ${__xbindkeys.name} = __xbindkeys;
    ${__xlib-keys-hack-starter.name} = __xlib-keys-hack-starter;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.raku}
  use v6.d;
  close $*IN;

  ${
    builtins.concatStringsSep "\n"
      (builtins.attrValues (builtins.mapAttrs (n: v: "my Str:D \\${n} := q<${v}>;") executables))
  }

  my Str:D \xbindkeys := ${__xbindkeys.name};
  my Str:D \xlib-keys-hack-starter := ${__xlib-keys-hack-starter.name};

  my Str:D \keyRepeatDelay := q<${toString keyRepeatDelay}>;
  my Str:D \keyRepeatInterval := q<${toString keyRepeatInterval}>;
  my Str:D \xkbLayout := q<${xkbLayout}>;
  my Str:D \xkbOptions := q<${xkbOptions'}>;

  ${src}
''
