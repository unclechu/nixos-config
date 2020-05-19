args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, xbindkeys ? null
, xlib-keys-hack-starter
, keyRepeat
, xkb
, ...
}:
let
  utils = import ../../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  src = builtins.readFile ./main.raku;
  raku = "${pkgs.rakudo}/bin/raku";
  xset = "${pkgs.xorg.xset}/bin/xset";
  setxkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";
  numlockx = "${pkgs.numlockx}/bin/numlockx";
  pkill = "${pkgs.procps}/bin/pkill";

  xbindkeys-origin = args.xbindkeys or pkgs.xbindkeys;
  xbindkeys-exe = "${xbindkeys-origin}/bin/${xbindkeys-origin.name}";

  xlib-keys-hack-starter-exe = "${xlib-keys-hack-starter}/bin/${xlib-keys-hack-starter-name}";
  xlib-keys-hack-starter-name = xlib-keys-hack-starter.name;

  keyRepeatDelay = keyRepeat.delay;
  keyRepeatInterval = keyRepeat.interval;
  xkbLayout = xkb.layout;
  xkbOptions = xkb.options;

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable raku}
    ${utils.bash.checkFileIsExecutable xset}
    ${utils.bash.checkValueIsPositiveNaturalNumber keyRepeatDelay}
    ${utils.bash.checkValueIsPositiveNaturalNumber keyRepeatInterval}
    ${utils.bash.checkFileIsExecutable setxkbmap}
    ${utils.bash.checkValueIsNonEmptyString xkbLayout}
    ${utils.bash.checkValueIsNonEmptyString xkbOptions}
    ${utils.bash.checkFileIsExecutable numlockx}
    ${utils.bash.checkFileIsExecutable pkill}
    ${utils.bash.checkFileIsExecutable xbindkeys-exe}
    ${utils.bash.checkFileIsExecutable xlib-keys-hack-starter-exe}
    ${utils.bash.checkValueIsNonEmptyString xlib-keys-hack-starter-name}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
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
    my \xlib-keys-hack-starter-name := q<${xlib-keys-hack-starter-name}>;
    ${src}
  '';
in
{
  inherit src name pkg checkPhase;
}
