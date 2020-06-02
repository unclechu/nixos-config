args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  keyRepeat = "keyRepeat";
  xkb = "xkb";
in
assert builtins.hasAttr keyRepeat args;
assert builtins.hasAttr xkb args;
let
  keyRepeatDelay = args."${keyRepeat}".delay;
  keyRepeatInterval = args."${keyRepeat}".interval;
  xkbLayout = args."${xkb}".layout;
  xkbOptions = args."${xkb}".options;
  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
in
assert utils.valueCheckers.isPositiveNaturalNumber keyRepeatDelay;
assert utils.valueCheckers.isPositiveNaturalNumber keyRepeatInterval;
assert utils.valueCheckers.isNonEmptyString xkbLayout;
assert utils.valueCheckers.isNonEmptyString xkbOptions;
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  xbindkeys = "xbindkeys";

  xbindkeys-drv =
    let k = xbindkeys; in
    assert builtins.hasAttr k args -> pkgs.lib.isDerivation args."${k}";
    assert builtins.hasAttr k pkgs;
    args."${k}" or pkgs."${k}";

  xlib-keys-hack-starter = "xlib-keys-hack-starter";
  appArgs = [ xbindkeys xlib-keys-hack-starter ];
  optionalAppArgs = [ xbindkeys ];

  appArgsAssertion =
    let
      f = a: k:
        assert ! builtins.elem k optionalAppArgs -> builtins.hasAttr k args;
        assert ! builtins.elem k optionalAppArgs -> pkgs.lib.isDerivation args."${k}";
        assert builtins.elem k optionalAppArgs -> (
          if k == xbindkeys
          then pkgs.lib.isDerivation xbindkeys-drv
          else throw "unexpected optional app key: '${k}'"
        );
        a+1;
    in
      builtins.foldl' f 0 appArgs;

  appArgExe = k:
    if builtins.elem k optionalAppArgs then (
      if k == xbindkeys
      then "${xbindkeys-drv}/bin/${xbindkeys-drv.name}"
      else throw "unexpected optional app key: '${k}'"
    ) else (
      assert builtins.elem k appArgs;
      let name = args."${k}".name; in "${builtins.getAttr k args}/bin/${name}"
    );
in
assert appArgsAssertion == builtins.length appArgs;
let
  utils = args.utils or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  src = builtins.readFile ./main.raku;
  raku = "${pkgs.rakudo}/bin/raku";
  xset = "${pkgs.xorg.xset}/bin/xset";
  setxkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";
  numlockx = "${pkgs.numlockx}/bin/numlockx";
  pkill = "${pkgs.procps}/bin/pkill";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable raku}
    ${utils.shellCheckers.fileIsExecutable xset}
    ${utils.shellCheckers.fileIsExecutable setxkbmap}
    ${utils.shellCheckers.fileIsExecutable numlockx}
    ${utils.shellCheckers.fileIsExecutable pkill}
    ${
      builtins.concatStringsSep "\n"
        (map (k: utils.shellCheckers.fileIsExecutable (appArgExe k)) appArgs)
    }
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
    my \xbindkeys := q<${appArgExe xbindkeys}>;
    my \xlib-keys-hack-starter-exe := q<${appArgExe xlib-keys-hack-starter}>;
    my \xlib-keys-hack-starter-name := q<${baseNameOf (appArgExe xlib-keys-hack-starter)}>;
    ${src}
  '';
in
{
  inherit src name pkg checkPhase;
}
