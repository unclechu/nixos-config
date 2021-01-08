args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  dzen-box = "dzen-box";
  appArgs = [ dzen-box ];

  appArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args; assert pkgs.lib.isDerivation args.${k}; a+1;
    in builtins.foldl' f 0 appArgs;

  appArgExe = k: assert builtins.elem k appArgs; "${builtins.getAttr k args}/bin/${k}";
in
assert appArgsAssertion == builtins.length appArgs;
let
  sources = import ../nix/sources.nix;
  utils = args.${utils-k} or (import sources.nix-utils { inherit pkgs; });
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile "${(import ../apps/wenzels-i3.nix args).rc}/apps/${name}.sh";

  bash = "${pkgs.bash}/bin/bash";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  pacmd = "${pkgs.pulseaudio}/bin/pacmd";
  awk = "${pkgs.gawk}/bin/awk";
  xargs = "${pkgs.findutils}/bin/xargs";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable pactl}
    ${utils.shellCheckers.fileIsExecutable pacmd}
    ${utils.shellCheckers.fileIsExecutable awk}
    ${utils.shellCheckers.fileIsExecutable xargs}
    ${
      builtins.concatStringsSep "\n"
        (map (k: utils.shellCheckers.fileIsExecutable (appArgExe k)) appArgs)
    }
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    set -e
    exec <&-

    PATH=${esc pkgs.pulseaudio}/bin:$PATH
    PATH=${esc pkgs.gawk}/bin:$PATH
    PATH=${esc pkgs.findutils}/bin:$PATH

    # guard dependencies
    >/dev/null which pactl
    >/dev/null which awk
    >/dev/null which xargs

    ${src}

    sinks=`${esc pacmd} list-sinks`
    re_in_between='([
    ]\s{1,}[^*
    ]+)+'
    re="^.*\sname: <$SINK>''${re_in_between}\s{2,}volume: ([^
    ]+)''${re_in_between}\s{2,}muted: (yes|no)
    .*$"
    if [[ $sinks =~ $re ]]; then
      re='^.* ([0-9]+)% .* ([0-9]+)% .*$'
      if [[ ''${BASH_REMATCH[4]} == yes ]]; then
        ${esc (appArgExe dzen-box)} MUTE lightblue
      elif [[ ''${BASH_REMATCH[2]} =~ $re ]]; then
        ${esc (appArgExe dzen-box)} $(( (''${BASH_REMATCH[1]} + ''${BASH_REMATCH[2]}) / 2 ))% lightblue
      fi
    fi
  '';
in
{
  inherit name src pkg checkPhase;
}
