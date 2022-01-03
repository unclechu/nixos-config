# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, writeScriptBin
, makeWrapper
, symlinkJoin

, rakudo
, xlibs # Just for ‘xinput’
}:
let
  escRaku = x: "'${lib.escape ["'"] x}'";

  makePointer =
    { pointerName # This will be a suffix for resulting derivation
    , deviceName # xinput device name
    , deviceProductId ? null # Additional validation by Device Product ID

    , rakuCommands
    # ↑ A function that takes Raku variable name with device xinput ID and returns Raku code that
    #   applies settings for that device. For instance (interpolated argument must not be escaped):
    #     ''
    }: let
      name = "pointer-${pointerName}";
      productIdPropName = "Device Product ID";
      raku-exe = "${rakudo}/bin/raku";
      esc = lib.escapeShellArg;

      script = writeScriptBin name ''
        #! ${raku-exe}
        use v6.d;
        close $*IN;

        my regex PropValueRegEx {
          ^\s* $<name>=<-[:]>+ \s+ '(' $<prop-id>=\d+ '):' \s* $<value>=<-[:]>+ \s*$
        };

        my Str:D @devices = run('xinput', 'list', '--short', :out).out.slurp(:close).chomp.lines;
        @devices .= grep: / '↳ ' ${escRaku deviceName} \s+ 'id='\d+ \s+ /;
        my Bool:D $is-found = False;

        for @devices -> $device {
          my $found-device-line = $device ~~ / 'id=' $<found-id> = (\d+) /;
          my Num:D \xinput-id = $<found-id>.Num;

          my Str %props;
          my Bool:D $props-are-read = False;

          sub read-props {
            return if $props-are-read;
            my @props = run('xinput', 'list-props', xinput-id, :out).out.slurp(:close).chomp.lines;
            %props = @props.map({ ($<name>.Str, $<value>.Str) if $_ ~~ &PropValueRegEx }).flat.Map;
            $props-are-read = True;
          }

          ${if isNull deviceProductId then "" else "{${verfiyProductId}}"}

          sub with-prop (Str:D \prop-name, Block:D $fn) {
            read-props;

            unless %props{prop-name}:exists {
              $*ERR.say: join q< >, [
                "WARNING!",
                "“{prop-name}” property is not found for xinput device ID #{xinput-id}!",
              ];
              next
            }

            $fn(%(name => prop-name, value => %props{prop-name}))
          }

          sub set-prop (Str:D \prop-name, |args) {
            run 'xinput', 'set-prop', xinput-id, prop-name, |args
          }

          {${rakuCommands}}
          $is-found = True;
        }

        unless $is-found {
          $*ERR.say: join q< >, [
            "Pointer “{${escRaku pointerName}}” is not found",
            "by “{${escRaku deviceName}}” device name"
              ${if isNull deviceProductId then "" else ''
                , "and by “{${escRaku deviceProductId}}” value"
                , "for “{${escRaku productIdPropName}}” property"
              ''} ~ "!",
          ];
          exit 1
        }
      '';

      verfiyProductId = ''
        read-props;
        my Str:D \prop-name = ${escRaku productIdPropName};

        unless %props{prop-name}:exists {
          $*ERR.say: join q< >, [
            "WARNING!",
            "“{prop-name}” property is not found for xinput device ID #{xinput-id}!",
          ];
          next
        }

        my Str:D \prop-value = %props{prop-name};

        unless prop-value eq ${escRaku deviceProductId} {
          $*ERR.say: join q< >, [
            "WARNING!",
            "“{prop-name}” property for found device ID #{xinput-id} mismatches expected value",
            "(got “{prop-value}” while it should be “{${escRaku deviceProductId}}”)",
          ];
          next
        }
      '';
    in
    assert builtins.isString pointerName;
    assert builtins.isString deviceName;
    assert (deviceProductId != null) -> builtins.isString deviceProductId;
    assert builtins.isString rakuCommands;
    {
      ${name} = symlinkJoin {
        inherit name;
        paths = [ script ];
        nativeBuildInputs = [ makeWrapper ];
        postBuild = ''
          if ! ( [[ -f ${esc raku-exe} && -r ${esc raku-exe} && -x ${esc raku-exe} ]] ); then
            >&2 printf '"%s" must be a readable executable file but it is not!\n' ${esc raku-exe}
            false
          fi
          SCRIPT_BIN_PATH="$out"/bin/${esc name}
          ${esc raku-exe} -c -- "$SCRIPT_BIN_PATH" # Syntax check
          wrapProgram "$SCRIPT_BIN_PATH" \
            --prefix PATH : ${esc (lib.makeBinPath [ xlibs.xinput ])}
        '';
      };
    };
in
makePointer {
  pointerName = "razor-wired-ambidextrous-mouse";
  deviceName = "Razer Razer Abyssus 2000";
  deviceProductId = "5426, 94";
  rakuCommands = ''
    with-prop 'libinput Left Handed Enabled', { set-prop $_<name>, '1' }
    with-prop 'libinput Accel Speed', { set-prop $_<name>, '-0.7' }
  '';
}
//
(
  let
    f = productId: cmdSuffix: deviceName: makePointer {
      pointerName = "logitech-g-pro-ambidextrous-mouse-${cmdSuffix}";
      inherit deviceName;
      deviceProductId = productId;
      rakuCommands = ''
        with-prop 'libinput Left Handed Enabled', { set-prop $_<name>, '1' }
      '';
    };
  in
    f "1133, 16505" "wireless" "Logitech G Pro"
    //
    f "1133, 49288" "wire" "Logitech G Pro Wireless Gaming Mouse"
)
//
makePointer {
  pointerName = "logitech-wireless-ambidextrous-small-mouse";
  deviceName = "Logitech Wireless Mouse";
  deviceProductId = "1133, 16469";
  rakuCommands = ''
    with-prop 'libinput Left Handed Enabled', { set-prop $_<name>, '1' }
  '';
}
//
makePointer {
  pointerName = "logitech-wireless-t650-touchpad";
  deviceName = "Logitech Rechargeable Touchpad T650";
  rakuCommands = ''
    with-prop 'libinput Natural Scrolling Enabled', { set-prop $_<name>, '1' }
  '';
}
//
makePointer {
  pointerName = "dell-latitude-laptop-touchpad";
  deviceName = "DELL081C:00 044E:121F Touchpad";
  rakuCommands = ''
    with-prop 'libinput Natural Scrolling Enabled', { set-prop $_<name>, '1' }
    with-prop 'libinput Left Handed Enabled', { set-prop $_<name>, '1' }
    with-prop 'libinput Tapping Enabled', { set-prop $_<name>, '0' }
  '';
}
//
makePointer {
  pointerName = "dell-latitude-laptop-dot";
  deviceName = "DELL081C:00 044E:121F Mouse";
  rakuCommands = ''
    with-prop 'libinput Natural Scrolling Enabled', { set-prop $_<name>, '0' }
    with-prop 'libinput Left Handed Enabled', { set-prop $_<name>, '1' }
    with-prop 'libinput Scroll Method Enabled', { set-prop $_<name>, qw<{0 0 1}> }
  '';
}
