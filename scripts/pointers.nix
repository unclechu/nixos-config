# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, writeText
, writeShellApplication

# Dependencies for check phase of “writeShellApplication”
, stdenv
, shellcheck

, rakudo
, xorg # Just for ‘xinput’
, which
}:
let
  escRaku = x: "'${lib.escape ["'"] x}'";

  makePointer =
    { pointerName # This will be a suffix for resulting derivation
    , deviceName # xinput device name
    , deviceProductId ? null # Additional validation by Device Product ID
    , rakuCommands # Raku code to evaluate for a found device match
    }: let
      name = "pointer-${pointerName}";
      productIdPropName = "Device Product ID";
      raku-exe = "${rakudo}/bin/raku";
      esc = lib.escapeShellArg;
      releaseModeVarName = "IS_RELEASE_MODE";

      pointerScript = writeText "${name}.raku" ''
        use v6.d;
        close $*IN;

        sub slurp-run (|args) {
          given run |args, :out { LEAVE { .sink }; .out.slurp(:close).chomp }
        }

        my Bool:D \is-release = (%*ENV{${escRaku releaseModeVarName}} // 0) == 1;

        sub checked-exe (Str:D \exe) of IO::Path:D {
          IO::Path.new: is-release ?? exe !! slurp-run(«which --», exe)
        }

        # Guard dependencies
        my IO::Path:D \xinput = CHECK { checked-exe 'xinput' };

        my regex PropValueRegEx {
          ^\s* $<name>=<-[:]>+ \s+ '(' $<prop-id>=\d+ '):' \s* $<value>=<-[:]>+ \s*$
        };

        my Str:D @devices = slurp-run(xinput, 'list', '--short').lines;
        @devices .= grep: / '↳ ' ${escRaku deviceName} \s+ 'id='\d+ \s+ /;
        my Bool:D $is-found = False;

        for @devices -> $device {
          my $found-device-line = $device ~~ / 'id=' $<found-id> = (\d+) /;
          my Num:D \xinput-id = $<found-id>.Num;

          my Str %props;
          my Bool:D $props-are-read = False;

          sub read-props {
            return if $props-are-read;
            my @props = slurp-run(xinput, 'list-props', xinput-id).lines;
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
            run xinput, 'set-prop', xinput-id, prop-name, |args
          }

          $*ERR.say: "Handling matching xinput device ID #{xinput-id}…";
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
      ${name} = writeShellApplication {
        inherit name;
        runtimeInputs = [ xorg.xinput ];
        text = ''
          raku_args=()
          VAR_NAME=${esc releaseModeVarName}
          if [[ ''${!VAR_NAME:-1} != 1 ]]; then raku_args+=(-c); fi
          ${esc raku-exe} "''${raku_args[@]}" -- ${esc pointerScript} "$@"
        '';
        checkPhase = ''
          runHook preCheck
          PROGRAM=$out/bin/${esc name}
          ${esc stdenv.shell} -n -- "$PROGRAM"
          ${esc shellcheck}/bin/shellcheck -- "$PROGRAM"
          env \
            PATH=${esc (lib.makeBinPath [ which ])}:"$PATH" \
            ${esc releaseModeVarName}=0 \
            ${esc stdenv.shell} -- "$PROGRAM"
          runHook postCheck
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
  pointerName = "dell-inspiron-laptop-touchpad";
  deviceName = "MSFT0001:00 06CB:7E7E Touchpad";
  rakuCommands = ''
    with-prop 'libinput Natural Scrolling Enabled', { set-prop $_<name>, '1' }
    with-prop 'libinput Left Handed Enabled', { set-prop $_<name>, '1' }
    with-prop 'libinput Tapping Enabled', { set-prop $_<name>, '1' }
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
