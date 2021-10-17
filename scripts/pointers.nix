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
      idVar = "$xinput-id";

      script = writeScriptBin name ''
        #! ${rakudo}/bin/raku
        use v6.d;
        close $*IN;

        sub fail(Str:D \msg) { $*ERR.say: msg; exit 1; }

        my @devices = run('xinput', 'list', '--short', :out).out.slurp(:close).chomp.lines;
        @devices .= grep: / '↳ ' ${escRaku deviceName} \s+ 'id='\d+ \s+ /;

        fail "Pointer “{${escRaku pointerName}}” is not found by “{${escRaku deviceName}}” device name!"
          unless @devices.elems > 0;

        # Just first device match is taken (pointers go first, before keyboards)
        my $found-device-line = @devices[0] ~~ / 'id=' $<found-id> = (\d+) /;
        my Num:D ${idVar} = $<found-id>.Num;

        ${if isNull deviceProductId then "" else verfiyProductId idVar}

        ${rakuCommands idVar}
      '';

      verfiyProductId = id: ''
        my regex PropRegEx {
          'Device Product ID' \s+ '(' $<prop-id>=\d+ '):' \s+ $<prop-val>=.+ \s* $
        };

        my @props = run('xinput', 'list-props', ${id}, :out).out.slurp(:close).chomp.lines;
        @props .= grep: /<PropRegEx>/;

        fail
          "“Device Product ID” property is not found for “{${escRaku pointerName}}” pointer " ~
          "(ID #{${id}})"
            unless @props.elems > 0;

        @props[0] ~~ /<PropRegEx>/;

        fail
          "Found pointer “{${escRaku pointerName}}” mismatches Device Product ID " ~
          "(got “{$<PropRegEx><prop-val>}” while should be “{${escRaku deviceProductId}}”)"
            unless $<PropRegEx><prop-val> eq ${escRaku deviceProductId};
      '';
    in
    assert builtins.isString pointerName;
    assert builtins.isString deviceName;
    assert (deviceProductId != null) -> builtins.isString deviceProductId;
    assert builtins.isFunction rakuCommands;
    assert builtins.isString (rakuCommands "$x");
    {
      ${name} = symlinkJoin {
        inherit name;
        paths = [ script ];
        nativeBuildInputs = [ makeWrapper ];
        postBuild = ''
          wrapProgram "$out"/bin/${lib.escapeShellArg name} \
            --prefix PATH : ${lib.escapeShellArg (lib.makeBinPath [ xlibs.xinput ])}
        '';
      };
    };
in
makePointer {
  pointerName = "razor-wired-ambidextrous-mouse";
  deviceName = "Razer Razer Abyssus 2000";
  deviceProductId = "5426, 94";
  rakuCommands = id: ''
    run('xinput', 'set-prop', ${id}, 'libinput Left Handed Enabled', '1');
    run('xinput', 'set-prop', ${id}, 'libinput Accel Speed', '-0.7');
  '';
}
//
makePointer {
  pointerName = "logitech-g-pro-ambidextrous-mouse";
  deviceName = "Logitech G Pro";
  deviceProductId = "1133, 16505";
  rakuCommands = id: ''
    run('xinput', 'set-prop', ${id}, 'libinput Left Handed Enabled', '1');
  '';
}
//
makePointer {
  pointerName = "logitech-wireless-ambidextrous-small-mouse";
  deviceName = "Logitech Wireless Mouse";
  deviceProductId = "1133, 16469";
  rakuCommands = id: ''
    run('xinput', 'set-prop', ${id}, 'libinput Left Handed Enabled', '1');
  '';
}
//
makePointer {
  pointerName = "logitech-wireless-t650-touchpad";
  deviceName = "Logitech Rechargeable Touchpad T650";
  rakuCommands = id: ''
    run('xinput', 'set-prop', ${id}, 'libinput Natural Scrolling Enabled', '1');
  '';
}
//
makePointer {
  pointerName = "dell-latitude-laptop-touchpad";
  deviceName = "DELL081C:00 044E:121F Touchpad";
  rakuCommands = id: ''
    run('xinput', 'set-prop', ${id}, 'libinput Natural Scrolling Enabled', '1');
    run('xinput', 'set-prop', ${id}, 'libinput Left Handed Enabled', '1');
    run('xinput', 'set-prop', ${id}, 'libinput Tapping Enabled', '0');
  '';
}
//
makePointer {
  pointerName = "dell-latitude-laptop-dot";
  deviceName = "DELL081C:00 044E:121F Mouse";
  rakuCommands = id: ''
    run('xinput', 'set-prop', ${id}, 'libinput Natural Scrolling Enabled', '0');
    run('xinput', 'set-prop', ${id}, 'libinput Left Handed Enabled', '1');
    run('xinput', 'set-prop', ${id}, 'libinput Scroll Method Enabled', qw<{0 0 1}>);
  '';
}
