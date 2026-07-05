# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, callPackage
, writeText

, rakudo
, xinput

, pointer-setup ? callPackage ./pointer-setup {}
}:
let
  makePointer =
    { pointerName # This will be a name suffix for the resulting derivation
    , deviceName # xinput device name
    , deviceProductId ? null # Additional validation by Device Product ID
    , setProps ? {}
    , name ? "pointer-${pointerName}"
    }: {
      ${name} =
        pointer-setup.make-pointer-setup-wrapper {
          pointer-name = name;
          device-name = deviceName;
          device-product-id = deviceProductId;
          set-props = setProps;
        };
    };
in
makePointer {
  pointerName = "razor-wired-ambidextrous-mouse";
  deviceName = "Razer Razer Abyssus 2000";
  deviceProductId = "5426, 94";
  setProps = {
    "libinput Left Handed Enabled" = ["1"];
    "libinput Accel Speed" = ["-0.7"];
  };
}
//
(
  let
    f = deviceProductId: cmdSuffix: deviceName: makePointer {
      pointerName = "logitech-g-pro-ambidextrous-mouse-${cmdSuffix}";
      inherit deviceName deviceProductId;
      setProps = { "libinput Left Handed Enabled" = ["1"]; };
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
  setProps = { "libinput Left Handed Enabled" = ["1"]; };
}
//
makePointer {
  pointerName = "logitech-wireless-t650-touchpad";
  deviceName = "Logitech Rechargeable Touchpad T650";
  setProps = { "libinput Natural Scrolling Enabled" = ["1"]; };
}
//
makePointer {
  pointerName = "dell-latitude-laptop-touchpad";
  deviceName = "DELL081C:00 044E:121F Touchpad";
  setProps = {
    "libinput Natural Scrolling Enabled" = ["1"];
    "libinput Left Handed Enabled" = ["1"];
    "libinput Tapping Enabled" = ["0"];
  };
}
//
makePointer {
  pointerName = "dell-inspiron-laptop-touchpad";
  deviceName = "MSFT0001:00 06CB:7E7E Touchpad";
  setProps = {
    "libinput Natural Scrolling Enabled" = ["1"];
    "libinput Left Handed Enabled" = ["1"];
    "libinput Tapping Enabled" = ["1"];
  };
}
//
makePointer {
  pointerName = "dell-latitude-laptop-dot";
  deviceName = "DELL081C:00 044E:121F Mouse";
  setProps = {
    "libinput Natural Scrolling Enabled" = ["0"];
    "libinput Left Handed Enabled" = ["1"];
    "libinput Scroll Method Enabled" = ["0" "0" "1"];
  };
}
//
makePointer {
  pointerName = "rusty-chunk-touchpad";
  deviceName = "SynPS/2 Synaptics TouchPad";
  setProps = {
    "libinput Natural Scrolling Enabled" = ["1"];
    "libinput Left Handed Enabled" = ["1"];
    "libinput Tapping Enabled" = ["1"];
  };
}
//
makePointer {
  pointerName = "logitech-b100";
  deviceName = "Logitech USB Optical Mouse";
  setProps = {
    "libinput Left Handed Enabled" = ["1"];
    "libinput Accel Speed" = ["-1"];
  };
}
