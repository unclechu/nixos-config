#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# A list of available modes (for arguments validation)
declare -A MODES && MODES=([gaming]=1 [no-numbers-shift]=1)

MODE=
IS_DEBUG=0
IS_REGULAR_KEYBOARD_CONFIG_FORCED=0

# Parse command-line arguments
for arg in "$@"; do
	# Turning on debug mode
	if [[ $arg == debug ]]; then
		IS_DEBUG=1
	# Forcing regular keyboard configuration.
	# Even in presence of more advanced keyboard like ErgoDox
	# enforce use of settings for the regular QWERTY keyboard.
	elif [[ $arg == force-regular ]]; then
		IS_REGULAR_KEYBOARD_CONFIG_FORCED=1
	# Mode switch
	elif [[ -n ${MODES[$arg]:-} ]]; then
		MODE=$1
	else
		>&2 printf 'Incorrect argument: “%s”\n' "$arg"
		exit 1
	fi
done

if (( IS_DEBUG == 1 )); then
	# Print the logs to the stdout & stderr instead of
	# redirecting everything to the log file (default behavior).

	# Debug all commands
	set -o xtrace
else
	# By default redirect the logs with potential
	# failures to the file for later analysis.
	_LOGFILE=$HOME/.xlib-keys-hack-fails
	exec 1>>"$_LOGFILE" 2>&1
	printf -- '->> STARTING at %s … <<-\n' "$(date)"
fi

grant-access-to-input-devices

# Feature turning on/off flags (arguments list for “xlib-keys-hack”)
FLAGS=(
	--disable-reset-by-window-focus-event
	--default-keyboard-layout=3 # Finnish layout by default
)

# Turning on IPC stuff
FLAGS+=(
	--xmobar-indicators
	--external-control
)

# Corsair K63 mechanical gaming keyboard
CORSAIR=(
	'--disable-xinput-device-name=Corsair Corsair Gaming K63 Keyboard'
	'--disable-xinput-device-name=Corsair Corsair Gaming K63 Keyboard Keyboard'
	'--disable-xinput-device-name=Corsair Corsair Gaming K64 Keyboard Consumer Control'
	'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-if01'
	'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-kbd'
	'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-if01-event-kbd'
)

# Oklick 910G mechanical gaming keyboard
OKLICK=(
	'--disable-xinput-device-name=HID 04b4:6018'
	'--disable-xinput-device-name=HID 04b4:6018 System Control'
	'--disable-xinput-device-name=HID 04b4:6018 Keyboard'
	'--disable-xinput-device-name=HID 04b4:6018 Consumer Control'
	'/dev/input/by-id/usb-04b4_6018-event-if01'
	'/dev/input/by-id/usb-04b4_6018-event-kbd'
	'--disable-xinput-device-name=HID 04b4:6018 Mouse'
	'--disable-xinput-device-name=HID 04b4:6018 Consumer Control'
	# '/dev/input/by-id/usb-04b4_6018-if01-mouse'
	# '/dev/input/by-id/usb-04b4_6018-if01-event-mouse'
)

# Embedded laptop keyboard.
EMBEDDED=(
	'--disable-xinput-device-name=AT Translated Set 2 keyboard'
)
# Conditionally added list of embedded device file descriptors.
#
# Embedded keyboard is disabled (this devices list is not added) in presence of
# external one. Because of two reasons:
#
#   1. The configuration can be pretty different and thus imcompatible between
#      “conventional” QWERTY keyboard layout and something like ortholinear
#      ErgoDox.
#   2. A better keyboard can be placed on top of the laptop, thus randomly
#      pushing keys of the embedded keyboard.
EMBEDDED_DEVICES=(
	'/dev/input/by-path/platform-i8042-serio-0-event-kbd'
	# I don’t know why I added this before, for my another laptop.
	# But when I tried it on another much older laptop I was getting
	# “unknown event” fatal failure from the “linux-evdev” library
	# as soon as I try to move the cursor using the touchpad.
	# As the name of the file says you these are clearly mouse events files.
	# '/dev/input/by-path/platform-i8042-serio-1-event-mouse'
	# '/dev/input/by-path/platform-i8042-serio-1-mouse'
)

JETACCESS_SLIM_LINE_K9_WIRELESS=(
	'--disable-xinput-device-name=Telink Wireless Receiver'
	'/dev/input/by-id/usb-Telink_Wireless_Receiver-event-kbd'
	'/dev/input/by-id/usb-Telink_Wireless_Receiver-if01-event-mouse'
	'/dev/input/by-id/usb-Telink_Wireless_Receiver-if01-mouse'
)

# Ducky One 2 Mini RGB keyboard
DUCKY=(
	'--disable-xinput-device-name=Ducky Ducky One2 Mini RGB'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-event-if03'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-event-kbd'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if01-event-mouse'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if01-mouse'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if02-event-kbd'
)

# Boolean flag indicates that Ducky is connected
HAS_DUCKY=0
for x in "${DUCKY[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_DUCKY=1
		break
	fi
done

# ZSA ErgoDox EZ keyboard
ERGODOX_EZ=(
	'--disable-xinput-device-name=ZSA Technology Labs Inc ErgoDox EZ Glow'
	'--disable-xinput-device-name=ZSA Technology Labs Inc ErgoDox EZ Glow Keyboard'
	/dev/input/by-id/usb-ZSA_Technology_Labs_Inc_ErgoDox_EZ_Glow-event-kbd

	'--disable-xinput-device-name=ZSA Technology Labs Inc ErgoDox EZ Shine'
	'--disable-xinput-device-name=ZSA Technology Labs Inc ErgoDox EZ Shine Keyboard'
	/dev/input/by-id/usb-ZSA_Technology_Labs_Inc_ErgoDox_EZ_Shine-event-kbd
)

# Boolean flag indicates that Ergodox is connected
HAS_ERGODOX_EZ=0
for x in "${ERGODOX_EZ[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_ERGODOX_EZ=1
		break
	fi
done

# ZSA Planck EZ keyboard
PLANCK_EZ=(
	'--disable-xinput-device-name=ZSA Technology Labs Planck EZ Glow'
	'--disable-xinput-device-name=ZSA Technology Labs Planck EZ Glow Keyboard'
	/dev/input/by-id/usb-ZSA_Technology_Labs_Planck_EZ_Glow-event-kbd
)

# Boolean flag indicates that Planck is connected
HAS_PLANCK_EZ=0
for x in "${PLANCK_EZ[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_PLANCK_EZ=1
		break
	fi
done

# ZSA Moonlander Mark I keyboard
MOONLANDER=(
	'--disable-xinput-device-name=ZSA Moonlander Mark I'
	'--disable-xinput-device-name=ZSA Moonlander Mark I Keyboard'
	/dev/input/by-id/usb-ZSA_Moonlander_Mark_I-event-kbd
)

HAS_MOONLANDER=0
for x in "${MOONLANDER[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_MOONLANDER=1
		break
	fi
done

# There is/are only regular QWERTY keyboard(s) present.
HAVE_ONLY_REGULAR_KEYBOARDS=0
if (( HAS_ERGODOX_EZ == 0 && HAS_MOONLANDER == 0 && HAS_PLANCK_EZ == 0 )); then
	HAVE_ONLY_REGULAR_KEYBOARDS=1
fi

# When gaming mode is on “additional controls” feature should be disabled so those keys will be
# triggered immediately when they are just pressed.
# Also on a regular qwerty keyboard it’s more useful to have Caps Lock key instead of remapped
# Escape on its place so you can still have Escape for whatever purpuse in a game. And in some games
# you can’t even map Escape as you would map any other key since it’s reserved to call game menu for
# instance.
if [[ $MODE == gaming ]]; then
	echo '[ GAMING MODE ON ]'
	if (( IS_REGULAR_KEYBOARD_CONFIG_FORCED == 1 || HAVE_ONLY_REGULAR_KEYBOARDS == 1 )); then
		FLAGS+=(
			--real-capslock
			--no-additional-controls
		)
	fi

# Ergonomic mode for regular qwerty keyboard
elif (( IS_REGULAR_KEYBOARD_CONFIG_FORCED == 1 || HAVE_ONLY_REGULAR_KEYBOARDS == 1 )); then
	FLAGS+=(
		--hold-alt-for-alternative-mode
		--ergonomic-mode
	)
fi

# Turn off embedded keyboard if there's external one.
# External one that is either usually placed on top of a laptop or has incompatible settings with
# something like ErgoDox.
if (( HAS_DUCKY == 0 && HAS_ERGODOX_EZ == 0 && HAS_MOONLANDER == 0 && HAS_PLANCK_EZ == 0 )); then
	EMBEDDED+=("${EMBEDDED_DEVICES[@]}")
fi

# If it’t a regular qwerty keyboard (including Ducky).
# Shift “hjkl” on software level.
# Turn on software debouncer (in order to fix issues with key bouncing on Ducky).
# Right Control as Super to have simmetrical Supers on my laptop’s embedded keyboard
# (control keys are provided by “additional controls” feature anyway).
if (( IS_REGULAR_KEYBOARD_CONFIG_FORCED == 1 || HAVE_ONLY_REGULAR_KEYBOARDS == 1 )); then
	FLAGS+=(
		--shift-hjkl
		'--software-debouncer=90'
		--right-control-as-super
	)

	if [[ $MODE == no-numbers-shift ]]; then
		FLAGS+=( --shift-numeric-keys )
	fi

# ErgoDox, Moonlander Mark I, or Planck
else
	FLAGS+=(
		--real-capslock
		--ergonomic-ergodox-mode
		--f24-as-vertical-bar
		--reset-by-real-escape
	)
	# Keep additional controls feature for Planck EZ
	if (( HAS_PLANCK_EZ == 0 )); then
		FLAGS+=( --no-additional-controls )
	# On Planck it’s Escape key which is right before A key,
	# making it work as an additional control.
	else
		FLAGS+=( --escape-is-additional-control )
	fi
	# Super double press feature may stand in your way if you press Super key too often in a game
	if [[ $MODE == gaming ]]; then
		FLAGS+=(
			--disable-super-double-press
			# --right-super-as-space
		)
	# Alternative mode turned on by held Alt keys is probably not a good idea in gaming mode.
	# In gaming mode your probably want to trigger Alts immediately when you press them.
	else
		FLAGS+=(
			--hold-alt-for-alternative-mode
		)
	fi
fi

# All arguments combined for “xlib-keys-hack”
ALL_XKH_ARGS=(
	"${FLAGS[@]}"
	"${EMBEDDED[@]}"
	"${JETACCESS_SLIM_LINE_K9_WIRELESS[@]}"
	"${DUCKY[@]}"
	"${CORSAIR[@]}"
	"${OKLICK[@]}"
	"${ERGODOX_EZ[@]}"
	"${MOONLANDER[@]}"
	"${PLANCK_EZ[@]}"
)

xlib-keys-hack "${ALL_XKH_ARGS[@]}" &
XKH_PID=$!

cleanup() { kill -- "$XKH_PID"; }
trap cleanup ABRT EXIT HUP INT PIPE QUIT TERM TRAP

exit_status=0
if ! wait -- "$XKH_PID"; then exit_status=$?; fi
DATE=$(date)
printf -- '->> Ended with exit code: %d (at %s) <<-\n' "$exit_status" "$DATE"

# --disable-xinput-device-name='Telink Wireless Receiver'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1:1.0-event-kbd'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1:1.1-event-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1:1.1-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:4:1.0-event-kbd'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:4:1.1-event-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:4:1.1-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1.1:1.0-event-kbd'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1.1:1.1-event-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1.1:1.1-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1.4:1.0-event-kbd'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1.4:1.1-event-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:1.4:1.1-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:4.4:1.0-event-kbd'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:4.4:1.1-event-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:4.4:1.1-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:3:1.0-event-kbd'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:3:1.1-event'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:3:1.1-event-mouse'
# '/dev/input/by-path/pci-0000:00:14.0-usb-0:3:1.1-mouse'

# --disable-xinput-device-name='Apple Inc. Apple Keyboard'
# '/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-kbd'
# '/dev/input/by-id/usb-Apple_Inc._Apple_Keyboard-event-if01'

# --disable-xinput-device-name='Microsoft Microsoft® 2.4GHz Transceiver v9.0'
# '/dev/input/by-id/usb-Microsoft_Microsoft®_2.4GHz_Transceiver_v9.0-event-kbd'
# '/dev/input/by-id/usb-Microsoft_Microsoft®_2.4GHz_Transceiver_v9.0-if02-event-kbd'

# --disable-xinput-device-name='2.4G Receiver'
# '/dev/input/by-id/usb-1d57_2.4G_Receiver-event-kbd'
# '/dev/input/by-id/usb-1d57_2.4G_Receiver-event-if02'
