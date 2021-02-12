#! /usr/bin/env bash

_LOGFILE=$HOME/.xlib-keys-hack-fails

exec 1>>"$_LOGFILE" 2>&1
printf -- '->> STARTING at %s … <<-\n' "$(date)"

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

# Embedded laptop keyboard
EMBEDDED=(
	'--disable-xinput-device-name=AT Translated Set 2 keyboard'
	# See below for devices list
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
	# old firmware
	'--disable-xinput-device-name=ErgoDox EZ ErgoDox EZ'
	/dev/input/by-id/usb-ErgoDox_EZ_ErgoDox_EZ_0-event-kbd

	# new firmware
	'--disable-xinput-device-name=ZSA Ergodox EZ'
	/dev/input/by-id/usb-ZSA_Ergodox_EZ_0-event-kbd
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
	'--disable-xinput-device-name=ZSA Planck EZ'
	/dev/input/by-id/usb-ZSA_Planck_EZ_0-event-kbd
)

# Boolean flag indicates that Planck is connected
HAS_PLANCK_EZ=0
for x in "${PLANCK_EZ[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_PLANCK_EZ=1
		break
	fi
done

# A list of available modes (for arguments validation)
declare -A MODES && MODES=([gaming]=1 [no-numbers-shift]=1)

# Parse command-line arguments
if (( $# == 0 )); then
	MODE=
elif (( $# == 1 )) && [[ -n ${MODES[$1]:-} ]]; then
	MODE=$1
else
	>&2 echo 'Arguments are incorrect:'
	>&2 printf ' "%s"' "${@//\"}"
	>&2 echo
	exit 1
fi

# When gaming mode is on “additional controls” feature should be disabled so those keys will be
# triggered immediately when they are just pressed.
# Also on a regular qwerty keyboard it’s more useful to have Caps Lock key instead of remapped
# Escape on its place so you can still have Escape for whatever purpuse in a game. And in some games
# you can’t even map Escape as you would map any other key since it’s reserved to call game menu for
# instance.
if [[ $MODE == gaming ]]; then
	echo '[ GAMING MODE ON ]'
	if (( HAS_ERGODOX_EZ == 0 && HAS_PLANCK_EZ == 0 )); then
		FLAGS+=(
			--real-capslock
			--no-additional-controls
		)
	fi

# Ergonomic mode for regular qwerty keyboard
elif (( HAS_ERGODOX_EZ == 0 && HAS_PLANCK_EZ == 0 )); then
	FLAGS+=(
		--hold-alt-for-alternative-mode
		--ergonomic-mode
	)
fi

# Turn off embedded keyboard if there's external one
if (( HAS_DUCKY == 0 && HAS_ERGODOX_EZ == 0 && HAS_PLANCK_EZ == 0 )); then
	EMBEDDED+=(
		'/dev/input/by-path/platform-i8042-serio-0-event-kbd'
		'/dev/input/by-path/platform-i8042-serio-1-event-mouse'
		'/dev/input/by-path/platform-i8042-serio-1-mouse'
	)
fi

# If it’t a regular qwerty keyboard (including Ducky).
# Shift “hjkl” on software level.
# Turn on software debouncer (in order to fix issues with key bouncing on Ducky).
# Right Control as Super to have simmetrical Supers on my laptop’s embedded keyboard
# (control keys are provided by “additional controls” feature anyway).
if (( HAS_ERGODOX_EZ == 0 && HAS_PLANCK_EZ == 0 )); then
	FLAGS+=(
		--shift-hjkl
		'--software-debouncer=90'
		--right-control-as-super
	)

	if [[ $MODE == no-numbers-shift ]]; then
		FLAGS+=( --shift-numeric-keys )
	fi

# Either ErgoDox or Planck
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
	"${ERGODOX_EZ[@]}"
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

# --disable-xinput-device-name='HID 04b4:6018'
# '/dev/input/by-id/usb-04b4_6018-event-kbd'
# '/dev/input/by-id/usb-04b4_6018-if01-event-mouse'
# '/dev/input/by-id/usb-04b4_6018-if01-mouse'
