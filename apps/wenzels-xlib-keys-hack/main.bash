_LOGFILE=$HOME/.xlib-keys-hack-fails

exec 1>>"$_LOGFILE" 2>&1
printf -- '->> STARTING at %s … <<-\n' "$(date)"

grant-access-to-input-devices

FLAGS=(
	--disable-reset-by-window-focus-event
)

# IPC stuff
FLAGS+=(
	--xmobar-indicators
	--external-control
)

DUCKY=(
	'--disable-xinput-device-name=Ducky Ducky One2 Mini RGB'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-event-if03'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-event-kbd'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if01-event-mouse'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if01-mouse'
	'/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if02-event-kbd'
)

CORSAIR=(
	'--disable-xinput-device-name=Corsair Corsair Gaming K63 Keyboard'
	'--disable-xinput-device-name=Corsair Corsair Gaming K63 Keyboard Keyboard'
	'--disable-xinput-device-name=Corsair Corsair Gaming K64 Keyboard Consumer Control'
	'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-if01'
	'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-kbd'
	'/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-if01-event-kbd'
)

EMBEDDED=(
	'--disable-xinput-device-name=AT Translated Set 2 keyboard'
	# See below for devices list
)

HAS_DUCKY=0
for x in "${DUCKY[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_DUCKY=1
		break
	fi
done

ERGODOX_EZ=(
	# old firmware
	'--disable-xinput-device-name=ErgoDox EZ ErgoDox EZ'
	/dev/input/by-id/usb-ErgoDox_EZ_ErgoDox_EZ_0-event-kbd

	# new firmware
	'--disable-xinput-device-name=ZSA Ergodox EZ'
	/dev/input/by-id/usb-ZSA_Ergodox_EZ_0-event-kbd
)

HAS_ERGODOX_EZ=0
for x in "${ERGODOX_EZ[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_ERGODOX_EZ=1
		break
	fi
done

PLANK_EZ=(
	'--disable-xinput-device-name=ZSA Planck EZ'
	/dev/input/by-id/usb-ZSA_Planck_EZ_0-event-kbd
)

HAS_PLANK_EZ=0
for x in "${PLANK_EZ[@]}"; do
	if [[ $x =~ ^/dev/input/ && -r $x ]]; then
		HAS_PLANK_EZ=1
		break
	fi
done

MODE=${1:-}

if [[ $MODE == gaming ]]; then
	echo '[ GAMING MODE ON ]'
	if (( HAS_ERGODOX_EZ == 0 && HAS_PLANK_EZ == 0 )); then
		FLAGS+=(
			--real-capslock
			--no-additional-controls
		)
	fi
elif (( HAS_ERGODOX_EZ == 0 && HAS_PLANK_EZ == 0 )); then
	FLAGS+=(
		--hold-alt-for-alternative-mode
		--ergonomic-mode
	)
fi

# Turn off embedded keyboard if there's external one
if (( HAS_DUCKY == 0 && HAS_ERGODOX_EZ == 0 && HAS_PLANK_EZ == 0 )); then
	EMBEDDED+=(
		'/dev/input/by-path/platform-i8042-serio-0-event-kbd'
		'/dev/input/by-path/platform-i8042-serio-1-event-mouse'
		'/dev/input/by-path/platform-i8042-serio-1-mouse'
	)
fi

if (( HAS_ERGODOX_EZ == 0 && HAS_PLANK_EZ == 0 )); then
	FLAGS+=(
		--shift-hjkl
		'--software-debouncer=90'
		--right-control-as-super
	)

	if [[ $MODE == no-numbers-shift ]]; then
		FLAGS+=( --shift-numeric-keys )
	fi
else
	FLAGS+=(
		--real-capslock
		--ergonomic-ergodox-mode
		--f24-as-vertical-bar
		--reset-by-real-escape
	)
	# Keep additional controls feature for Plank EZ
	if (( HAS_PLANK_EZ == 0 )); then
		FLAGS+=( --no-additional-controls )
	fi
	if [[ $MODE == gaming ]]; then
		FLAGS+=(
			--disable-super-double-press
			# --right-super-as-space
		)
	else
		FLAGS+=(
			--hold-alt-for-alternative-mode
		)
	fi
fi

ALL_XKH_ARGS=(
	"${FLAGS[@]}"
	"${EMBEDDED[@]}"
	"${DUCKY[@]}"
	"${CORSAIR[@]}"
	"${ERGODOX_EZ[@]}"
	"${PLANK_EZ[@]}"
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

# --disable-xinput-device-name='Telink Wireless Receiver'
# '/dev/input/by-id/usb-Telink_Wireless_Receiver-event-kbd'
# '/dev/input/by-id/usb-Telink_Wireless_Receiver-if01-event-mouse'
# '/dev/input/by-id/usb-Telink_Wireless_Receiver-if01-mouse'

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
