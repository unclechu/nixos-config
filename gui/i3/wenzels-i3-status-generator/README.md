# wenzels-i3-status-generator

My own status line generator for [i3 window manager][i3wm].

## Integration with [xlib-keys-hack][xlib-keys-hack]

It is tightly integrated with my own software-level keyboard customization
utility — [xlib-keys-hack][xlib-keys-hack]. It’s a hard dependency for it to
function properly). This status line generator gets the current state of most
indicators using a [xlib-keys-hack][xlib-keys-hack]’s [D-Bus][dbus] interface.

## Usage

It makes sense to connect this dependency specifically to [i3wm]:

``` nix
{
  services.xserver.windowManager.i3.extraPackages = [
    pkgs.i3status
    wenzels-i3-status-generator
  ];
}
```

In your i3 configuration you can attach it like this:

``` i3config
bar {
  status_command wenzels-i3-status-generator
}
```

## Development/testing

Some useful commands:

``` sh
cabal build -O0
cabal run -O0 exe:wenzels-i3-status-generator
hlint src/
```

## License

[MIT] — For the code of this repository.
Some third-party dependencies may have different licenses.

[MIT]: LICENSE
[i3wm]: https://i3wm.org/
[xlib-keys-hack]: https://github.com/unclechu/xlib-keys-hack
[dbus]: https://www.freedesktop.org/wiki/Software/dbus/
