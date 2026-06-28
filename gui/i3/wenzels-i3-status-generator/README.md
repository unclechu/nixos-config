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
cabal build
cabal run exe:wenzels-i3-status-generator
cabal v2-repl library:wenzels-i3-status-generator
hlint src/
```

Note that “production” build is when you build the derivation with Nix.
When you build it for development the optimizations are disabled in
[cabal.project](cabal.project) file. You can always force the optimizations:

``` sh
cabal build -O1
```

### Vim configuration

Haskell Language Server (also known as [HLS][HLS]) is already provided in the
nix-shell configuration for this project. To enable it for my
[Neovim config](https://github.com/unclechu/neovimrc) I do this:

``` vim
if !exists('g:ale_linters') | let g:ale_linters = {} | endif
let g:ale_linters.haskell = []

if executable('haskell-language-server-wrapper')
	SetupNeovimLsp
endif
```

[hie.yaml](hie.yaml) configures HLS for this package.

## License

[MIT] — For the code of this repository.
Some third-party dependencies may have different licenses.

[MIT]: LICENSE
[i3wm]: https://i3wm.org/
[xlib-keys-hack]: https://github.com/unclechu/xlib-keys-hack
[dbus]: https://www.freedesktop.org/wiki/Software/dbus/
[HLS]: https://github.com/haskell/haskell-language-server
