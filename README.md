# My NixOS Configuration

## Usage

This config is designed to be used with own precise `nixos` channel pin.
Not that it wouldn’t work with default `nixos-20.09` channel
but I like to manually update it and to store the precise pin in the repo
in order to be sure my machines are 100% in sync.
In short, to have better reproducibility.

Please see `--help` info from [this script](channels/manage.raku):

``` sh
channels/manage.raku --help
```

In order to fully setup channels you just have to run:

``` sh
channels/manage.raku fetch
channels/manage.raku override
```

*P.S. Mind that the second command (`override`) requires “sudo” access.*

1. Load into [NixOS Live CD](https://nixos.org/download.html)

1. Prepare file system according to a hardware configuration of a machine
   (see `hardware` directory, write new one for new hardware)
   and mount everything into `/mnt` as the root of the system you’re about to build

1. Clone this repo into `/mnt/etc/nixos` and `cd` to that dir:

   ```sh
   sudo mkdir /mnt/etc
   sudo nix-shell -p git --run 'git clone https://github.com/unclechu/nixos-config.git /mnt/etc/nixos'
   cd /mnt/etc/nixos
   ```

1. Link related hardware config as `machine-specific.nix`:

   ```sh
   sudo ln -s hardware/wenzel-nixos-pc.nix machine-specific.nix
   ```

1. Install:

   ```sh
   sudo nix-shell -p git --run 'nixos-install --no-root-passwd --root /mnt'
   ```

1. Change the password in order to be able to login:

   ```sh
   sudo nixos-enter --root /mnt -c 'passwd wenzel'
   ```

1. Reboot into the built system:

   ```sh
   reboot
   ```

## How to manage external dependencies

Apart from general “nixos” and “nixos-unstable” channels some another external dependencies
(such as [Home Manager] and my own [nix-utils]) are managed using [niv] tool.
Those dependencies are described in [nix/sources.json].

You don’t need [niv] to install the dependencies, only to update them, remove or add a new one
(but you also can as well manually edit that [nix/sources.json] file).

*N.B. “nixos” channel can’t be managed by [niv] in a NixOS configuration because it’s imported even
before `configuration.nix` is called. `configuration.nix` is called with `pkgs` argument which is
imported already. Also all the root system dependencies are provided by that channel.*

## Known issues

1. In a new installed NixOS from this config I had to manually create
   `/nix/var/nix/profiles/per-user/wenzel` directory and `chown` it to `wenzel:root` (as in the
   first machine). Otherwise [Home Manager] systemd service was failing with error of that directory
   doesn’t exists. NixOS also couldn’t be rebuilt before that directory has been created.

   **TODO** Check on new installation whether there is still this error.

## Author

[Viacheslav Lotsmanov](mailto:lotsmanov89@gmail.com)

[Home Manager]: https://github.com/nix-community/home-manager
[nix-utils]: https://github.com/unclechu/nix-utils
[niv]: https://github.com/nmattia/niv#readme
[nix/sources.json]: nix/sources.json
