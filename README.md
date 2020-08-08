# My NixOS Configuration

## Usage

**WARNING!** Mind that at the moment of 9 August 2020 it relies on **nixos-unstable** channel
because I needed `config.qt5.style = "adwaita-dark"` option to work. **nixos-20.03** wouldn’t
compile with this option. So you would like to run this first in order to make it work:

```sh
nix-channel --remove nixos
nix-channel --add https://nixos.org/channels/nixos-unstable nixos
nix-channel --update nixos
```

1. Load into [NixOS Live CD](https://nixos.org/download.html)

1. Prepare file system according to a hardware configuration of a machine
   (see `hardware` directory, write new one for new hardware)
   and mount everything into `/mnt` as the root of the system you're about to build

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

## Known issues

1. In a new installed NixOS from this config I had to manually create
   `/nix/var/nix/profiles/per-user/wenzel` directory and `chown` it to `wenzel:root` (as in the
   first machine). Otherwise Home Manager systemd service was failing with error of that directory
   doesn't exists. NixOS also couldn't be rebuilt before that directory has been created.

## Author

[Viacheslav Lotsmanov](mailto:lotsmanov89@gmail.com)
