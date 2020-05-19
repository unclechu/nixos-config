# My NixOS Configuration

## Usage

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
   sudo nixos-install --root /mnt
   ```

1. Change the password in order to be able to login:

   ```sh
   sudo nixos-enter --root /mnt -c 'passwd wenzel'
   ```

1. Reboot into the built system:

   ```sh
   reboot
   ```

**TODO** fill this README

## Author

[Viacheslav Lotsmanov](mailto:lotsmanov89@gmail.com)
