# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ lib
, rustPlatform
, neovide
, newNeovideSrc ? sources.neovide

# With default Clang 11 it seems to infinitely stuck with full CPU load on
# “Compiling unicode-segmentation v1.10.1”. Maybe it’s actually not infinite
# but just takes too long. Clang 16 took also quite a bit of time but it seems
# it at least finishes faster.
, clang16Stdenv
}:
(neovide.override { clangStdenv = clang16Stdenv; }).overrideAttrs (old: {
  src = newNeovideSrc;
  version = "git-${newNeovideSrc.branch}-${builtins.substring 0 7 newNeovideSrc.rev}";

  cargoDeps = rustPlatform.importCargoLock {
    lockFile = "${newNeovideSrc}/Cargo.lock";

    outputHashes = {
      "winit-0.28.6" = "sha256-mxX+v16lMdYahPxVsDg2gMmZtx6qBF4Wu4SlNh4MYY0=";
    };
  };
})
