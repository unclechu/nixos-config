# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, rustPlatform
, gtk3-x11
, atk
, gdk_pixbuf
, pango
, cairo
, glib
, bash

, neovim # Set to ‘null’ to use default ‘nvim’ from ‘PATH’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, bashAliasesFile # Set to ‘null’ to use default ‘~/.bash_aliases’
}:
let
  inherit (__nix-utils) esc nameOfModuleFile writeCheckedExecutable wrapExecutable shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  neovim-gtk = rustPlatform.buildRustPackage {
    pname   = name;
    version = "git-master-2021-01-02";
    src     = sources.${name};

    cargoSha256 = "1l6fly2s493mk997b090ms6n6874pl7al9a9fmwf6k8ixvvzg8gr";

    meta = {
      description =
        "GTK UI for Neovim with ligatures support (written in Rust using gtk-rs bindings)";

      homepage    = "https://github.com/daa84/neovim-gtk";
      license     = lib.licenses.gpl3;
      maintainers = [];
    };

    buildInputs = [
      gtk3-x11
      atk
      gdk_pixbuf
      pango
      cairo
      glib
    ];
  };

  wrapped-neovim-gtk =
    if isNull neovim
    then neovim-gtk
    else wrapExecutable "${neovim-gtk}/bin/nvim-gtk" { deps = [ neovim ]; };

  bash-exe = "${bash}/bin/bash";
  nvim-gtk = "${wrapped-neovim-gtk}/bin/nvim-gtk";

  # TODO Use ‘wrapExecutable’ instead
  g = writeCheckedExecutable "g" ''
    ${shellCheckers.fileIsExecutable bash-exe}
    ${shellCheckers.fileIsExecutable nvim-gtk}
    ${
      if isNull bashAliasesFile
      then ""
      else shellCheckers.fileIsReadable bashAliasesFile
    }
  '' ''
    #! ${bash-exe}
    . ${
      if isNull bashAliasesFile
      then "~/.bash_aliases"
      else esc bashAliasesFile
    } || exit
    export NVIM_GTK_NO_HEADERBAR=1 || exit
    notm burp ${esc nvim-gtk} "$@"
  '';
in
wrapped-neovim-gtk // { inherit g; }
