let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}

, bashAliasesFile # set as ‘null’ to use default ‘~/.bash_aliases’
, neovim          # set as ‘null’ to use default ‘nvim’ from ‘PATH’
}:
let
  inherit (nix-utils) esc nameOfModuleFile writeCheckedExecutable wrapExecutable;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  neovim-gtk = pkgs.rustPlatform.buildRustPackage {
    pname   = name;
    version = "git-master-2021-01-02";

    src = pkgs.fetchFromGitHub {
      owner  = "daa84";
      repo   = name;
      rev    = "c03649276ee47caa9bc7beb68f6c800b8c97651a";
      sha256 = "1v026lz8ww94yyl0vbqsnp55py2fsa09vcgiqa8nc7cp005418kn";
    };

    cargoSha256 = "1839a9q3dlijkd7nfbaw04kfhbbikhkhzzbja12ny05z50n7rrnm";

    meta = {
      description = "GTK UI for Neovim written in Rust using gtk-rs bindings with ligatures support";
      homepage    = "https://github.com/daa84/neovim-gtk";
      license     = pkgs.lib.licenses.gpl3;
      maintainers = [];
    };

    buildInputs = [
      pkgs.gtk3-x11
      pkgs.atk
      pkgs.gdk_pixbuf
      pkgs.pango
      pkgs.cairo
      pkgs.glib
    ];
  };

  wrapped-neovim-gtk =
    if isNull neovim
    then neovim-gtk
    else wrapExecutable "${neovim-gtk}/bin/nvim-gtk" { deps = [ neovim ]; };

  bash = "${pkgs.bash}/bin/bash";
  nvim-gtk = "${wrapped-neovim-gtk}/bin/nvim-gtk";

  # TODO Use ‘wrapExecutable’ instead
  g = writeCheckedExecutable "g" ''
    ${nix-utils.shellCheckers.fileIsExecutable bash}
    ${nix-utils.shellCheckers.fileIsExecutable nvim-gtk}
    ${
      if isNull bashAliasesFile
      then ""
      else nix-utils.shellCheckers.fileIsReadable bashAliasesFile
    }
  '' ''
    #! ${bash}
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
