# XMonad configuration

## Development environment

Just enter nix-shell (in my config I can just use `nsh` Bash alias).
Then inside it both GHC with all the dependencies and HIE will be available.
In my Neovim setup I can call `SetupNeovimLsp` and it will be enough for
LSP integration with HIE. The defaults will work fine.
