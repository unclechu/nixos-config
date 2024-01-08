# XMonad configuration

## Development environment

Just enter nix-shell (in my config I can just use `nsh` Bash alias).
Then inside it both [GHC] with all the dependencies and [HLS] will be available.
In my Neovim setup I can call `SetupNeovimLsp` and it will be enough for
[LSP] integration with [HLS]. The defaults will work fine.

[GHC]: https://www.haskell.org/ghc/
[HLS]: https://github.com/haskell/haskell-language-server
[LSP]: https://en.wikipedia.org/wiki/Language_Server_Protocol
