### Vim configuration

[nix-shell configuration](default.nix) provides either
[nimlsp](https://github.com/PMunch/nimlsp) or
[nimlangserver](https://github.com/nim-lang/langserver) depending on `__nimLsp` nix-shell argument
(one of: `[null "nimlsp" "nimlangserver"]`, default is `nimlsp`).
To enable it for my [Neovim config](https://github.com/unclechu/neovimrc) I do this:

``` vim
se cc=81,101

if !exists('g:ale_linters') | let g:ale_linters = {} | endif
let g:ale_linters.nim = []

if executable('nimlsp')
lua <<EOF
vim.g.SetupNeovimLsp({
	nimls = { cmd = { "nimlsp" } }
})
EOF
endif

if executable('nimlangserver')
lua <<EOF
vim.g.SetupNeovimLsp({
	nim_langserver = { cmd = { "nimlangserver" } }
})
EOF
endif
```

#### Known issues

Note that `nimlangserver` is not currently working for me.
When I try to open a `*.nim` file I get in Neovim:
> Client nim_langserver quit with exit code 0 and signal 11. Check log for errors: /home/wenzel/.local/state/nvim/lsp.log

I checked the log file and there is this in there:
> SIGSEGV: Illegal storage access. (Attempt to read from nil?)

I’m not sure what is it missing. But for now I stick just `nimlsp`.

But also note that `nimlsp` has issues with Nim `2.2.*` so in nix-shell
I use `2.0.*` (automatically switched when `__nimLsp == "nimlsp"`).
See https://github.com/PMunch/nimlsp/issues/180
But the final Nix derivation is built with newer Nim regardless.

I also tried `nimlangserver` with Nim `2.0.*` in case it makes a difference:

``` nix
nimlangserver-nim-2_0 = (nimlangserver.override {
  buildNimPackage = buildNimPackage.override {
    nim2 = nim-2_0;
  };
}).overrideAttrs (old: {
  postPatch = (old.postPatch or "") + ''
    substituteInPlace nimlangserver.nim \
      --replace-fail "of lsp:" "of ServerMode.lsp:" \
      --replace-fail "of mcp:" "of ServerMode.mcp:"
  '';
});
```

But it doesn’t help.
