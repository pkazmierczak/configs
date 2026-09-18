# Neovim config

Requires **Neovim ≥ 0.12**.

The guiding rule: if Neovim core can do it, core does it. Plugins only fill
what's left.

| Job | Handled by |
| --- | --- |
| Plugin management | `vim.pack` (core) |
| LSP client, diagnostics, completion, snippets | core |
| Syntax highlighting, folding | core treesitter (nvim-treesitter only installs parsers) |
| Comments (`gc`), `[`/`]` motions, editorconfig | core |
| Statusline | ~90 lines of Lua in `lua/config/statusline.lua` |
| Colorscheme | zenbones (light: forestbones, dark: zenbones) |
| Picker / file explorer / TODO highlighting / surround | mini.nvim |
| Git gutter, hunks, inline blame | gitsigns.nvim |
| Prose | render-markdown.nvim + zen-mode.nvim + `:Prose` |

## Layout

```
init.lua                 leader key, then requires the modules below
lua/config/options.lua   editor options
lua/config/plugins.lua   vim.pack specs + plugin setup
lua/config/theme.lua     light/dark colorscheme switching
lua/config/statusline.lua
lua/config/lsp.lua       which servers to run, diagnostics, format-on-save
lua/config/keymaps.lua
lua/config/autocmds.lua
lua/config/prose.lua     prose mode
lua/config/tools.lua     "open line on GitHub", test runner
after/lsp/*.lua          per-server overrides on top of nvim-lspconfig
nvim-pack-lock.json      plugin lockfile — commit this
```

## External tools

Neovim does not install these; your package manager does.

```sh
brew install tree-sitter-cli ripgrep lua-language-server
go install golang.org/x/tools/gopls@latest
rustup component add rust-analyzer clippy
brew install hashicorp/tap/terraform-ls   # optional
```

`tree-sitter-cli` is mandatory: nvim-treesitter's `main` branch shells out to it
to build parsers.

Add a language server by appending its name to `vim.lsp.enable({...})` in
`lua/config/lsp.lua` (any name from nvim-lspconfig works) and, if you need to
tweak it, dropping a `after/lsp/<name>.lua`. Verify with `:checkhealth vim.lsp`.

## Maintenance

```vim
:lua vim.pack.update()        " update all, review the diff, :w to apply, :q to discard
:lua vim.pack.update({'foo'}) " update one
:lua vim.pack.del({'foo'})    " delete from disk (after removing its spec)
:TSUpdate                     " update treesitter parsers
:checkhealth                  " when something feels off
```

## Keymaps

`<leader>` is `<Space>`. Press it and wait — which-key lists everything.

### Core defaults worth remembering (not defined in this config)

| Key | Action |
| --- | --- |
| `K` / `grn` / `gra` / `grr` / `gri` / `grt` / `gO` | hover, rename, code action, references, implementations, type def, symbols |
| `gc` / `gcc` | comment operator / comment line |
| `[d` `]d` · `[q` `]q` · `[b` `]b` · `[<Space>` `]<Space>` | diagnostics, quickfix, buffers, blank lines |
| `<C-w>d` | diagnostic float |
| `gx` | open URL under cursor |

### This config

| Key | Action |
| --- | --- |
| `q` / `Q` | close buffer / force close |
| `<leader>v` / `<leader>q` | vertical split / close window |
| `<leader><Space>` | recent files |
| `<leader>f` / `<leader>b` / `<leader>/` | files / buffers / grep |
| `<leader>s/` | grep, restricted to the current file's extension |
| `<leader>'` | resume last picker |
| `<C-n>` or `<leader>ff` | file explorer |
| `<leader>s…` | search: `h` help, `k` keymaps, `c` commands, `w` word under cursor, `t` TODOs, `s` spelling |
| `gd` / `<leader>D` / `<leader>r` / `<leader>a` / `<leader>e` | definition, type definition, rename, code action, line diagnostics |
| `<leader>ds` / `<leader>ws` / `<leader>wd` | document symbols / workspace symbols / workspace diagnostics |
| `<leader>dd` / `<leader>we` | current-buffer diagnostics / errors only |
| `[c` `]c` / `<leader>ph` | previous/next hunk, preview hunk |
| `<leader>g…` | git: `s` stage hunk, `r` reset hunk, `d` diff, `b` blame line, `l` blame file, `f` git files, `c` commits, `B` open line on GitHub |
| `<leader>t…` | toggle: `b` light/dark, `p` prose, `z` zen, `s` spell, `w` wrap, `f` format-on-save, `h` inlay hints, `m` markdown rendering |
| `<F10>` | run tests for the current filetype |
| `<Tab>` / `<S-Tab>` / `<CR>` / `<C-Space>` | completion menu (insert mode) |

The old lazy.nvim config is kept in `.backup/` — delete it once you're happy.

## Driving the picker

Inside any picker, the query is not just a substring — `:help MiniPick-overview`:

| Query | Meaning |
| --- | --- |
| `foo` | fuzzy (default) |
| `'foo` | exact substring |
| `^foo` | exact, anchored to the start of the line |
| `foo$` | exact, anchored to the end |
| `*foo` | force fuzzy |
| `foo bar` | both, in any order |

So in the diagnostics picker (rows look like `E │ path │ message`) typing `^E`
leaves only errors, and in the files picker `.go$` leaves only Go files.

Live grep (`<leader>/`) has two extra mappings of its own:

| Key | Action |
| --- | --- |
| `<C-o>` | add a glob, e.g. `*.go` or `!*_test.go` — stacks, shown in the border |
| `<C-e>` | toggle regex ↔ plain-text matching |

And every picker understands:

| Key | Action |
| --- | --- |
| `<C-Space>` | refine: fuzzy-filter the current results instead of re-querying |
| `<Tab>` / `<S-Tab>` | toggle preview / toggle info |
| `<C-x>` / `<C-a>` / `<M-CR>` | mark item / mark all / send marked to quickfix |
| `<C-s>` / `<C-v>` / `<C-t>` | open in split / vsplit / tab |
