-- ~/.config/nvim/init.lua
--
-- Requires Neovim >= 0.12 (uses the built-in `vim.pack` plugin manager).
-- Everything that Neovim can do on its own is left to Neovim: LSP, completion,
-- diagnostics, snippets, comments, `[`/`]` motions, editorconfig, plugin
-- management. Plugins only fill the remaining gaps.
--
-- Layout:
--   init.lua              -- this file
--   lua/config/*.lua      -- the actual config, one topic per file
--   lsp/*.lua             -- per-server LSP overrides (native Neovim format)

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

require('config.options')
require('config.plugins')
require('config.theme')
require('config.statusline')
require('config.lsp')
require('config.keymaps')
require('config.autocmds')
