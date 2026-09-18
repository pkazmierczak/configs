-- Editor options. See `:help option-list`.

local o = vim.o

-- UI
o.number = true
o.relativenumber = false
o.cursorline = true
o.signcolumn = 'yes'
o.scrolloff = 5
o.showmode = false -- the statusline already shows it
o.laststatus = 3 -- one global statusline
o.winborder = 'rounded' -- default border for all floats (hover, diagnostics, ...)
o.termguicolors = true
o.list = true
o.listchars = 'tab:  ,trail:·,nbsp:␣,extends:›,precedes:‹'
o.splitright = true
o.splitbelow = true
o.confirm = true -- ask instead of failing on :q with unsaved changes

-- Behaviour
o.mouse = 'a'
o.clipboard = 'unnamedplus'
o.undofile = true
o.breakindent = true
o.updatetime = 250
o.timeoutlen = 300
o.jumpoptions = 'stack,view'

-- Search
o.hlsearch = false
o.ignorecase = true
o.smartcase = true
o.inccommand = 'split' -- live preview for :s

-- Indentation (Go wants real tabs; editorconfig/ftplugins override per project)
o.tabstop = 4
o.shiftwidth = 4
o.softtabstop = 4

-- Completion: Neovim's built-in popup menu, driven by the LSP client.
-- `fuzzy` + `popup` need 0.11+; `popup` shows documentation next to the menu.
o.completeopt = 'menuone,noselect,fuzzy,popup'
o.pumheight = 12
o.wildmode = 'longest:full,full'

-- Folds come from treesitter, but start fully open.
o.foldmethod = 'expr'
o.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
o.foldtext = ''
o.foldlevelstart = 99
o.fillchars = 'fold: ,foldopen:▾,foldclose:▸,foldsep: ,eob: '

-- Spelling is off by default but ready for prose (see config/prose.lua).
o.spelllang = 'en_us'

-- Disable unused built-in providers (speeds up startup and quiets :checkhealth).
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_python3_provider = 0
