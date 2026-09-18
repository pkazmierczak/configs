-- Plugins.
--
-- Managed by `vim.pack`, Neovim's built-in plugin manager (0.12+).
--   :lua vim.pack.update()        -- update everything, review diff, `:w` to confirm
--   :lua vim.pack.update({'foo'}) -- update one plugin
--   :lua vim.pack.del({'foo'})    -- remove from disk after deleting its spec below
-- State is tracked in `~/.config/nvim/nvim-pack-lock.json`; commit it.

vim.pack.add({
  -- Colorscheme (lush is zenbones' only dependency).
  { src = 'https://github.com/rktjmp/lush.nvim' },
  { src = 'https://github.com/mcchrish/zenbones.nvim' },

  -- The mini.nvim family. Used here for: picker, file explorer, icons,
  -- TODO/FIXME highlighting, surround. One repo, many small modules.
  { src = 'https://github.com/echasnovski/mini.nvim', version = 'stable' },

  -- Git signs in the gutter, hunk actions and inline blame.
  { src = 'https://github.com/lewis6991/gitsigns.nvim' },

  -- Language server *definitions* only. The LSP client is built into Neovim.
  { src = 'https://github.com/neovim/nvim-lspconfig' },

  -- Treesitter parsers (highlighting/folding itself is built into Neovim).
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter', version = 'main' },

  -- Popup with available keybindings.
  { src = 'https://github.com/folke/which-key.nvim' },

  -- Prose: in-buffer markdown rendering + distraction-free mode.
  { src = 'https://github.com/MeanderingProgrammer/render-markdown.nvim' },
  { src = 'https://github.com/folke/zen-mode.nvim' },

  -- TLA+ syntax.
  { src = 'https://github.com/florentc/vim-tla' },
}, { confirm = false })

-- ---------------------------------------------------------------------------
-- mini.nvim
-- ---------------------------------------------------------------------------

require('mini.icons').setup()
MiniIcons.mock_nvim_web_devicons()

-- Every picker opens as the same centered float, whatever the terminal size.
local function centered_float()
  local height = math.min(25, math.floor(0.7 * vim.o.lines))
  local width = math.min(120, math.floor(0.8 * vim.o.columns))
  return {
    relative = 'editor',
    anchor = 'NW',
    height = height,
    width = width,
    row = math.floor(0.5 * (vim.o.lines - height)),
    col = math.floor(0.5 * (vim.o.columns - width)),
    border = 'rounded',
  }
end

require('mini.pick').setup({
  options = { use_cache = true },
  window = { config = centered_float },
})
vim.ui.select = MiniPick.ui_select

require('mini.extra').setup()
require('mini.surround').setup()
require('mini.files').setup({
  windows = { preview = true, width_focus = 30, width_preview = 60 },
  options = { use_as_default_explorer = true },
})

-- ...and so does the file explorer, which otherwise hugs the top-left corner.
vim.api.nvim_create_autocmd('User', {
  group = vim.api.nvim_create_augroup('mini-files-center', { clear = true }),
  pattern = { 'MiniFilesWindowOpen', 'MiniFilesWindowUpdate' },
  callback = function(args)
    local win = args.data.win_id
    local cfg = vim.api.nvim_win_get_config(win)
    cfg.border = 'rounded'
    cfg.height = math.min(25, math.floor(0.7 * vim.o.lines))
    cfg.row = math.floor(0.5 * (vim.o.lines - cfg.height))
    vim.api.nvim_win_set_config(win, cfg)
  end,
})

-- TODO / FIXME / HACK / NOTE / WARN highlighting, plus inline #rrggbb swatches.
local hipatterns = require('mini.hipatterns')
local function word(w)
  return '%f[%w]()' .. w .. '()%f[%W]'
end
hipatterns.setup({
  highlighters = {
    fixme = { pattern = word('FIXME'), group = 'MiniHipatternsFixme' },
    hack = { pattern = word('HACK'), group = 'MiniHipatternsHack' },
    todo = { pattern = word('TODO'), group = 'MiniHipatternsTodo' },
    note = { pattern = word('NOTE'), group = 'MiniHipatternsNote' },
    warn = { pattern = word('WARN'), group = 'MiniHipatternsFixme' },
    xxx = { pattern = word('XXX'), group = 'MiniHipatternsFixme' },
    hex_color = hipatterns.gen_highlighter.hex_color(),
  },
})

-- ---------------------------------------------------------------------------
-- Git
-- ---------------------------------------------------------------------------

require('gitsigns').setup({
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
    untracked = { text = '┆' },
  },
  current_line_blame = true,
  current_line_blame_opts = { delay = 400, virt_text_pos = 'eol', ignore_whitespace = true },
  current_line_blame_formatter = '   <author>, <author_time:%R> · <summary>',
  preview_config = { border = 'rounded' },
})

-- ---------------------------------------------------------------------------
-- Treesitter
-- ---------------------------------------------------------------------------

local parsers = {
  'bash', 'c', 'css', 'diff', 'dockerfile', 'git_config', 'git_rebase',
  'gitcommit', 'gitignore', 'go', 'gomod', 'gosum', 'gowork', 'hcl', 'html',
  'javascript', 'json', 'lua', 'luadoc', 'make', 'markdown',
  'markdown_inline', 'python', 'query', 'regex', 'rust', 'sql', 'terraform',
  'toml', 'tsx', 'typescript', 'vim', 'vimdoc', 'yaml',
}

pcall(function()
  local installed = require('nvim-treesitter.config').get_installed('parsers')
  local missing = vim.tbl_filter(function(lang)
    return not vim.tbl_contains(installed, lang)
  end, parsers)
  if #missing > 0 then
    require('nvim-treesitter').install(missing)
  end
end)

-- Start treesitter highlighting for any filetype that has a parser available.
vim.api.nvim_create_autocmd('FileType', {
  group = vim.api.nvim_create_augroup('treesitter-start', { clear = true }),
  callback = function(ev)
    local lang = vim.treesitter.language.get_lang(ev.match)
    if lang and pcall(vim.treesitter.language.add, lang) then
      pcall(vim.treesitter.start, ev.buf, lang)
    end
  end,
})

-- ---------------------------------------------------------------------------
-- which-key
-- ---------------------------------------------------------------------------

local wk = require('which-key')
wk.setup({ preset = 'helix', delay = 400 })
wk.add({
  { '<leader>g', group = 'git' },
  { '<leader>s', group = 'search' },
  { '<leader>t', group = 'toggle' },
  { '<leader>d', group = 'document' },
  { '<leader>w', group = 'workspace' },
})

-- ---------------------------------------------------------------------------
-- Prose
-- ---------------------------------------------------------------------------

require('render-markdown').setup({
  completions = { lsp = { enabled = true } },
  heading = { icons = { '# ', '## ', '### ', '#### ', '##### ', '###### ' } },
  code = { style = 'normal', width = 'block', right_pad = 2 },
})

require('zen-mode').setup({
  window = { width = 82, options = { number = false, signcolumn = 'no' } },
  plugins = { options = { laststatus = 0 } },
})
