-- Keymaps. `<leader>` is <Space>.
--
-- Neovim 0.11+ already provides a lot out of the box, and none of it is
-- redefined here:
--   K / grn / gra / grr / gri / grt / gO   LSP hover, rename, code action, ...
--   gc / gcc                               comment (operator + line)
--   [d ]d  [q ]q  [b ]b  [<Space> ]<Space> jump between things
--   <C-w>d  <C-l>  y<C-g>  gx              diagnostics float, redraw, path, open
-- Run `:help default-mappings` or hit <leader> and wait for which-key.

local map = vim.keymap.set
local pick = function(fn, ...)
  local args = { ... }
  return function()
    fn(unpack(args))
  end
end

local theme = require('config.theme')
local prose = require('config.prose')
local tools = require('config.tools')

-- ---------------------------------------------------------------------------
-- Basics
-- ---------------------------------------------------------------------------

map({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Move by display line when the line wraps, unless a count was given.
map({ 'n', 'v' }, 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
map({ 'n', 'v' }, 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Buffers and windows (note: `q` no longer records macros).
map('n', 'q', '<cmd>bprevious | bdelete #<cr>', { silent = true, desc = 'Close buffer' })
map('n', 'Q', '<cmd>bdelete!<cr>', { silent = true, desc = 'Close buffer (force)' })
map('n', '<leader>v', '<cmd>vsplit<cr>', { silent = true, desc = 'Vertical split' })
map('n', '<leader>q', '<cmd>close<cr>', { silent = true, desc = 'Close window' })

-- Escape out of the terminal.
map('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Leave terminal mode' })

-- ---------------------------------------------------------------------------
-- Completion (Neovim's built-in popup menu)
-- ---------------------------------------------------------------------------

map('i', '<C-Space>', function()
  vim.lsp.completion.get()
end, { desc = 'Trigger completion' })

map('i', '<CR>', function()
  if vim.fn.pumvisible() == 0 then
    return '<CR>'
  end
  return vim.fn.complete_info({ 'selected' }).selected ~= -1 and '<C-y>' or '<C-e><CR>'
end, { expr = true, desc = 'Accept completion / newline' })

map('i', '<Tab>', function()
  if vim.fn.pumvisible() == 1 then
    return '<C-n>'
  end
  if vim.snippet.active({ direction = 1 }) then
    return '<cmd>lua vim.snippet.jump(1)<cr>'
  end
  return '<Tab>'
end, { expr = true, desc = 'Next completion / snippet placeholder' })

map('i', '<S-Tab>', function()
  if vim.fn.pumvisible() == 1 then
    return '<C-p>'
  end
  if vim.snippet.active({ direction = -1 }) then
    return '<cmd>lua vim.snippet.jump(-1)<cr>'
  end
  return '<S-Tab>'
end, { expr = true, desc = 'Previous completion / snippet placeholder' })

-- ---------------------------------------------------------------------------
-- Pickers (mini.pick / mini.extra) and the file explorer (mini.files)
-- ---------------------------------------------------------------------------

map('n', '<leader><space>', pick(MiniExtra.pickers.oldfiles), { desc = 'Recent files' })
map('n', '<leader>f', pick(MiniPick.builtin.files), { desc = 'Find files' })
map('n', '<leader>b', pick(MiniPick.builtin.buffers), { desc = 'Buffers' })
map('n', '<leader>/', pick(MiniPick.builtin.grep_live), { desc = 'Grep' })
map('n', '<leader>s/', function()
  local ext = vim.fn.expand('%:e')
  MiniPick.builtin.grep_live({ globs = ext ~= '' and { '*.' .. ext } or {} })
end, { desc = 'Grep (current file type)' })
map('n', "<leader>'", pick(MiniPick.builtin.resume), { desc = 'Resume last picker' })

map('n', '<leader>sh', pick(MiniPick.builtin.help), { desc = 'Help tags' })
map('n', '<leader>sk', pick(MiniExtra.pickers.keymaps), { desc = 'Keymaps' })
map('n', '<leader>sc', pick(MiniExtra.pickers.commands), { desc = 'Commands' })
map('n', '<leader>ss', pick(MiniExtra.pickers.spellsuggest), { desc = 'Spelling suggestions' })
map('n', '<leader>sw', function()
  MiniPick.builtin.grep({ pattern = vim.fn.expand('<cword>') })
end, { desc = 'Search word under cursor' })
map('n', '<leader>st', function()
  MiniPick.builtin.grep({ pattern = [[\b(TODO|FIXME|HACK|XXX|WARN|NOTE)\b]] })
end, { desc = 'Search TODOs' })

local function explorer()
  if MiniFiles.close() then
    return
  end
  local path = vim.api.nvim_buf_get_name(0)
  MiniFiles.open(vim.uv.fs_stat(path) and path or vim.uv.cwd())
  pcall(MiniFiles.reveal_cwd)
end
map('n', '<C-n>', explorer, { desc = 'File explorer' })
map('n', '<leader>ff', explorer, { desc = 'File explorer' })

-- ---------------------------------------------------------------------------
-- LSP & diagnostics
-- ---------------------------------------------------------------------------

map('n', 'gd', vim.lsp.buf.definition, { desc = 'Goto definition' })
map('n', '<leader>D', vim.lsp.buf.type_definition, { desc = 'Type definition' })
map('n', '<leader>r', vim.lsp.buf.rename, { desc = 'Rename symbol' })
map({ 'n', 'v' }, '<leader>a', vim.lsp.buf.code_action, { desc = 'Code action' })
map('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Line diagnostics' })

-- Give the built-in `gr*` / `gO` defaults a fuzzy picker instead of a quickfix list.
map('n', 'grr', pick(MiniExtra.pickers.lsp, { scope = 'references' }), { desc = 'References' })
map('n', 'gri', pick(MiniExtra.pickers.lsp, { scope = 'implementation' }), { desc = 'Implementations' })
map('n', 'gO', pick(MiniExtra.pickers.lsp, { scope = 'document_symbol' }), { desc = 'Document symbols' })

map('n', '<leader>ds', pick(MiniExtra.pickers.lsp, { scope = 'document_symbol' }), { desc = 'Document symbols' })
map('n', '<leader>dd', pick(MiniExtra.pickers.diagnostic, { scope = 'current' }), { desc = 'Document diagnostics' })

-- `workspace_symbol_live` sends every keystroke to the server. The plain
-- `workspace_symbol` scope queries once with an empty string, which servers
-- like gopls answer with nothing at all.
map('n', '<leader>ws', pick(MiniExtra.pickers.lsp, { scope = 'workspace_symbol_live' }), { desc = 'Workspace symbols' })
map('n', '<leader>wd', pick(MiniExtra.pickers.diagnostic), { desc = 'Workspace diagnostics' })
map(
  'n',
  '<leader>we',
  pick(MiniExtra.pickers.diagnostic, { get_opts = { severity = vim.diagnostic.severity.ERROR } }),
  { desc = 'Workspace errors only' }
)

-- ---------------------------------------------------------------------------
-- Git
-- ---------------------------------------------------------------------------

local gs = require('gitsigns')

map('n', ']c', function()
  gs.nav_hunk('next')
end, { desc = 'Next hunk' })
map('n', '[c', function()
  gs.nav_hunk('prev')
end, { desc = 'Previous hunk' })

map('n', '<leader>ph', gs.preview_hunk, { desc = 'Preview hunk' })
map({ 'n', 'v' }, '<leader>gs', gs.stage_hunk, { desc = 'Stage hunk' })
map({ 'n', 'v' }, '<leader>gr', gs.reset_hunk, { desc = 'Reset hunk' })
map('n', '<leader>gd', gs.diffthis, { desc = 'Diff this file' })
map('n', '<leader>gb', function()
  gs.blame_line({ full = true })
end, { desc = 'Blame line' })
map('n', '<leader>gl', gs.blame, { desc = 'Blame file' })
map('n', '<leader>gf', pick(MiniExtra.pickers.git_files), { desc = 'Git files' })
map('n', '<leader>gc', pick(MiniExtra.pickers.git_commits), { desc = 'Git commits' })
map('n', '<leader>gB', tools.browse_line, { desc = 'Browse line on the forge' })

-- ---------------------------------------------------------------------------
-- Toggles
-- ---------------------------------------------------------------------------

map('n', '<leader>tb', theme.toggle, { desc = 'Background: light/dark' })
map('n', '<leader>tp', prose.toggle, { desc = 'Prose mode' })
map('n', '<leader>tz', '<cmd>ZenMode<cr>', { desc = 'Zen mode' })
map('n', '<leader>ts', '<cmd>setlocal spell!<cr>', { desc = 'Spell check' })
map('n', '<leader>tw', '<cmd>setlocal wrap!<cr>', { desc = 'Soft wrap' })
map('n', '<leader>tf', '<cmd>AutoformatToggle<cr>', { desc = 'Format on save' })
map('n', '<leader>th', function()
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = 0 }), { bufnr = 0 })
end, { desc = 'Inlay hints' })
map('n', '<leader>tm', '<cmd>RenderMarkdown toggle<cr>', { desc = 'Markdown rendering' })

-- ---------------------------------------------------------------------------
-- Run
-- ---------------------------------------------------------------------------

map('n', '<F10>', tools.test, { desc = 'Run tests' })
