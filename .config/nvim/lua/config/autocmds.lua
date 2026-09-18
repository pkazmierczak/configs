-- Autocommands.

local function augroup(name)
  return vim.api.nvim_create_augroup('config.' .. name, { clear = true })
end

-- Briefly highlight whatever was just yanked.
vim.api.nvim_create_autocmd('TextYankPost', {
  group = augroup('yank'),
  callback = function()
    vim.hl.on_yank()
  end,
})

-- Reopen a file where you left it.
vim.api.nvim_create_autocmd('BufReadPost', {
  group = augroup('last-position'),
  callback = function(ev)
    if vim.bo[ev.buf].filetype:match('^git') then
      return
    end
    local mark = vim.api.nvim_buf_get_mark(ev.buf, '"')
    if mark[1] > 0 and mark[1] <= vim.api.nvim_buf_line_count(ev.buf) then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Notice files that changed on disk (e.g. after a `git checkout`).
vim.api.nvim_create_autocmd({ 'FocusGained', 'TermClose', 'TermLeave' }, {
  group = augroup('checktime'),
  command = 'checktime',
})

-- Keep splits proportional when the terminal is resized.
vim.api.nvim_create_autocmd('VimResized', {
  group = augroup('resize'),
  command = 'tabdo wincmd =',
})

-- In throwaway windows, `q` should just close the window.
vim.api.nvim_create_autocmd('FileType', {
  group = augroup('quick-close'),
  pattern = { 'help', 'man', 'qf', 'checkhealth', 'gitsigns-blame', 'lspinfo' },
  callback = function(ev)
    vim.bo[ev.buf].buflisted = false
    vim.keymap.set('n', 'q', '<cmd>close<cr>', { buffer = ev.buf, silent = true })
  end,
})

-- Terminals are not documents.
vim.api.nvim_create_autocmd('TermOpen', {
  group = augroup('terminal'),
  callback = function()
    vim.wo.number = false
    vim.wo.relativenumber = false
    vim.wo.signcolumn = 'no'
  end,
})

-- Prose filetypes start in prose mode.
vim.api.nvim_create_autocmd('FileType', {
  group = augroup('prose'),
  pattern = { 'markdown', 'text', 'tex', 'typst', 'rst', 'gitcommit', 'mail' },
  callback = function()
    require('config.prose').on()
  end,
})
