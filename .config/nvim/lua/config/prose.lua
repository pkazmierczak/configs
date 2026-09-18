-- Prose mode: turn a code editor into a text editor.
--
-- Soft wrapping that breaks at word boundaries, spell checking, no line
-- numbers or listchars, concealed markdown syntax. Toggle with `<leader>tp`
-- or `:Prose`. Combine with `<leader>tz` (zen mode) for the full experience.

local M = {}

local opts = {
  wrap = true,
  linebreak = true,
  breakindent = true,
  spell = true,
  number = false,
  relativenumber = false,
  list = false,
  cursorline = false,
  conceallevel = 2,
  signcolumn = 'no',
}

function M.on()
  if vim.w.prose_saved then
    return
  end
  local saved = {}
  for name, value in pairs(opts) do
    saved[name] = vim.wo[name]
    vim.wo[name] = value
  end
  vim.w.prose_saved = saved
end

function M.off()
  local saved = vim.w.prose_saved
  if not saved then
    return
  end
  for name, value in pairs(saved) do
    vim.wo[name] = value
  end
  vim.w.prose_saved = nil
end

function M.toggle()
  if vim.w.prose_saved then
    M.off()
  else
    M.on()
  end
end

vim.api.nvim_create_user_command('Prose', M.toggle, { desc = 'Toggle prose mode' })

return M
