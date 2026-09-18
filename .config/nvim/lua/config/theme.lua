-- Colorscheme: zenbones family.
--   light -> forestbones
--   dark  -> zenbones
-- Light is the default; `<leader>tb` (or `:Background`) flips it. No automatic
-- OS-based switching on purpose — it behaves differently on every platform.

local M = {}

local schemes = { light = 'forestbones', dark = 'zenbones' }

-- A slightly softer contrast than the defaults; see `:help zenbones`.
vim.g.forestbones = { darken_comments = 45, lighten_noncurrent_window = true }
vim.g.zenbones = { darken_comments = 45, lighten_noncurrent_window = true }

function M.set(background)
  vim.o.background = background
  local ok = pcall(vim.cmd.colorscheme, schemes[background])
  if not ok then
    vim.cmd.colorscheme('default')
  end
end

function M.toggle()
  M.set(vim.o.background == 'dark' and 'light' or 'dark')
end

vim.api.nvim_create_user_command('Background', function(opts)
  M.set(opts.args ~= '' and opts.args or (vim.o.background == 'dark' and 'light' or 'dark'))
end, {
  nargs = '?',
  complete = function()
    return { 'light', 'dark' }
  end,
  desc = 'Switch between the light and dark colorscheme',
})

M.set('light')

return M
