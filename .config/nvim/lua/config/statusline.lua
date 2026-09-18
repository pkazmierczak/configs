-- A small hand-rolled statusline. No plugin, nothing to break on updates.
--
--  NORMAL │  main │ lua/config/lsp.lua [+]        E1 W2 │ lua │ 14:32 │ 88:12
--
-- `laststatus = 3` (see config/options.lua) means there is exactly one of these
-- for the whole UI, so it always describes the current window.

local M = {}

local modes = {
  n = 'NORMAL',
  no = 'OP-PEND',
  v = 'VISUAL',
  V = 'V-LINE',
  ['\22'] = 'V-BLOCK',
  s = 'SELECT',
  S = 'S-LINE',
  ['\19'] = 'S-BLOCK',
  i = 'INSERT',
  R = 'REPLACE',
  c = 'COMMAND',
  r = 'PROMPT',
  ['!'] = 'SHELL',
  t = 'TERMINAL',
}

local sep = '%#StlDim# │ %#StatusLine#'

local function branch()
  local head = vim.b.gitsigns_head or vim.g.gitsigns_head
  if not head or head == '' then
    return ''
  end
  return '%#StlDim#  ' .. head .. sep
end

local function diagnostics()
  local count = vim.diagnostic.count(0)
  local out = {}
  local severities = {
    { vim.diagnostic.severity.ERROR, 'StlError', 'E' },
    { vim.diagnostic.severity.WARN, 'StlWarn', 'W' },
  }
  for _, s in ipairs(severities) do
    local n = count[s[1]]
    if n and n > 0 then
      out[#out + 1] = ('%%#%s#%s%d'):format(s[2], s[3], n)
    end
  end
  if #out == 0 then
    return ''
  end
  return table.concat(out, ' ') .. sep
end

function _G.Statusline()
  local mode = modes[vim.api.nvim_get_mode().mode] or 'NORMAL'
  local ft = vim.bo.filetype
  return table.concat({
    '%#StlMode# ',
    mode,
    sep,
    branch(),
    '%<%f%m%r',
    '%=',
    diagnostics(),
    ft ~= '' and ('%#StlDim#' .. ft .. sep) or '',
    '%#StlDim#' .. os.date('%H:%M'),
    sep,
    '%#StatusLine#%l:%v ',
  })
end

-- Derive the statusline accents from whatever colorscheme is active.
local function set_highlights()
  local sl = vim.api.nvim_get_hl(0, { name = 'StatusLine', link = false })
  local function derive(name, from, opts)
    local src = vim.api.nvim_get_hl(0, { name = from, link = false })
    vim.api.nvim_set_hl(0, name, vim.tbl_extend('force', { fg = src.fg, bg = sl.bg }, opts or {}))
  end
  derive('StlMode', 'Title', { bold = true })
  derive('StlDim', 'Comment')
  derive('StlError', 'DiagnosticError')
  derive('StlWarn', 'DiagnosticWarn')
end

vim.api.nvim_create_autocmd('ColorScheme', {
  group = vim.api.nvim_create_augroup('statusline-colors', { clear = true }),
  callback = set_highlights,
})
set_highlights()

-- Keep the clock honest.
if not M._timer then
  M._timer = assert(vim.uv.new_timer())
  M._timer:start(
    1000,
    10000,
    vim.schedule_wrap(function()
      pcall(vim.api.nvim__redraw, { statusline = true })
    end)
  )
end

vim.o.statusline = '%!v:lua.Statusline()'

return M
