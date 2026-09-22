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

-- ---------------------------------------------------------------------------
-- LSP progress ("indexing...", "loading workspace...", etc.)
-- ---------------------------------------------------------------------------

local spinner_frames = { '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏' }
local spinner_frame = 1
local progress = {} -- token -> { client, title, message, percentage }

vim.api.nvim_create_autocmd('LspProgress', {
  group = vim.api.nvim_create_augroup('lsp-progress', { clear = true }),
  callback = function(ev)
    local value = ev.data.params.value
    local token = ev.data.client_id .. ':' .. ev.data.params.token
    if value.kind == 'end' then
      progress[token] = nil
    else
      local client = vim.lsp.get_client_by_id(ev.data.client_id)
      progress[token] = {
        client = client and client.name or '?',
        title = value.title,
        message = value.message,
        percentage = value.percentage,
      }
    end
  end,
})

local function lsp_progress()
  local token, p = next(progress)
  if not token then
    return ''
  end
  local text = p.title or ''
  if p.message then
    text = text .. ' ' .. p.message
  end
  if p.percentage then
    text = text .. ' (' .. p.percentage .. '%%)'
  end
  return ('%%#StlDim#%s %s: %s'):format(spinner_frames[spinner_frame], p.client, text) .. sep
end

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
    lsp_progress(),
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

-- Animate the LSP progress spinner, but only redraw while something is
-- actually running so this stays a no-op the rest of the time.
if not M._spinner_timer then
  M._spinner_timer = assert(vim.uv.new_timer())
  M._spinner_timer:start(
    80,
    80,
    vim.schedule_wrap(function()
      if next(progress) == nil then
        return
      end
      spinner_frame = spinner_frame % #spinner_frames + 1
      pcall(vim.api.nvim__redraw, { statusline = true })
    end)
  )
end

vim.o.statusline = '%!v:lua.Statusline()'

return M
