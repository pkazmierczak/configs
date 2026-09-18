-- LSP. Neovim 0.11+ ships the client, the completion engine, snippet expansion
-- and sensible `gr*` keymaps, so this file only has to say *which* servers to
-- run and what should happen when one attaches.
--
-- Base server definitions come from nvim-lspconfig; per-server overrides live
-- in `after/lsp/<name>.lua` (see `:help lsp-config-merge`).
--
-- Adding a server = add its name below (and install its binary).
-- Check what is running with `:checkhealth vim.lsp`.

vim.lsp.enable({
  'gopls',
  'rust_analyzer',
  'lua_ls',
  'terraformls',
})

-- ---------------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------------

local severity = vim.diagnostic.severity

vim.diagnostic.config({
  severity_sort = true,
  update_in_insert = false,
  -- Full message only for the line the cursor is on; the rest stays out of the
  -- way and is visible via the sign column and the statusline counters.
  virtual_text = false,
  virtual_lines = { current_line = true },
  underline = { severity = { min = severity.WARN } },
  signs = {
    text = {
      [severity.ERROR] = 'E',
      [severity.WARN] = 'W',
      [severity.INFO] = 'I',
      [severity.HINT] = 'H',
    },
  },
  float = { source = true, header = '', prefix = '' },
})

-- ---------------------------------------------------------------------------
-- On attach
-- ---------------------------------------------------------------------------

local group = vim.api.nvim_create_augroup('lsp-attach', { clear = true })

vim.api.nvim_create_autocmd('LspAttach', {
  group = group,
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if not client then
      return
    end

    -- Built-in autocompletion — no completion plugin required.
    if client:supports_method('textDocument/completion') then
      vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
    end

    -- Dim-highlight other occurrences of the symbol under the cursor.
    if client:supports_method('textDocument/documentHighlight') then
      local hl = vim.api.nvim_create_augroup('lsp-highlight.' .. ev.buf, { clear = true })
      vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        group = hl,
        buffer = ev.buf,
        callback = vim.lsp.buf.document_highlight,
      })
      vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI', 'InsertEnter' }, {
        group = hl,
        buffer = ev.buf,
        callback = vim.lsp.buf.clear_references,
      })
      vim.api.nvim_create_autocmd('LspDetach', {
        group = group,
        buffer = ev.buf,
        callback = function(e)
          vim.lsp.buf.clear_references()
          pcall(vim.api.nvim_del_augroup_by_name, 'lsp-highlight.' .. e.buf)
        end,
      })
    end
  end,
})

-- ---------------------------------------------------------------------------
-- Format on save
-- ---------------------------------------------------------------------------

vim.g.autoformat = true

-- `source.organizeImports` is how gopls does goimports.
local function organize_imports(buf, timeout_ms)
  for _, client in ipairs(vim.lsp.get_clients({ bufnr = buf, method = 'textDocument/codeAction' })) do
    local params = vim.lsp.util.make_range_params(0, client.offset_encoding)
    params.context = { only = { 'source.organizeImports' }, diagnostics = {} }
    local ok, res = pcall(function()
      return client:request_sync('textDocument/codeAction', params, timeout_ms, buf)
    end)
    for _, action in pairs(ok and res and res.result or {}) do
      if action.edit then
        vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding)
      elseif action.command then
        pcall(function()
          client:exec_cmd(action.command, { bufnr = buf })
        end)
      end
    end
  end
end

vim.api.nvim_create_autocmd('BufWritePre', {
  group = vim.api.nvim_create_augroup('lsp-format-on-save', { clear = true }),
  callback = function(ev)
    if not vim.g.autoformat or vim.b[ev.buf].autoformat == false then
      return
    end
    if vim.bo[ev.buf].filetype == 'go' then
      organize_imports(ev.buf, 1500)
    end
    vim.lsp.buf.format({ bufnr = ev.buf, timeout_ms = 2000 })
  end,
})

vim.api.nvim_create_user_command('AutoformatToggle', function(opts)
  if opts.bang then
    vim.b.autoformat = vim.b.autoformat == false
    vim.notify('buffer autoformat: ' .. tostring(vim.b.autoformat ~= false))
  else
    vim.g.autoformat = not vim.g.autoformat
    vim.notify('autoformat: ' .. tostring(vim.g.autoformat))
  end
end, { bang = true, desc = 'Toggle format-on-save (! for current buffer only)' })
