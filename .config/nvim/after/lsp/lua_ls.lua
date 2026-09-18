-- Overrides on top of nvim-lspconfig's `lsp/lua_ls.lua`.
-- Install: brew install lua-language-server
--
-- The `on_init` hook replaces what neodev/lazydev used to do: when the project
-- looks like a Neovim config, point the server at Neovim's own Lua runtime.
return {
  on_init = function(client)
    local root = client.workspace_folders and client.workspace_folders[1]
    if root and (vim.uv.fs_stat(root.name .. '/.luarc.json') or vim.uv.fs_stat(root.name .. '/.luarc.jsonc')) then
      return
    end

    client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua or {}, {
      runtime = { version = 'LuaJIT', path = { 'lua/?.lua', 'lua/?/init.lua' } },
      workspace = {
        checkThirdParty = false,
        library = { vim.env.VIMRUNTIME, '${3rd}/luv/library' },
      },
    })
  end,
  settings = {
    Lua = {
      diagnostics = { globals = { 'vim', 'MiniPick', 'MiniFiles', 'MiniIcons', 'MiniExtra' } },
      telemetry = { enable = false },
      hint = { enable = true },
      format = { enable = false },
    },
  },
}
