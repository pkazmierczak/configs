-- Overrides on top of nvim-lspconfig's `lsp/rust_analyzer.lua`.
-- Install: rustup component add rust-analyzer clippy
return {
  settings = {
    ['rust-analyzer'] = {
      cargo = { features = 'all' },
      check = { command = 'clippy' },
      checkOnSave = true,
      procMacro = { enable = true },
      inlayHints = {
        closureReturnTypeHints = { enable = 'with_block' },
        parameterHints = { enable = true },
        typeHints = { enable = true },
      },
    },
  },
}
