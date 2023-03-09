local status, nvim_lsp = pcall(require, "lspconfig")
if not status then
  return
end

local protocol = require("vim.lsp.protocol")

-- Set up completion using nvim_cmp with LSP source
local capabilities = require("cmp_nvim_lsp").default_capabilities(
  vim.lsp.protocol.make_client_capabilities()
)

nvim_lsp.flow.setup {
  on_attach = on_attach,
  capabilities = capabilities
}

nvim_lsp.texlab.setup {
  on_attach = on_attach,
  filetypes = { "tex", "bib", "plaintex" },
  cmd = { "texlab" },
  capabilities = capabilities,
}

nvim_lsp.lua_ls.setup {
  on_attach = on_attach,
  filetypes = { "lua" },
  settings = {
    Lua = {
      diagnostics = {
        -- vim global
        globals = { "vim" }
      },
      workspace = {
        --Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false
      },
    },
  },
}

nvim_lsp.clangd.setup {
  on_attach = on_attach,
  filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto", "h" },
  cmd = { "clangd" },
  capabilities = capabilities
}

vim.o.updatetime = 250
vim.cmd [[
autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})
]]
