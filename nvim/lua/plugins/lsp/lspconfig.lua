local status, nvim_lsp = pcall(require, "lspconfig")
if not status then
	return
end

local protocol = require("vim.lsp.protocol")

local on_attach = function(client, bufnr)
	if client.server_capabilities.documentFormattingProvider then
  	  vim.api.nvim_create_autocmd("BufWritePre", {
		  group = vim.api.nvim_create_augroup( "Format", { clear = true 
}),       buffer = bufnr,
		  callback = function() vim.lsp.buf.formatting_seq_sync() end
		})
	end
end

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
  filetypes = { "tex"},
  capabilities = capabilities
}

nvim_lsp.lua_ls.setup {
  on_attach = on_attach,
  filetypes = { "lua"},
  capabilities = capabilities
}

nvim_lsp.clangd.setup{
  on_attach = on_attach,
  filetypes = { "cpp", "h"},
  capabilities = capabilities
}
