local lsp_zero = require("lsp-zero")
local lspconfig = require("lspconfig")

lsp_zero.on_attach(function(client, bufnr)
	lsp_zero.default_keymaps({
		buffer = bufnr,
	})
end)

lspconfig.lua_ls.setup({})
