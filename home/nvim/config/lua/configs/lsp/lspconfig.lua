local lspconfig = require("lspconfig")
vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]

lspconfig.tsserver.setup {
}

lspconfig.lua_ls.setup {
}

lspconfig.texlab.setup {
}

lspconfig.clangd.setup {
}

lspconfig.omnisharp.setup {
	cmd = { "omnisharp" }
}

lspconfig.pyright.setup {
}
