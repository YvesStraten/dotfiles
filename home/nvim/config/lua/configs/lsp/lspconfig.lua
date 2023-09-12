local lspconfig = require("lspconfig")

lspconfig.tsserver.setup {
}

lspconfig.lua_ls.setup {
}

lspconfig.texlab.setup {
}

lspconfig.clangd.setup {
}

lspconfig.omnisharp.setup {
	cmd = { "OmniSharp" }
}

lspconfig.pyright.setup {
}
