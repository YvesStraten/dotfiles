local null_ls = require("null-ls")

local formatting = null_ls.builtins.formatting
local lint = null_ls.builtins.diagnostics

local sources = {
	formatting.stylua,
	formatting.alejandra,
  formatting.clang_format,

	lint.shellcheck,
  lint.cppcheck,
}

null_ls.setup({
	debug = true,
	sources = sources,
})
