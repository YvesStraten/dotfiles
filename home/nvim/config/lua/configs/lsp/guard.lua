local ft = require("guard.filetype")

ft("typescript,javascript,typescriptreact"):fmt("prettier")
ft("nix"):fmt("nixfmt")
ft("lua"):fmt("stylua")
ft("sh"):lint("shellcheck")

require("guard").setup({
	fmt_on_save = true,
	lsp_as_default_formatter = true,
})
