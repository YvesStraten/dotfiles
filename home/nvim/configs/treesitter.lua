require("nvim-treesitter.configs").setup({
	ensure_installed = {},
	ignore_install = {
		"latex",
	},

	auto_install = false,

	highlight = { enable = true },

	indent = { enable = true },
})
