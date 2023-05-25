local plugins = {
	{
		"nvim-treesitter/nvim-treesitter",
		opts = {
			ensure_installed = {
				"c",
				"cpp",
        "markdown",
			},
		},
	},

	{
		"neovim/nvim-lspconfig",
		dependencies = {
			"jose-elias-alvarez/null-ls.nvim",
			config = function()
				require("custom.configs.null-ls")
			end,
		},

		config = function()
			require("plugins.configs.lspconfig")
			require("custom.configs.lspconfig")
		end,
	},

	{
		"williamboman/mason.nvim",
		opts = {
			ensure_installed = {
				"lua-language-server",
				"html-lsp",
				"stylua",
				"texlab",
			},
		},
	},

	{
		"lervag/vimtex",
		ft = "tex",
		config = function()
			require("custom.configs.vimtex")
		end,
	},

	{
		"quarto-dev/quarto-nvim",
    lazy = false,
    dependencies = {
      "jmbuhr/otter.nvim",
    },
		config = function()
			require("quarto").setup({})
		end,
	},
}
return plugins
