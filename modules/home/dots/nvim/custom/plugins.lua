local plugins = {
	{
		"nvim-treesitter/nvim-treesitter",
		opts = {
			ensure_installed = {
				"c",
				"cpp",
				"markdown",
				"nix",
        "html",
        "css",
        "javascript",
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

	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = {
			"cd app && npm install",
		},
		config = function() end,
	},

	{
		"sindrets/diffview.nvim",
		event = "VeryLazy",
		config = function() end,
	},

	{
		"tpope/vim-fugitive",
		event = "InsertEnter",
		config = function() end,
	},

	--[[ {
		"rcarriga/nvim-dap-ui",
		event = "InsertEnter",
		dependencies = {
			"mfussenegger/nvim-dap",
			"folke/neodev.nvim",
		},
		config = function()
			require("custom.configs.nvim-dap")
		end,
	}, ]]

	{
		"andrewferrier/wrapping.nvim",
		event = "VeryLazy",
		config = function()
			require("wrapping").setup()
		end,
	},

	{
		"jbyuki/instant.nvim",
		event = "VeryLazy",
		config = function()
      vim.g.instant_username = "yvess"
    end,
	},

	{
		"rhysd/vim-grammarous",
		ft = "tex",
		config = function() end,
	},
}
return plugins
