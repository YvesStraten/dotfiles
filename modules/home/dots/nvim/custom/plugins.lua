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
				"python",
				"lua",
				"gitcommit",
				"json",
			},
		},
	},

	{
		"williamboman/mason.nvim",
		opts = {
			ensure_installed = {
				"html-lsp",
				"lua-language-server",
				"clangd",
				"texlab",
				"pyright",
				"rnix-lsp",
				"prettierd",
				"typescript-language-server",

				"stylua",
				"htmlbeautifier",
				"shellcheck",
			},
		},
	},

	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{
				"jose-elias-alvarez/null-ls.nvim",
				config = function()
					require("custom.configs.null-ls")
				end,
			},

			{
				"glepnir/lspsaga.nvim",
				dependencies = {
					"nvim-tree/nvim-web-devicons",
					"nvim-treesitter/nvim-treesitter",
				},
				config = function()
					require("lspsaga").setup({})
				end,
			},
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
		"folke/zen-mode.nvim",
		event = "BufEnter",
		config = function() end,
	},

	{
		"ellisonleao/glow.nvim",
		ft = "markdown",
		config = function() end,
	},

	{
		"stevearc/oil.nvim",
		event = "VeryLazy",
		config = function()
			require("oil").setup()
		end,
	},

	{
		"turbio/bracey.vim",
		ft = {
			"html",
			"css",
			"javascript",
		},
		build = "npm install --prefix server",
		config = function() end,
	},

	{
		"NvChad/nvterm",
		enabled = false,
	},
}
return plugins
