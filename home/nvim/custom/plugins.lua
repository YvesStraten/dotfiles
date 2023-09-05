local plugins = {
	{
		"nvim-treesitter/nvim-treesitter",
		opts = {
			ensure_installed = {
				"c",
				"org",
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
		"neovim/nvim-lspconfig",
		dependencies = {
			{
				"jose-elias-alvarez/null-ls.nvim",
				opts = function()
					return require("custom.configs.null-ls")
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
			require("core.utils").load_mappings("vimtex")
		end,
	},

	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = {
			"cd app && npm install",
		},
		config = function()
			require("core.utils").load_mappings("markdownpreview")
		end,
	},

	{
		"sindrets/diffview.nvim",
		event = "VeryLazy",
		config = function() end,
	},

	{
		"mfussenegger/nvim-dap",
		config = function(_, _)
			require("core.utils").load_mappings("dap")
		end,
	},

	{
		"rcarriga/nvim-dap-ui",
		event = "VeryLazy",
		dependencies = "mfussenegger/nvim-dap",
		config = function()
			require("custom.configs.nvim-dap")
		end,
	},

	{
		"jay-babu/mason-nvim-dap.nvim",
		event = "VeryLazy",
		dependencies = {
			"williamboman/mason.nvim",
			"mfussenegger/nvim-dap",
		},
		opts = {
			handlers = {},
		},
	},

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
		"rcarriga/nvim-notify",
		enabled = false,
		event = "VeryLazy",
		config = function()
			vim.notify = require("notify")
		end,
	},

	{
		"nvim-orgmode/orgmode",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
		},
		ft = "org",
		config = function()
			require("custom.configs.orgmode")
		end,
	},

	{
		"jakemason/ouroboros.nvim",
		ft = {
			"cpp",
			"h",
			"hpp",
			"c",
		},
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		config = function()
			require("core.utils").load_mappings("ouroboros")
		end,
	},

	{
		"andweeb/presence.nvim",
		event = "VeryLazy",
		config = function()
			require("presence").setup()
		end,
	},

	{
		"NeogitOrg/neogit",
		cmd = "Neogit",
		dependencies = {
			"nvim-lua/plenary.nvim", -- required
			"nvim-telescope/telescope.nvim", -- optional
			"sindrets/diffview.nvim", -- optional
			"ibhagwan/fzf-lua",
		},
		config = true,
	},

	{
		"tpope/vim-dadbod",
		cmd = "DB",
		config = true,
	},

	{
		"nvim-telescope/telescope-project.nvim",
		dependencies = {
			"nvim-telescope/telescope-file-browser.nvim",
			"nvim-telescope/telescope.nvim",
		},
		config = function()
			require("telescope").load_extension("project")
		end,
	},

	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			{
				-- snippet plugin
				"L3MON4D3/LuaSnip",
				config = function(_, opts)
					require("plugins.configs.others").luasnip(opts) -- from default luasnip conf

					local luasnip = require("luasnip")

					luasnip.filetype_extend("javascriptreact", { "html" })
					luasnip.filetype_extend("typescriptreact", { "html" })
					require("luasnip/loaders/from_vscode").lazy_load() -- from default luasnip conf
				end,
			},
		},
	},
}
return plugins
