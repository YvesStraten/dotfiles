require("lazy").setup({
	{
		"Mofiqul/dracula.nvim",
		lazy = false,
		init = function()
			vim.cmd [[colorscheme dracula]]
		end
	},

	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
		config = function()
			require("configs.which-key.which-key")
		end
	},

	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {}
	},

	{
		"numToStr/Comment.nvim",
		lazy = false,
		opts = {

		}
	},

	{
		"nvim-telescope/telescope.nvim",
		event = "VeryLazy",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{
				"nvim-telescope/telescope-project.nvim",
				config = function()
					require("telescope").load_extension("project")
				end
			},
		},
		opts = {

		}
	},

	{
		"neovim/nvim-lspconfig",
		dependencies = {
			"nvimdev/lspsaga.nvim",
			config = function()
				require("lspsaga")
			end
		},
		event = "BufEnter",
		config = function()
			require("configs.lsp.lspconfig")
		end,
	},

	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"neovim/nvim-lspconfig",
			"onsails/lspkind.nvim",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"L3MON4D3/Luasnip",
			"saadparwaiz1/cmp_luasnip",
		},
		config = function()
			require("configs.cmp.cmp")
		end,
	},

	{
		"L3MON4D3/Luasnip",
		dependencies = {
			"rafamadriz/friendly-snippets"
		},
		config = function()
			require("luasnip.loaders.from_vscode").lazy_load()
		end,
	},

	{
		"nvim-neo-tree/neo-tree.nvim",
		event = "VeryLazy",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		},
		opts = {
			close_if_last_window = true,
			window = {
				width = 25,
			},
			buffers = {
				follow_current_file = {
					enabled = true,
					leave_dirs_open = true,
				}
			}
		}
	},

	{
		"nvim-treesitter/nvim-treesitter",
		cmd = {
			"TSInstall",
			"TSUpdate"
		},
		opts = {
			ensure = {
				"c",
				"lua",
				"cpp",
			},
			sync_install = true,
			auto_install = true,
			ignore_install = {
				"latex"
			},
		}

	},

	{
		"romgrk/barbar.nvim",
		event = "BufEnter",
		dependencies = {
			"lewis6991/gitsigns.nvim",
			"nvim-tree/nvim-web-devicons",
		},
		init = function() vim.g.barbar_auto_setup = false end,
		opts = {}
	},

	{
		"christoomey/vim-tmux-navigator",
		event = "VeryLazy",
		config = function()
		end,
	},

	{
		"kristijanhusak/vim-dadbod-ui",
		cmd = "DBUI",
		dependencies = {
			"tpope/vim-dadbod",
		},
		config = function()
		end,
	},

	{
		"lervag/vimtex",
		ft = "tex",
		config = function()
			require("configs.vimtex.vimtex")
		end,
	},

	{
		"nvim-orgmode/orgmode",
		ft = "org",
		config = function()
			require("configs.orgmode.orgmode")
		end
	},

	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim",      -- required
			"nvim-telescope/telescope.nvim", -- optional
			"sindrets/diffview.nvim",     -- optional
			"ibhagwan/fzf-lua",           -- optional
		},
		config = true
	},

	{
		"dkarter/bullets.vim",
		ft = {
			"markdown",
			"org",
		},
		config = function()
		end,
	},

	{
		"lukas-reineke/indent-blankline.nvim",
		event = "BufEnter",
		config = function()
			require("configs.indent-blankline.indent-blankline")
		end
	},

	{
		"norcalli/nvim-colorizer.lua",
		event = "VeryLazy",
		opts = {}
	},

	{
		"nvim-lualine/lualine.nvim",
		event = "VeryLazy",
		dependencies = {
			"nvim-tree/nvim-web-devicons"
		},
		opts = {
			sections = {
				lualine_x = { "fileformat" }
			},
			globalstatus = true
		}
	},

	{
		"nvimdev/dashboard-nvim",
		event = "VimEnter",
		dependencies = {
			"nvim-tree/nvim-web-devicons"
		},
		config = function()
			require("configs.dashboard.dashboard")
		end
	},

	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = "cd app && yarn install",
		config = function()
		end
	}
})
