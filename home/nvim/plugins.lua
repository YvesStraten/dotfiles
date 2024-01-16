local colorscheme = "kanagawa"

require("lazy").setup({
	{
		"rebelot/kanagawa.nvim",
		priority = 1000,
		config = function()
			require("kanagawa").setup({
				compile = true,
				transparent = true,
				theme = "dragon",
			})
			vim.opt.background = "dark"
			vim.cmd("colorscheme " .. colorscheme)
		end,
	},

	{
		"ThePrimeagen/harpoon",
		branch = "harpoon2",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},

	{
		"rcarriga/nvim-notify",
		lazy = false,
		config = function()
			local notify = require("notify")
			vim.notify = notify
			require("notify").setup({
				render = "compact",
			})
		end,
	},

	{
		"jakemason/ouroboros",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		keys = {
			{
				"<C-o>",
				":Ouroboros<cr>",
				desc = "Change to h or cpp",
			},
		},
		ft = {
			"c",
			"cpp",
			"h",
			"hpp",
		},
		config = function() end,
	},

	{
		"mbbill/undotree",
		keys = {
			{
				"<leader>u",
				":UndotreeShow<CR>",
				desc = "Undotree",
				true,
			},
		},
		event = "VeryLazy",
		config = function() end,
	},

	-- {
	-- 	"VonHeikemen/fine-cmdline.nvim",
	-- 	event = "VeryLazy",
	-- 	keys = {
	-- 		{
	-- 			":",
	-- 			"<cmd>FineCmdline<cr>",
	-- 			desc = "Vim cmd",
	-- 		},
	-- 	},
	-- 	dependencies = {
	-- 		"MunifTanjim/nui.nvim",
	-- 	},
	-- 	config = function() end,
	-- },

	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
		config = function()
			require("configs.which-key.which-key")
		end,
	},

	{
		"akinsho/toggleterm.nvim",
		config = function()
			require("toggleterm").setup({
				size = 20,
				open_mapping = [[<c-e>]],
				hide_numbers = true,
				shade_filetypes = {},
				shade_terminals = true,
				shading_factor = 2,
				start_in_insert = true,
				insert_mappings = true,
				persist_size = true,
				direction = "float",
				close_on_exit = false,
				shell = vim.o.shell,
				float_opts = {
					border = "curved",
					winblend = 0,
					highlights = {
						border = "Normal",
						background = "Normal",
					},
				},
			})
		end,
	},

	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {},
	},

	{
		"numToStr/Comment.nvim",
		event = "InsertEnter",
		opts = {},
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
				end,
			},
		},
		opts = {},
	},

	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{
				"nvim-lua/lsp-status.nvim",
				config = function() end,
			},

			{
				"nvimdev/lspsaga.nvim",
				config = function() end,
				dependencies = {
					"nvim-treesitter/nvim-treesitter", -- optional
					"nvim-tree/nvim-web-devicons", -- optional
				},
			},

			{
				"nvimtools/none-ls.nvim",
				dependencies = {
					"nvim-lua/plenary.nvim",
				},
				config = function()
					require("configs.lsp.none_ls")
				end,
			},
		},
		event = "BufEnter",
		config = function()
			require("configs.lsp.lspconfig")
		end,
	},

	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		dependencies = {
			"neovim/nvim-lspconfig",
			"onsails/lspkind.nvim",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-nvim-lua",
			-- "L3MON4D3/Luasnip",
			"saadparwaiz1/cmp_luasnip",
			"quangnguyen30192/cmp-nvim-ultisnips",
			{
				"SirVer/ultisnips",
				dependencies = {
					"YvesStraten/vim-snippets",
					config = function() end,
				},
				config = function() end,
			},
		},
		config = function()
			require("configs.cmp.cmp")
		end,
	},

	-- {
	-- 	"L3MON4D3/Luasnip",
	-- 	dependencies = {
	-- 		"rafamadriz/friendly-snippets",
	-- 	},
	-- 	config = function()
	-- 		require("luasnip.loaders.from_vscode").lazy_load()
	-- 	end,
	-- },

	{
		"nvim-neo-tree/neo-tree.nvim",
		event = "VeryLazy",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		},
		keys = {
			{
				"<C-n>",
				":Neotree toggle reveal=true<cr>",
				desc = "File explorer",
				true,
			},
		},
		opts = {
			close_if_last_window = true,
			window = {
				width = 20,
			},
			buffers = {
				follow_current_file = {
					enabled = true,
					leave_dirs_open = true,
				},
			},
		},
	},

	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")
			configs.setup({
				ensure_installed = {
					"c",
					"cpp",
					"lua",
					"cpp",
					"javascript",
					"typescript",
				},
				ignore_install = {
					"latex",
				},
				highlight = { enable = true },
				indent = { enable = true },
			})
		end,
	},

	{
		"romgrk/barbar.nvim",
		event = "BufEnter",
		dependencies = {
			"lewis6991/gitsigns.nvim",
			"nvim-tree/nvim-web-devicons",
		},
		keys = {
			{
				"<tab>",
				":BufferNext<cr>",
				desc = "Next Buffer",
				true,
			},
			{
				"<S-tab>",
				":BufferPrevious<cr>",
				desc = "Previous Buffer",
				true,
			},
		},
		init = function()
			vim.g.barbar_auto_setup = false
		end,
		opts = {},
	},

	{
		"christoomey/vim-tmux-navigator",
		event = "VeryLazy",
		config = function() end,
	},

	{
		"kristijanhusak/vim-dadbod-ui",
		cmd = "DBUI",
		dependencies = {
			"tpope/vim-dadbod",
		},
		config = function() end,
	},

	{
		"lervag/vimtex",
		keys = {
			{
				"<leader>tl",
				"<cmd>VimtexCompile<cr>",
				desc = "Compile tex",
				true,
			},
			{
				"<C-c>",
				":VimtexTocToggle<cr>",
				desc = "Toggle toc for latex",
				true,
			},
		},
		tag = "v1.6",
		dependencies = {
			"KeitaNakamura/tex-conceal.vim",
			config = function() end,
		},
		ft = "tex",
		config = function()
			require("configs.vimtex.vimtex")
		end,
	},

	{
		"nvim-orgmode/orgmode",
		event = "VeryLazy",
		config = function()
			require("configs.orgmode.orgmode")
		end,
	},

	{
		"tpope/vim-fugitive",
		event = "VeryLazy",
		config = function() end,
	},

	{
		"dkarter/bullets.vim",
		ft = {
			"markdown",
			"org",
		},
		config = function() end,
	},

	{
		"lukas-reineke/indent-blankline.nvim",
		event = "BufEnter",
		config = function()
			require("configs.indent-blankline.indent-blankline")
		end,
	},

	{
		"norcalli/nvim-colorizer.lua",
		event = "BufEnter",
		config = function()
			require("colorizer").setup()
		end,
	},

	{
		"kylechui/nvim-surround",
		event = "VeryLazy",
		config = function()
			require("nvim-surround").setup()
		end,
	},

	{
		"nvim-lualine/lualine.nvim",
		event = "VeryLazy",
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			require("configs.lualine")
		end,
	},

	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = "cd app && yarn install",
		keys = {
			{
				"<leader>tm",
				"<cmd>MarkdownPreview<cr>",
				"Preview markdown",
				true,
			},
		},
		config = function() end,
	},
}, {
	install = {
		colorscheme = { colorscheme },
	},
	lazy = true,
})
