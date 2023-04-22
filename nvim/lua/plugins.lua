return {

	{
		"catppuccin/nvim",
		lazy = false,
		priority = 1000,
		config = function()
			vim.cmd([[colorscheme catppuccin-mocha]])
		end,
	},

	{
		"lervag/vimtex",
		ft = "tex",
		tag = "v1.6",
		dependencies = {
			"KeitaNakamura/tex-conceal.vim",
		},
		keys = {
			{ "<leader>ll", "<cmd>:VimtexCompile<cr>", desc = "compiles tex to pdf" },
		},
		config = function()
			vim.opt.conceallevel = 2
			vim.g.tex_conceal = "abdgms"
			vim.g.tex_superscripts = "[0-9a-zA-W.,:;+-<>/()=]"
			vim.g.tex_subscripts = "[0-9aehijklmnoprstuvx,+-/().]"
			vim.g.tex_conceal_frac = 1
		end,
	},

	{
		"andrewferrier/wrapping.nvim",
		event = "VeryLazy",
		config = function()
			require("wrapping").setup()
		end,
	},

	{
		"lukas-reineke/indent-blankline.nvim", 
		event = "BufEnter",
		config = function()
			require("indent_blankline").setup()
		end,
	},

	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = {
			"cd app && yarn install",
		},
		config = function()
		end,
	},

	{
		"aspeddro/pandoc.nvim",
		ft = "markdown",
		config = function()
			require("pandoc").setup()
		end,
	},

	{
		"numToStr/Comment.nvim",
		event = "InsertEnter",
		config = function()
			require("Comment").setup()
		end,
	},

	{
		"s1n7ax/nvim-terminal",
		event = "VeryLazy",
		config = function()
			vim.opt.hidden = true, require("nvim-terminal").setup()
		end,
	},

	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = function()
			require("nvim-autopairs").setup({
				disable_filetype = { "TelescopePrompt", "vim" },
			})
		end,
	},

	{
		"romgrk/barbar.nvim",
		event = "VeryLazy",
		keys = {
			{ "<leader>c", "<cmd>BufferClose<cr>", desc = "Closes current tab" },
		},
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		opts = {},
	},

	{
		"nvim-telescope/telescope.nvim",
		event = "VeryLazy",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		keys = {
			{ "<leader>ff", "<cmd> Telescope find_files<cr>", desc = "Find files" },
			{ "<leader>fg", "<cmd>Telescope live_grep<cr>",   desc = "Live grep" },
		},
	},

	{
		"folke/which-key.nvim",
		config = function()
			vim.opt.timeout = true
			vim.opt.timeoutlen = 300
			require("which-key").setup({})
		end,
	},

	{
		"nvim-lualine/lualine.nvim",
		lazy = false,
		dependencies = {
			"nvim-tree/nvim-web-devicons",
			opt = true,
		},
		config = function()
			local theme = require("lualine.themes.ayu_dark")
			require("lualine").setup({
				options = {
					theme = theme,
				},
			})
		end,
	},

	{
		"numToStr/Navigator.nvim",
		event = "VeryLazy",
		keys = {
			{ "<C-k>", "<cmd>NavigatorUp<cr>" },
			{ "<C-j>", "<cmd>NavigatorDown<cr>" },
			{ "<C-h>", "<cmd>NavigatorLeft<cr>" },
			{ "<C-l>", "<cmd>NavigatorRight<cr>" },
		},
		config = function()
			require("Navigator").setup()
		end,
	},

	{
		"nvim-tree/nvim-tree.lua",
		event = "VeryLazy",
		dependencies = {
			"nvim-tree/nvim-web-devicons",
			lazy = true,
		},
		keys = {
			{ "<C-n>", "<cmd>NvimTreeToggle<cr>", desc = "Toggles file tree" },
		},
		config = function()
			require("nvim-tree").setup()
		end,
	},

	{
		"SirVer/ultisnips",
		event = "VeryLazy",
		dependencies = {
			"ItsMindstorm/vim-snippets",
		},
		keys = {},
		config = function()
		end,
	},

	{
		"neovim/nvim-lspconfig",
		event = "BufEnter",
		dependencies = {
			{
				"williamboman/mason.nvim",
				config = function()
					require("mason").setup({
						ui = {
							icons = {
								package_installed = "✓",
								package_pending = "➜",
								package_uninstalled = "✗",
							},
						},
					})
				end,
			},

			{
				"williamboman/mason-lspconfig.nvim",
				config = function()
					require("mason-lspconfig").setup({
						ensure_installed = {
							"lua_ls",
							"texlab",
						},
					})
				end,
			},

			{
				"jayp0521/mason-null-ls.nvim",
				config = function()
					require("mason-null-ls").setup({
						ensure_installed = {
							"stylua",
						},
						automatic_setup = true,
					})
				end,
			},

			{
				"jose-elias-alvarez/null-ls.nvim",
				dependencies = {
					"nvim-lua/plenary.nvim",
				},
				keys = {
					{
						"<leader>lf",
						"<cmd> lua vim.lsp.buf.format({async = false})<cr>",
						desc = "formats the buffer",
					},
				},
				config = function()
					local null_ls = require("null-ls")

					null_ls.setup({
						sources = {
							null_ls.builtins.formatting.stylua,
						},
					})
				end,
			},

			{
				"nvim-treesitter/nvim-treesitter",
				config = function()
					require("nvim-treesitter.configs").setup({
						ensure_installed = {
							"lua",
						},
					})
				end,
			},

			{
				"onsails/lspkind.nvim", 
				config = function()
					require("lspkind").init({
						mode = "tex_symbol",
					})
				end,
			},
		},
		config = function()
		end,
	},

	{
		"hrsh7th/nvim-cmp",
		event = "BufEnter",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"quangnguyen30192/cmp-nvim-ultisnips",
			"hrsh7th/cmp-path",
		},
		config = function()
			local cmp = require("cmp")

			cmp.setup({
				snippet = {
					expand = function(args)
						vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
					end,
				},
				window = {
					-- completion = cmp.config.window.bordered(),
					-- documentation = cmp.config.window.bordered(),
				},
				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-Space>"] = cmp.mapping.complete(),
					["<C-e>"] = cmp.mapping.abort(),
					["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "ultisnips" }, -- For ultisnips users.
					{ name = "path" },
				}, {
					{ name = "buffer" },
				}),
			})

			local lspconfig = require("lspconfig")
			local capabilities = require("cmp_nvim_lsp").default_capabilities()
			lspconfig.lua_ls.setup({
				capabilities = capabilities,
			})
		end,
	},
}
