return {

	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			vim.cmd([[colorscheme tokyonight]])
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
			vim.g.tex_conceal = "abdgm"
			vim.g.tex_superscripts = "[0-9a-zA-W.,:;+-<>/()=]"
			vim.g.tex_subscripts = "[0-9aehijklmnoprstuvx,+-/().]"
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
		event = "VeryLazy",
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
			"nvim-lua/plenary.nvim"
		},
		keys = {
			{ "<leader>ff", "<cmd> Telescope find_files<cr>", desc = "Find files"},
			{ "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live grep"},

		},

	},

	{
		"folke/which-key.nvim",
		config = function()
			vim.opt.timeout = true
			vim.opt.timeoutlen = 300 
			require("which-key").setup({

			})
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
			local gruvbox = require("lualine.themes.gruvbox")
			require("lualine").setup({
				options = {
					theme = gruvbox,
				},
			})
		end,
	},

	{
		"numToStr/Navigator.nvim",
		event = "VeryLazy",
		keys = {
			{ "<C-j>", "<cmd>NavigatorUp<cr>" },
			{ "<C-k>", "<cmd>NavigatorDown<cr>" },
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
		event = "VeryLazy",
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
						},
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
		},
		config = function()
			local lspconfig = require("lspconfig")
			local capabilities = require("cmp_nvim_lsp").default_capabilities()
			lspconfig.lua_ls.setup({
				capabilities = capabilities,
			})
		end,
	},

	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"quangnguyen30192/cmp-nvim-ultisnips",
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
		end,
	},
}
