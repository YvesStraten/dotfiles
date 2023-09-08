local wk = require("which-key")
wk.register({
	["<leader>"] = {
		f = {
			name = "Telescope",
			f = { ":Telescope find_files<cr>", "Find file" },
			g = { ":Telescope live_grep<cr>", "Grep files" },
			p = { ":Telescope project<cr>", "Project picker" }
		},

		x = { ":BufferClose<cr>", "Close buffer" },
		s = {
			h = { ":split<cr>", "Horizontal split" },
			v = { ":vsplit<cr>", "Vertical split" },
			p = { ":set spell<cr> <bar> set spelllang=en_gb<cr>" },
		},

		l = {
			r = { ":Lspsaga rename<cr>", "Rename definition" },
			p = { ":Lspsaga peek_definition<cr>", "Peek definition" },
		},

		g = {
			o = { ":Neogit<cr>", "Neogit" }
		}
	},
	["<C-n>"] = { ":Neotree toggle reveal=true<cr>", "File explorer" },
	["<tab>"] = { ":BufferNext<cr>", "Next Buffer" },
	["<S-tab>"] = { ":BufferPrevious<cr>", "Previous Buffer" },
	["<esc>"] = { ":nohlsearch<cr>", "Wipes previous search" }
})
