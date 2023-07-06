local M = {}

M.vimtex = {
	n = {
		["<leader>ll"] = {
			"<cmd> VimtexCompile <CR>",
			"Compile latex file",
		},
	},
}
M.fugitive = {
	n = {
		["<leader>g"] = {
			"<cmd> Git <CR>",
			"Open Fugitive",
		},

		["<leader>gp"] = {
			"<cmd> Git push <CR>",
			"Push commits",
		},
	},
}
M.lspsaga = {
	n = {
		["<leader>lf"] = {
			"<cmd>Lspsaga lsp_finder<CR>",
			"Find the symbol's definition",
		},

		["<leader>lca"] = {
			"<cmd>Lspsaga code_action<CR>",
			"Code action for lspsaga",
		},

		["<leader>lr"] = {
			"<cmd>Lspsaga rename <CR>",
			"Rename all occurunces of hovered word",
		},

		["<leader>lpd"] = {
			"<cmd>Lspsaga peek_definition<CR>",
			"Peek definition",
		},

		["<leader>lgd"] = {
			"<cmd>Lspsaga goto_definition<CR>",
			"Go to definition",
		},
	},
}

return M
