local M = {}

M.vimtex = {
	n = {
		["<leader>ll"] = {
			"<cmd> VimtexCompile <CR>",
			"Compile latex file",
		},
	},
}

return M
