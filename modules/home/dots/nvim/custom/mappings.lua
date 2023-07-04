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
    }
  },
}

return M
