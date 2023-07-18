local M = {}

M.vimtex = {
	plugin = true,
	n = {
		["<leader>ll"] = {
			"<cmd> VimtexCompile <CR>",
			"Compile latex file",
		},
	},
}
M.oil = {
  n = {
    ["<C-n>"] = {
      "<cmd> Oil --float <CR>",
      "Open file explorer"
    },
  },
}
M.ouroboros = {
	plugin = true,
	n = {
		["<leader>jp"] = {
			"<cmd> Ouroboros <CR>",
			"Jump to header file",
		},
	},
}
M.markdownpreview = {
	plugin = true,
	n = {
		["<leader>ll"] = {
			"<cmd> MarkDownPreview <CR>",
			"Preview Markdown",
		},
	},
}
M.dap = {
	plugin = true,
	n = {
		["<leader>db"] = {
			"<cmd> DapToggleBreakpoint <CR>",
			"Toggle Breakpoint at line",
		},

		["<leader>dr"] = {
			"<cmd> DapContinue <CR>",
			"Start or continue debugger",
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
			"Rename all occurrences of hovered word",
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
