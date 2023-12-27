local function lowercase_mode()
	local mode_map = {
		n = "normal",
		i = "insert",
		v = "visual",
		V = "v-line",
		c = "command",
		R = "replace",
		s = "select",
		S = "s-line",
		[""] = "empty",
		t = "terminal",
	}

	local mode = vim.fn.mode()
	return mode_map[mode] or mode
end

require("lualine").setup({
	sections = {
		lualine_a = { lowercase_mode },
		lualine_b = { "branch", "diff", "diagnostics" },
		lualine_c = { "filename", "os.date('%a')" },
		lualine_x = { "filetype" },
		lualine_y = { "progress" },
		lualine_z = { "location" },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {},
		lualine_x = { "encoding", "fileformat" },
		lualine_y = {},
		lualine_z = {},
	},
	globalstatus = true,
})
