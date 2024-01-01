local function lowercase_mode()
	local mode_map = {
		n = "Normal",
		i = "Insert",
		v = "Visual",
		V = "V-line",
		c = "Command",
		R = "Replace",
		s = "Select",
		S = "S-line",
		[""] = "Empty",
		t = "Terminal",
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
