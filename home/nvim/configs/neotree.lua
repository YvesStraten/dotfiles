require("neo-tree").setup({
	close_if_last_window = true,
	window = {
		width = 20,
	},
	filesystem = {
		follow_current_file = {
			enabled = true,
			leave_dirs_open = true,
		},
	},
})
