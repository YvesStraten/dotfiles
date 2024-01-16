local wk = require("which-key")
-- local harpoon = require("harpoon")

-- harpoon:setup()

wk.register({
	["<leader>"] = {
		--	a = {
		--		function()
		--			harpoon:list():append()
		--		end,
		--		"Harpoon append",
		--	},
		--	h = {
		--		function()
		--			harpoon.ui:toggle_quick_menu(harpoon:list())
		--		end,
		--		"Harpoon list",
		--	},

		f = {
			name = "Telescope",
			f = { ":Telescope find_files<cr>", "Find file" },
			g = { ":Telescope live_grep<cr>", "Grep files" },
			p = { ":Telescope project<cr>", "Project picker" },
		},

		u = { ":UndotreeShow<cr>", "Undotree" },

		-- s = {
		-- 	h = { ":split<cr>", "Horizontal split" },
		-- 	v = { ":vsplit<cr>", "Vertical split" },
		-- 	p = { ":set spell<cr> <bar> set spelllang=en_gb<cr>" },
		-- },

		l = {
			r = { ":Lspsaga rename<cr>", "Rename definition" },
			p = { ":Lspsaga peek_definition<cr>", "Peek definition" },
			a = { ":Lspsaga code_action<cr>", "Code action" },
		},

		g = {
			o = { ":Git<cr>", "Fugitive" },
			p = { ":Git push<cr>", "Git push" },
		},
	},
	-- ["<C-t>"] = {
	-- 	function()
	-- 		harpoon:list():select(1)
	-- 	end,
	-- 	"1",
	-- },

	-- ["<C-y>"] = {
	-- 	function()
	-- 		harpoon:list():select(2)
	-- 	end,
	-- 	"2",
	-- },

	-- ["<C-p>"] = {
	-- 	function()
	-- 		harpoon:list():select(3)
	-- 	end,
	-- 	"3",
	-- },

	-- ["<C-s>"] = {
	-- 	function()
	-- 		harpoon:list():select(4)
	-- 	end,
	-- 	"4",
	-- },
	["<C-n>"] = {
		":Neotree toggle<cr>",
		"File manager",
	},
	["<tab>"] = {
		":BufferNext<cr>",
		"Cycle buffers",
	},
	["<S-tab>"] = {
		":BufferPrevious<cr>",
		"Cycle buffers",
	},
})
