vim.g.mapleader = " " 

local keymap = vim.keymap -- concinesess

-- general keymaps
keymap.set("i", "jk", "<ESC>")

keymap.set("n", "<leader>nh", ":nohl<CR>")


keymap.set("n", "<leader>+", "<C-a>")
keymap.set("n", "<leader>-", "<C-x>")

keymap.set("n", "<leader>sv", "<C-w>v") --vertical split
keymap.set("n", "<leader>sh", "<C-w>s") -- horizontal split
keymap.set("n", "<leader>se", "<C-w>=") -- equal window split
keymap.set("n", "<leader>sx", ":close<CR>") -- close window split

keymap.set("n", "<leader>to", ":tabnew<CR>")
keymap.set("n", "<leader>tx", ":tabclose<CR>")
keymap.set("n", "<leader>tn", "tabn<CR>")
keymap.set("n", "<leader>tp", "tabp<CR>")

-- plugin keymaps
keymap.set("n", "<leader>sm", ":MaximizerToggle<CR>")

