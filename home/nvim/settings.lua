vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
-- Only 8 lines
vim.opt.scrolloff = 8
vim.opt.updatetime = 50
vim.opt.relativenumber = true
vim.opt.termguicolors = true

-- Incremental search
vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.g.mapleader = " "

-- Easy movement of selected lines
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Jumps half pages
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Keep cursor in place when searching
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- Allows for cutting to keep stuff in the clip
vim.keymap.set("x", "<leader>p", '"_dP')

vim.keymap.set("n", "<leader>y", '"+y')
vim.keymap.set("v", "<leader>y", '"+y')
vim.keymap.set("n", "<leader>Y", '"+Y')

-- Buffer management
vim.keymap.set("n", "<leader>x", ":BufferClose<cr>")
vim.keymap.set("n", "<leader>X", ":BufferCloseAllButCurrent<cr>")
-- Splits
vim.keymap.set("n", "<leader>sh", ":split<cr>")
vim.keymap.set("n", "<leader>sv", ":vsplit<cr>")

-- Spelling
vim.keymap.set("n", "<leader>sp", ":set spell<cr> <bar> set spelllang=en_gb<cr>")

vim.keymap.set("n", "<esc>", ":nohlsearch<cr>", { silent = true })

vim.opt.clipboard = ""

vim.opt.signcolumn = "yes"

vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.cmd([[
set backupdir=~/nvim/backup//
]])
