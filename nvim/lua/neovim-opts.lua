vim.g.mapleader = " "
vim.opt.autoindent = true 
vim.opt.hlsearch = true 
vim.opt.ignorecase = true
vim.g.tex_flavor = "latex"
vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.shiftwidth = 2
vim.opt.expandtab = true 
vim.opt.autoindent = true 

vim.opt.wrap = false 

vim.opt.ignorecase = true
vim.opt.smartcase = true

-- terminal colors
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.opt.signcolumn = "yes"

-- backspace
vim.opt.backspace = "indent,eol,start"

-- clipboard
vim.opt.clipboard:append("unnamedplus")



vim.keymap.set("n", "<leader>sp", ":set spell<CR> <bar> :set spelllang=en_gb<CR>")

vim.cmd([[
set backupdir=~/nvim/backup//
]])
