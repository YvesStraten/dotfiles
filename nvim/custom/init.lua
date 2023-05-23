vim.g.tex_flavor = "latex"
vim.opt.relativenumber = true

vim.keymap.set("n", "<leader>sp", ":set spell<CR> <bar> :set spelllang=en_gb<CR>")

vim.cmd([[
set backupdir=~/nvim/backup//
]])
