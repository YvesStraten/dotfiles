local opt = vim.opt -- for conciness

-- line numbers
opt.number = true
opt.relativenumber = true

-- tabs and indentation
opt.tabstop = 4
opt.shiftwidth = 2
opt.expandtab = true
opt.autoindent = true

-- line wrap
opt.wrap = false

-- search settings
opt.ignorecase = true
opt.smartcase = true

-- terminal colors
opt.termguicolors = true
opt.background = "dark"
opt.signcolumn = "yes"

-- backspace
opt.backspace = "indent,eol,start"

-- clipboard
opt.clipboard:append("unnamedplus")

-- split windows
opt.splitright = true
opt.splitbelow = true

opt.iskeyword:append("-")

-- time out which key
vim.o.timeout = true
vim.o.timeoutlen = 300

vim.cmd([[
let g:vimtex_view_method = "zathura"
let g:vimtex_fold_enabled=1
let g:tex_flavor = "latex"
let g:vimtex_syntax_conceal_default=1
let g:vimtex_quickfix_mode=0
set conceallevel=2
let g:tex_conceal="abdgm"
let g:tex_superscripts= "[0-9a-zA-W.,:;+-<>/()=]"
let g:tex_subscripts= "[0-9aehijklmnoprstuvx,+-/().]"
let g:tex_conceal_frac=1
]])

vim.cmd([[
set backupdir=~/nvim/backup//
]])
