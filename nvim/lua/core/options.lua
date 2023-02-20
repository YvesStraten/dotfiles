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
let g:livepreview_previewer = "zathura"
let g:updatime = "1000"
]])

vim.cmd([[
let g:tex_flavor = "latex"
let g:conceallevel = 2
let g:tex_conceal = "abdgm"
]])

vim.cmd([[
let g:AirLatexUsername="yves.straten@gmail.com"
let g:AirLatexDomain="www.overleaf.com"
]])
