local opt = vim.opt -- for conciness

-- line numbers
opt.relativenumber = true
opt.number = true

-- tabs and indentation
opt.tabstop = 2
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

-- spell setup
vim.api.nvim_create_autocmd({ "BufEnter" }, {
	pattern = "*.tex",
	callback = function()
		vim.cmd("setlocal spell")
		vim.cmd("setlocal spelllang=en_gb")
	end,
})
