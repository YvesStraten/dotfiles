vim.g.tex_flavor = "latex"
vim.opt.conceallevel = 2
vim.g.tex_conceal = "abdgm"
-- vim.g.tex_superscripts = "[0-9a-zA-W.,:;+-<>/()=]"
-- vim.g.tex_subscripts = "[0-9aehijklmnoprstuvx,+-/().]"
-- vim.g.tex_conceal_frac = 1

if vim.loop.os_uname().sysname == "Darwin" then
	vim.g.vimtex_view_method = "skim"
else
	vim.g.vimtex_view_method = "zathura"
end

-- Options needed to get minted to work
vim.g.vimtex_compiler_latexmk = {
	options = {
		"-verbose",
		"-file-line-error",
		"-synctex=1",
		"-interaction=nonstopmode",
		"-shell-escape",
	},
}

vim.keymap.set("n", "<leader>tl", "<cmd>VimtexCompile<cr>")
vim.keymap.set("n", "<C-c>", "<cmd>VimtexTocToggle<cr>")
