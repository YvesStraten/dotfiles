local vimtex_setup, vimtex = pcall(require, "vimtex")
if not vimtex_setup then
	return
end

vim.g.tex_flavor = "latex"
vim.g.vimtex_view_method = "okular"
vim.go.vimtex_quickfix_mode = 0
vim.go.conceallevel = 1
vim.g.tex_conceal = "abdmg"

vimtex.setup()
