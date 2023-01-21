vim.keymap.set('n', '<leader>sr', require('telescope.builtin').resume, { desc = '[S]earch [R]esume' })

vim.g.UltiSnipsExpandTrigger="<tab>"
vim.g.UltiSnipsJumpForwardTrigger="<c-j>"
vim.g.UltiSnipsJumpBackwardTrigger="<c-k>"

vim.g.vim_markdown_folding_disabled = 1

vim.go.vim_markdown_conceal = 0
vim.go.vim_markdown_math = 1

vim.go.vim_markdown_frontmatter = 1
vim.go.vim_markdown_toml_frontmatter = 1
vim.go.vim_markdown_json_frontmatter = 1

vim.g.tex_flavor = "latex"
vim.g.vimtex_view_general_viewer = "okular"
vim.g.vimtex_view_general_options = "--unique file:@pdf\#src:@line@tex"
vim.go.vimtex_quickfix_mode = 0
vim.go.conceallevel = 1
vim.g.tex_conceal = 'abdmg'

vim.go.spell = true
vim.go.spelllang = "en_gb"
