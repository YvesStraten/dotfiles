local UltiSnips_setup, UltiSnips = pcall(require, "UltiSnips")
if not UltiSnips_setup then
  return
end 

vim.g.UltiSnipsEditSplit="vertical"
vim.g.UltiSnipsExpandTrigger = "<tab>"
vim.g.UltiSnipsJumpForwardTrigger = "<c-j>"
vim.g.UltiSnipsJumpBackwardTrigger = "<c-k>"
vim.g.UltiSnipsSnippetDirectories=$HOME"/.config/nvim/UltiSnips"

UltiSnips.setup()
