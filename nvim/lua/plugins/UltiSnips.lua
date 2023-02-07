local UltiSnips_setup, UltiSnips = pcall(require, "UltiSnips")
if not UltiSnips_setup then
	return
end

vim.cmd([[

let g:UltiSnipsEditSplit = "vertical"
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumBackwardTrigger - "<c-k>"

]])

UltiSnips.setup()
