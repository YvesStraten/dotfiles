local vimtex_setup, vimtex = pcall(require, "vimtex")
if not vimtex_setup then
	return
end

vimtex.setup()
