local status, _ = pcall(vim.cmd, "colorscheme nordfox")
if not status then
	print("colorscheme not found!")
  return
end
