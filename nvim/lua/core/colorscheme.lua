local status, _ = pcall(vim.cmd, "colorscheme catppuccin-macchiato")
if not status then
  print("colorscheme not found!")
  return
end
