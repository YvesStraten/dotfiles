local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

-- reloads nvim when saving this file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins-setup.lua source <afile> | PackerSync
  augroup end
]])

local status, packer = pcall(require, "packer")
if not status then
	return
end

return packer.startup(function(use)
	use("wbthomason/packer.nvim")
  
	use("EdenEast/nightfox.nvim") -- color scheme

  -- split windows navigation
  use("christoomey/vim-tmux-navigator")

  use("szw/vim-maximizer")

  use("tpope/vim-surround")
  use("vim-scripts/ReplaceWithRegister")


	if packer_bootstrap then
		require("packer").sync()
	end
end)
