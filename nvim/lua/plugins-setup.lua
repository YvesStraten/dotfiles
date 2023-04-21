local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
    vim.cmd([[packadd packer.nvim]])
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

  use("ellisonleao/gruvbox.nvim") -- color scheme
  use("catppuccin/nvim")

  -- lua functions
  use("nvim-lua/plenary.nvim")

  -- split windows navigation
  use("christoomey/vim-tmux-navigator")

  use("szw/vim-maximizer")

  -- essential plugins
  use("tpope/vim-surround")
  use("vim-scripts/ReplaceWithRegister")

  use("numToStr/Comment.nvim")

  -- file explorer
  use("nvim-tree/nvim-tree.lua")

  -- status line
  use("nvim-lualine/lualine.nvim")

  -- autocompletion
  use("hrsh7th/nvim-cmp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/cmp-path")

  -- managing & installing lsp servers, linters & formatters
  use("williamboman/mason.nvim") -- in charge of managing lsp servers, linters & formatters
  use("williamboman/mason-lspconfig.nvim") -- bridges gap b/w mason & lspconfig

  -- configuring lsp servers
  use("neovim/nvim-lspconfig") -- easily configure language servers
  use("hrsh7th/cmp-nvim-lsp") -- for autocompletion

  -- auto closing
  use("windwp/nvim-autopairs") -- autoclose parens, brackets, quotes, etc...

  -- git integration
  use("lewis6991/gitsigns.nvim") -- show line modifications on left hand side

  -- LateX and snippets
  use { "lervag/vimtex", tag = "v1.6" }
  use("SirVer/ultisnips")

  -- cmp ultisnips
  use("quangnguyen30192/cmp-nvim-ultisnips")

  -- which key
  use("folke/which-key.nvim")

  use("ryanoasis/vim-devicons")

  use("ItsMindstorm/vim-snippets")
  use("onsails/lspkind.nvim")

  use("da-h/AirLatex.vim")

  use("junegunn/goyo.vim")

  use("KeitaNakamura/tex-conceal.vim")

  use("andrewferrier/wrapping.nvim")

  use("vim-pandoc/vim-pandoc")


  if packer_bootstrap then
    require("packer").sync()
  end
end)
