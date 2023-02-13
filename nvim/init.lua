require("plugins-setup")
require("core.options")
require("core.keymaps")
require("core.colorscheme")

-- LSP
require("plugins.lsp.mason")
require("plugins.lsp.lspconfig")

-- Plugin imports
require("plugins.nvim-cmp")
require("plugins.comment")
require("plugins.nvim-tree")
require("plugins.lualine")
require("plugins.telescope")
require("plugins.gitsigns")

-- tex
require("plugins.vimtex")

-- whichkey
require("plugins.whichkey")

-- Ultisnips
require("plugins.UltiSnips")
