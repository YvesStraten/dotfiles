return function(use)
  use({
    "folke/which-key.nvim",
      config = function()
        require("which-key").setup({})
      end
  })

  use({ 
    "iamcco/markdown-preview.nvim",
    run = "cd app && npm install",
    setup = function()
      vim.g.mkdp_filetypes = { "markdown" } 
  end,
  ft = { "markdown" },
  cmd = { "MarkdownPreview" },
  requires = { "zhaozg/vim-diagram", "aklt/plantuml-syntax" },
  })
  
  use({ "SirVer/ultisnips",
  })

  use({ "honza/vim-snippets",
  })

  use({ "godlygeek/tabular",
  })

  use({ "elzr/vim-json",
  })

  use({ "plasticboy/vim-markdown"
  })

  use({ "vim-pandoc/vim-pandoc-syntax"
  })

   
  use({ "preservim/nerdtree"
  })

  use({ "lervag/vimtex"
  })
  
  use({ "KeitaNakamura/tex-conceal.vim"
  })
  
end

