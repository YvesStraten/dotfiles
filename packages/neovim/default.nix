{...}: {
  imports = [
  ];

  vim = {
    luaConfigPre = ''
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled(), { bufnr })
      vim.opt.undodir = os.getenv('HOME') .. '/.vim/undodir';
      vim.opt.backupdir = os.getenv('HOME') .. '/.vim/backup';
    '';

    keymaps = [
      {
        mode = "x";
        key = "<leader>p";
        action = "\"_dP";
      }

      {
        mode = "n";
        key = "<leader>y";
        action = "\"+y";
      }

      {
        mode = "v";
        key = "<leader>y";
        action = "\"+y";
      }

      {
        mode = "v";
        key = "<leader>Y";
        action = "\"+Y";
      }

      {
        mode = "v";
        key = "J";
        action = ":m '>+1<CR>gv=gv";
        silent = true;
      }

      {
        mode = "v";
        key = "K";
        action = ":m '<-2<CR>gv=gv";
        silent = true;
      }

      {
        key = "<C-d>";
        mode = "n";
        action = "<C-d>zz";
        silent = true;
      }

      {
        key = "<C-u>";
        mode = "n";
        action = "<C-u>zz";
        silent = true;
      }

      {
        key = "<esc>";
        mode = "n";
        action = ":nohlsearch<CR>";
        silent = true;
      }

      {
        key = "<leader>toc";
        mode = "n";
        action = ":VimtexTocToggle<CR>";
        desc = "Toggle TOC";
      }

      {
        key = "<leader>ll";
        mode = "n";
        action = ":VimtexCompile<CR>";
        desc = "Compile latex document";
      }

      {
        key = "<leader>n";
        mode = "n";
        action = ":Neotree toggle<CR>";
        silent = true;
      }

      {
        key = "<leader>sp";
        mode = "n";
        action = ":split<CR>";
        desc = "Split horizontally";
        silent = true;
      }

      {
        key = "<leader>vp";
        mode = "n";
        action = ":vsplit<CR>";
        desc = "Split vertically";
        silent = true;
      }

      {
        key = "<leader>sh";
        mode = "n";
        action = ":split<cr>";
        silent = true;
      }

      {
        key = "<leader>sv";
        mode = "n";
        action = ":vsplit<cr>";
        silent = true;
      }

      {
        key = "n";
        mode = "n";
        action = "nzzzv";
        silent = true;
      }

      {
        key = "N";
        mode = "n";
        action = "Nzzzv";
        silent = true;
      }

      {
        key = "<leader>la";
        mode = "n";
        action = ":Lspsaga code_action<cr>";
        silent = true;
      }

      {
        mode = "t";
        key = "<Esc>";
        action = "<C-\\><C-n>";
        desc = "Exit terminal";
      }
    ];

    theme = {
      enable = true;
      name = "gruvbox";
      style = "dark";
    };

    languages = {
      enableLSP = true;
      enableFormat = true;
      enableExtraDiagnostics = true;
      enableTreesitter = true;

      nix.enable = true;
      # tex.enable = true;
      # tex.build.builder.args = [
      #   "-pdf"
      #   "-shell-escape"
      #   "%f"
      # ];

      rust = {
        enable = true;
        lsp.opts = ''
          ['rust-analyzer'] = {
            cargo = { allFeature = true },
            checkOnSave = true,
            procMacro = { enable = true },
          },
        '';
        crates.enable = true;
        format.enable = true;
      };

      java.enable = true;
      sql.enable = true;
      svelte.enable = true;
      ts.enable = true;
      bash.enable = true;
      clang.enable = true;
    };

    lsp = {
      formatOnSave = true;
      lspsaga.enable = true;
      trouble.enable = true;
      otter-nvim.enable = true;
    };

    autopairs.nvim-autopairs.enable = true;
    autocomplete = {
      blink-cmp.enable = true;
      blink-cmp.friendly-snippets.enable = true;
    };

    snippets.luasnip.enable = true;

    spellcheck.enable = true;
    telescope.enable = true;

    binds = {
      whichKey.enable = true;
      cheatsheet.enable = true;
    };

    statusline.lualine = {
      enable = true;
    };

    terminal = {
      toggleterm = {
        enable = true;
        lazygit.enable = true;
        mappings = {
          open = "<c-e>";
        };
      };
    };

    filetree.neo-tree.enable = true;

    git.gitsigns.enable = true;

    options = {
      tabstop = 2;
      shiftwidth = 2;
      scrolloff = 8;
    };

    ui = {
      noice.enable = true;
      colorizer.enable = true;
    };

    visuals = {
      indent-blankline.enable = true;
    };
  };
}
