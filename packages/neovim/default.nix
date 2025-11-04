{ pkgs, lib, ... }:
{
  imports = [
  ];

  vim = {
    luaConfigPre = ''
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

    treesitter.context = {
      enable = true;
      setupOpts.max_lines = 5;
    };

    lsp = {
      enable = true;
      inlayHints.enable = true;
      formatOnSave = true;
      lspsaga.enable = true;
      lspkind.enable = true;
      trouble.enable = true;
      lightbulb.enable = true;
      nvim-docs-view.enable = true;
      otter-nvim.enable = true;
    };

    diagnostics.nvim-lint = {
      enable = true;
      linters.flake8.cmd = "${pkgs.python3Packages.flake8}/bin/flake8";
      linters_by_ft = {
        python = [
          "flake8"
        ];
      };
    };

    languages = {
      enableFormat = true;
      enableExtraDiagnostics = true;
      enableTreesitter = true;

      nix = {
        enable = true;
        extraDiagnostics.enable = true;
        format.type = "nixfmt";
        format.package = pkgs.nixfmt-rfc-style;
      };
      typst.enable = true;
      # TODO: Add when latex support is merged https://github.com/NotAShelf/nvf/pull/569
      # tex = {
      #   enable = true;
      #   pdfViewer.zathura.enable = true;
      #   lsp.texlab.forwardSearch.enable = true;
      #   build = {
      #     forwardSearchAfter = true;
      #     onSave = true;
      #     builder.args = [
      #       "-pdf"
      #       "-shell-escape"
      #       "%f"
      #     ];
      #   };
      # };

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
      };

      java.enable = true;
      haskell.enable = true;
      python = {
        enable = true;
        format.enable = false;
      };
      sql.enable = true;
      svelte.enable = true;
      ts.enable = true;
      bash.enable = true;
      clang.enable = true;
    };

    autopairs.nvim-autopairs.enable = true;

    autocomplete.nvim-cmp.enable = true;

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
          open = "<c-t>";
        };
      };
    };

    filetree.neo-tree = {
      enable = true;
      setupOpts = {
        enable_cursor_hijack = true;
        reveal = true;
        window.width = 25;
        filesystem = {
          filtered_items = {
            hide_dotfiles = false;
            hide_gitignored = false;
            hide_hidden = false;
          };

          follow_current_file = {
            enabled = true;
            leave_dirs_open = true;
          };
        };
      };
    };

    git.gitsigns.enable = true;
    git.git-conflict.enable = true;

    options = {
      tabstop = 2;
      shiftwidth = 2;
      scrolloff = 8;
    };

    dashboard.alpha.enable = true;

    notes = {
      todo-comments.enable = true;
    };

    ui = {
      noice.enable = true;
      colorizer.enable = true;
      borders.enable = true;
      fastaction.enable = true;
    };

    visuals = {
      indent-blankline.enable = true;
    };

    utility = {
      surround.enable = true;
    };

    navigation.harpoon.enable = true;

    comments.comment-nvim.enable = true;

    presence.neocord.enable = true;
  };
}
