{pkgs, ...}: {
  imports = [
		./hardtime.nix
    ./telescope/telescope.nix
    ./toggleterm.nix
    ./lsp/lsp.nix
		./lsp/lspsaga.nix
		./lsp/trouble.nix
		./lsp/notify.nix
    ./lsp/conform.nix
    ./lsp/nvim-cmp.nix
    ./lsp/dap.nix
    ./gitsigns/gitsigns.nix
    ./lualine.nix
    # ./dashboard/dashboard.nix
    ./modules
  ];

  plugins = {
		lazy.enable = true;
    lazygit = {
      enable = true;
      gitPackage = null;
    };

    harpoon = {
      enable = true;
      enableTelescope = true;
      keymaps = {
        addFile = "<leader>a";
        toggleQuickMenu = "<C-a>";
        navFile = {
          "1" = "<C-h>";
          "2" = "<C-t>";
          "3" = "<C-n>";
          "4" = "<C-s>";
        };
      };
    };

    headlines.enable = true;
    neocord.enable = true;

    treesitter = {
      enable = true;
      nixvimInjections = true;
      settings = {
        indent.enable = true;
        highlight = {
          enable = true;
          disable = ["latex" "xml"];
        };
      };
    };

    treesitter-context = {
      enable = true;
      settings.max_lines = 5;
    };

    which-key.enable = true;

    comment.enable = true;

    indent-blankline.enable = true;
    ts-autotag.enable = true;

    noice.enable = true;

    nvim-autopairs.enable = true;
    neo-tree = {
      enable = true;
      enableModifiedMarkers = true;
      autoCleanAfterSessionRestore = true;
      closeIfLastWindow = true;
      enableRefreshOnWrite = true;

      window.width = 25;
      filesystem = {
        followCurrentFile = {
          enabled = true;
          leaveDirsOpen = true;
        };
      };
    };

    fugitive.enable = true;
    undotree.enable = true;

    markdown-preview = {
      enable = true;
      settings.auto_start = true;
    };
    vimtex = {
      enable = true;
      texlivePackage = null;
      settings = {
        view_method =
          if pkgs.stdenv.isDarwin
          then "skim"
          else "zathura";
        compiler_latexmk = {
          options = [
            "-verbose"
            "-file-line-error"
            "-synctex=1"
            "-interaction=nonstopmode"
            "-shell-escape"
          ];
        };
      };
    };
  };

  colorschemes.catppuccin = {
    enable = true;
    settings = {
      background = {
        light = "macchiato";
        dark = "mocha";
      };
      flavour = "mocha"; # "latte", "mocha", "frappe", "macchiato" or raw lua code
      disable_bold = false;
      disable_italic = false;
      disable_underline = false;
      transparent_background = true;
      term_colors = true;
      integrations = {
        cmp = true;
        noice = true;
        notify = true;
        neotree = true;
        harpoon = true;
        gitsigns = true;
        which_key = true;
        illuminate = {
          enabled = true;
        };
        treesitter = true;
        treesitter_context = true;
        telescope.enabled = true;
        indent_blankline.enabled = true;
        mini.enabled = true;
        native_lsp = {
          enabled = true;
          inlay_hints = {
            background = true;
          };
          underlines = {
            errors = ["underline"];
            hints = ["underline"];
            information = ["underline"];
            warnings = ["underline"];
          };
        };
      };
    };
  };
  extraPlugins = with pkgs.vimPlugins; [
    friendly-snippets
    vim-dadbod
    vim-dadbod-ui
    vim-dadbod-completion
    bullets-vim
  ];
}
