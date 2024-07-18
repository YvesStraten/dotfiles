{pkgs, ...}: {
  imports = [
    ./telescope/telescope.nix
    ./lsp/lsp.nix
    ./lsp/null-ls.nix
    ./lsp/nvim-cmp.nix
    ./lsp/dap.nix
    ./gitsigns/gitsigns.nix
    ./lualine.nix
    # ./dashboard/dashboard.nix
  ];

  plugins = {
    neocord.enable = true;
    barbar = {
      enable = true;
      tabpages = true;
    };
    treesitter = {
      enable = true;
      disabledLanguages = ["tex"];
      nixvimInjections = true;
      settings = {
        indent.enable = true;
      };
    };

    treesitter-context.enable = true;
    which-key.enable = true;

    comment.enable = true;

    tmux-navigator.enable = true;
    indent-blankline.enable = true;
    ts-autotag.enable = true;

    # noice.enable = true;

    nvim-colorizer = {
      enable = true;
      userDefaultOptions = {
        tailwind = true;
        sass.enable = true;
      };
    };
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

    neogit.enable = true;
    undotree.enable = true;
    toggleterm = {
      enable = true;
      settings = {
        direction = "float";
        hideNumbers = true;
        open_mapping = "[[<C-e>]]";
        floatOpts.border = "curved";
      };
    };
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
    ultisnips
    ouroboros
    vim-dadbod
    vim-dadbod-ui
    vim-dadbod-completion
    vim-snippets
    bullets-vim
  ];
}
