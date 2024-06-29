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
    presence-nvim = {
      enable = true;
      autoUpdate = true;
    };
    barbar = {
      enable = true;
      tabpages = true;
    };
    treesitter = {
      enable = true;
      indent = true;
      ensureInstalled = "all";
      disabledLanguages = ["tex"];
      nixvimInjections = true;
    };
    which-key.enable = true;

    comment.enable = true;

    tmux-navigator.enable = true;
    indent-blankline.enable = true;
    ts-autotag.enable = true;

    noice.enable = true;

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

  colorschemes.kanagawa = {
    enable = true;
    settings = {
      compile = true;
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
