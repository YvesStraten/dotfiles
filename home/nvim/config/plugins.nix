{ pkgs
, inputs
, ...
}: {
  imports = [
    ./telescope/telescope.nix
    ./lsp/lsp.nix
    ./lsp/null-ls.nix
    ./lsp/nvim-cmp.nix
    ./lsp/dap.nix
    ./gitsigns/gitsigns.nix
    # ./dashboard/dashboard.nix
  ];

  plugins = {
    barbar = {
      enable = true;
      tabpages = true;
    };
    treesitter = {
      enable = true;
      ensureInstalled = "all";
      disabledLanguages = [
        "latex"
      ];
    };
    which-key.enable = true;

    lualine.enable = true;
    comment-nvim.enable = true;

    tmux-navigator.enable = true;
    indent-blankline.enable = true;

    noice.enable = true;

    notify.enable = true;
    nvim-colorizer.enable = true;
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
    toggleterm = {
      enable = true;
      direction = "float";
      hideNumbers = true;
      openMapping = "<C-e>";

      floatOpts.border = "curved";
    };
    vimtex = {
      enable = true;
      viewMethod = "zathura_simple";
    };
  };

  colorschemes.kanagawa = {
    enable = true;
    compile = true;
    transparent = true;
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
