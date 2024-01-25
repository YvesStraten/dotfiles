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
      indent = true;
      disabledLanguages = [
        "tex"
      ];
      nixvimInjections = true;
    };
    which-key.enable = true;

    lualine = {
      enable = true;
      globalstatus = true;
      sections = {
        lualine_a = [
          {
            name.__raw = ''
              function()
              	local mode_map = {
              		n = "Normal",
              		i = "Insert",
              		v = "Visual",
              		V = "V-line",
              		c = "Command",
              		R = "Replace",
              		s = "Select",
              		S = "S-line",
              		[""] = "Empty",
              		t = "Terminal",
              	}

              	local mode = vim.fn.mode()
              	return mode_map[mode] or mode
              end
            '';
          }
        ];
        lualine_b = [ "branch" "diff" "diagnostics" ];
        lualine_c = [ "filename" ];
        lualine_x = [ "filetype" ];
        lualine_y = [ "progress" ];
        lualine_z = [ ''" " .. os.date("%R")'' ];
      };
    };

    comment-nvim.enable = true;

    tmux-navigator.enable = true;
    indent-blankline.enable = true;
    ts-autotag.enable = true;

    # noice.enable = true;

    notify.enable = true;
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
      extraConfig = {
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
