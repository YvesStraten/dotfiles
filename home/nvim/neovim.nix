{
  pkgs,
  config,
  inputs,
  ...
}: {
  programs.neovim = {
    enable = true;
    defaultEditor = true;

    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    extraLuaConfig = ''
      ${builtins.readFile ./settings.lua}
    '';

    plugins = with pkgs.vimPlugins; [
      {
        plugin = kanagawa-nvim;
        config = "colorscheme kanagawa";
      }

      {
        plugin = nvim-notify;
        type = "lua";
        config = "${builtins.readFile ./configs/notify.lua}";
      }

      {
        plugin = nvim-lspconfig;
        type = "lua";
        config = "${builtins.readFile ./configs/lsp/lspconfig.lua}";
      }

      undotree

      {
        plugin = none-ls-nvim;
        type = "lua";
        config = "${builtins.readFile ./configs/lsp/none_ls.lua}";
      }

      plenary-nvim
      nvim-web-devicons
      lspsaga-nvim
      lspkind-nvim

      {
        plugin = nvim-autopairs;
        type = "lua";
        config = "require('nvim-autopairs').setup({})";
      }

      {
        plugin = indent-blankline-nvim;
        type = "lua";
        config = "require('ibl').setup({})";
      }

      vim-fugitive
      vim-tmux-navigator

      {
        plugin = neo-tree-nvim;
        type = "lua";
        config = "${builtins.readFile ./configs/neotree.lua}";
      }

      telescope-nvim
      {
        plugin = comment-nvim;
        type = "lua";
        config = "require('Comment').setup()";
      }
      barbar-nvim

      {
        plugin = lualine-nvim;
        type = "lua";
        config = "${builtins.readFile ./configs/lualine.lua}";
      }

      {
        plugin = toggleterm-nvim;
        type = "lua";
        config = "${builtins.readFile ./configs/toggleterm.lua}";
      }

      bullets-vim
      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = "${builtins.readFile ./configs/treesitter.lua}";
      }

      {
        plugin = nvim-cmp;
        type = "lua";
        config = "${builtins.readFile ./configs/cmp/cmp.lua}";
      }

      {
        plugin = vimtex;
        type = "lua";
        config = "${builtins.readFile ./configs/vimtex/vimtex.lua}";
      }

      {
        plugin = which-key-nvim;
        type = "lua";
        config = "${builtins.readFile ./configs/which-key/which-key.lua}";
      }

      cmp-path
      cmp-nvim-lsp
      cmp-buffer
      ultisnips
      vim-snippets
      {
        plugin = ouroboros;
        type = "lua";
        config = "vim.keymap.set('n', '<C-o>', ':Ouroboros<cr>')";
      }
      cmp-nvim-ultisnips

      {
        plugin = nvim-colorizer-lua;
        type = "lua";
        config = "${builtins.readFile ./configs/colorizer.lua}";
      }

      {
        plugin = presence-nvim;
        type = "lua";
        config = "require('presence').setup({})";
      }
    ];
  };

  home.packages = with pkgs; [
    python310Packages.pynvim
    ripgrep
    fzf
    languagetool

    # LSP servers
    texlab
    omnisharp-roslyn
    sumneko-lua-language-server
    stylua
    nodePackages_latest.prettier
    nodePackages_latest.vscode-html-languageserver-bin
    nodePackages_latest.typescript-language-server
    nodePackages_latest.eslint
    html-tidy
    rnix-lsp
    shellcheck
    nodePackages_latest.pyright
    cppcheck
    alejandra
    clang-tools
    # nixpkgs-fmt
    nixfmt

    # DAP protocols
    lldb
  ];

  services.syncthing = {
    enable = true;
  };
}
