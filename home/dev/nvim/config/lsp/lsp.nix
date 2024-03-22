{
  plugins = {
    lsp = {
      enable = true;
      servers = {
        tsserver.enable = true;
        tailwindcss.enable = true;
        clangd.enable = true;
        lua-ls.enable = true;
        texlab.enable = true;

        pyright.enable = true;
        rust-analyzer.enable = true;
      };
    };
    lsp-lines.enable = true;
    lspsaga.enable = true;
    nvim-jdtls = {
      enable = true;
      configuration = "/Users/yvess/.cache/jdtls/config";
      data = "/Users/yvess/.cache/jdtls/workspace";
    };
  };
}
