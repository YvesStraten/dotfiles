{
  plugins = {
    lsp = {
      enable = true;
      servers = {
        tsserver.enable = true;
        tailwindcss.enable = true;
        clangd.enable = true;
        lua-ls.enable = true;
        rnix-lsp.enable = true;
        texlab.enable = true;

        pyright.enable = true;
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