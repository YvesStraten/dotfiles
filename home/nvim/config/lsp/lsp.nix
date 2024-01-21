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
        java-language-server.enable = true;
      };
    };
    lsp-lines.enable = true;
    lspsaga.enable = true;
  };
}
