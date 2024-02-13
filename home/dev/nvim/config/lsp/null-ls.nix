{
  plugins = {
    none-ls = {
      enable = true;
      sources = {
        formatting = {
          alejandra.enable = true;
          stylua.enable = true;
          prettier.enable = true;
        };
      };
    };
    lsp-format.enable = true;
  };
}
