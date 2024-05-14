{
  plugins = {
    none-ls = {
      enable = true;
      sources = {
        formatting = {
          alejandra.enable = true;
          stylua.enable = true;
          prettier = {
            enable = true;
            disableTsServerFormatter = true;
          };
        };
      };
    };
    lsp-format.enable = true;
  };
}
