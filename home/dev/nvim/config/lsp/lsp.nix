{
  pkgs,
  lib,
  ...
}: {
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
        rust-analyzer = {
          enable = true;
          installRustc = false;
          installCargo = false;
        };

        nixd.enable = true;
      };
    };
    lsp-lines.enable = true;
    lspsaga.enable = true;
    nvim-jdtls = {
      enable = true;
      data = "~/.cache/jdtls/workspace";
      cmd = [
        "${lib.getExe pkgs.jdt-language-server}"
      ];
    };
  };
}
