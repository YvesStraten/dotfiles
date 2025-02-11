{
  pkgs,
  lib,
  ...
}: {
  plugins = {
    rustaceanvim = {
      enable = true;
    };
    lsp = {
      enable = true;
      inlayHints = true;
      servers = {
	hls.enable = true;
			nil-ls.enable = true;
        tsserver.enable = true;
        tailwindcss.enable = true;
        svelte.enable = true;
        clangd.enable = true;
        lua-ls.enable = true;
        texlab.enable = true;

        pyright.enable = true;
        #  rust-analyzer = {
        #    enable = true;
        #    installRustc = false;
        #    installCargo = false;
        #    settings = {
        #      inlayHints = {
        # enable = true;
        #        bindingModeHints.enable = true;
        #        chainingHints.enable = true;
        #        closureCaptureHints.enable = true;
        #      };
        #    };
        #  };

        # nixd.enable = true;
      };
    };
    # lsp-lines.enable = true;
    nvim-jdtls = {
      enable = true;
      data = "~/.cache/jdtls/workspace";
      cmd = [
        "${lib.getExe pkgs.jdt-language-server}"
      ];
    };
  };
}
