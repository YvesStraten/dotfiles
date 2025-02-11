{
	# Credits to redyf: `https://github.com/redyf/Neve/blob/1038e7aa676dc11e756f2c6227082fab7fc32945/config/lsp/conform.nix#L4`
  plugins.conform-nvim = {
    enable = true;
    notifyOnError = true;
    formattersByFt = {
      html = [["prettierd" "prettier"]];
      css = [["prettierd" "prettier"]];
      javascript = [["prettierd" "prettier"]];
      javascriptreact = [["prettierd" "prettier"]];
      typescript = [["prettierd" "prettier"]];
      typescriptreact = [["prettierd" "prettier"]];
      java = ["google-java-format"];
      python = ["black"];
      lua = ["stylua"];
      nix = ["nixfmt"];
      markdown = [["prettierd" "prettier"]];
      rust = ["rustfmt"];
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>uf";
      action = ":FormatToggle<CR>";
      options = {
        desc = "Toggle Format";
        silent = true;
      };
    }
    {
      mode = "n";
      key = "<leader>cf";
      action = "<cmd>lua require('conform').format()<cr>";
      options = {
        silent = true;
        desc = "Format Buffer";
      };
    }

    {
      mode = "v";
      key = "<leader>cF";
      action = "<cmd>lua require('conform').format()<cr>";
      options = {
        silent = true;
        desc = "Format Lines";
      };
    }
  ];
}
