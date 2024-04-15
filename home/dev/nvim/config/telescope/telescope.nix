{
  plugins.telescope = {
    enable = true;
    extensions = { fzf-native.enable = true; };
    keymaps = {
      "<leader>ff" = {
        action = "find_files";
        options = {
          desc = "Telescope find files";
        };
      };
      "<leader>fg" = {
        action = "live_grep";
        options = {
          desc = "Live grep files";
        };
      };
    };
  };
}
