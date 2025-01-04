{
  plugins.telescope = {
    enable = true;

    extensions = {
      fzf-native.enable = true;
    };
    settings = {
      defaults = {
        path_display.__raw = ''
          {
            "truncate"
          }
        '';
        layout_strategy = "vertical";
        layout_config = {
          horizontal = {
            preview_cutoff = 0;
          };

          vertical = {
            preview_cutoff = 0;
          };
        };
        mappings = {
          i = {
            "<esc>" = {
              __raw = ''
                function(...)
                  return require("telescope.actions").close(...)
                end'';
            };
          };
        };
      };
    };

    keymaps = {
      "<leader><leader>" = {
        action = "find_files";
        options = {
          desc = "Telescope find files";
        };
      };
      "<leader>/" = {
        action = "live_grep";
        options = {
          desc = "Live grep files";
        };
      };

      "<leader>m" = {
        action = "man_pages";
        options.desc = "Man pages grep";
      };

      # "<leader>fh" = {
      #   action = "which_key";
      #   options = {
      #     desc = "Which key";
      #   };
      # };
    };
  };
}
