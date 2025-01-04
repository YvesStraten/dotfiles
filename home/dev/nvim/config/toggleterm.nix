{ ... }: {
  plugins.toggleterm = {
    enable = true;
    settings = {
      direction = "horizontal";
      hideNumbers = true;
      open_mapping = "[[<C-e>]]";
      floatOpts.border = "curved";
      size.__raw = ''
          function(term)
            if term.direction == "horizontal" then
              return 10
            elseif term.direction == "vertical" then
              return vim.o.columns * 0.4
            end
          end,
          '';
    };
  };

  keymaps = [ 
    {
      mode = "t";
      key = "<Esc>";
      action = "<C-\\><C-n>";
      options = {
        desc = "Exit terminal";
      };
    }
  ];
}
