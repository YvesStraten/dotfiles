{
  plugins = {
    gitsigns = {
      enable = true;
      settings.signs = {
        add.text = "+";
        change.text = "~";
        changedelete.text = "-";
        delete.text = "-";
        delete.showCount = true;
      };
    };
  };
}
