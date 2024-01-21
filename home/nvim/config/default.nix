{
  imports = [
    ./plugins.nix
    ./keymaps/keymaps.nix
  ];

  config = {
    options = {
      number = true;
      relativenumber = true;
      conceallevel = 2;
      tabstop = 2;
      shiftwidth = 2;
      scrolloff = 8;
      updatetime = 50;
      termguicolors = true;

      hlsearch = false;
      incsearch = true;
    };

    globals = {
      tex_conceal = "abdgms";
      tex_superscripts = "[0-9a-zA-W.,:;+-<>/()=]";
      tex_subscripts = "[0-9aehijklmnoprstuvx,+-/().]";
      tex_conceal_frac = 1;
    };

    extraConfigLuaPre = ''

      local cmp_ultisnips_mappings = require("cmp_nvim_ultisnips.mappings")

    '';
  };
}
