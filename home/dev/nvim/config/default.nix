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
      smartindent = true;

      hlsearch = false;
      incsearch = true;
      signcolumn = "yes";
    };

    globals = {
      tex_conceal = "abdgms";
      tex_superscripts = "[0-9a-zA-W.,:;+-<>/()=]";
      tex_subscripts = "[0-9aehijklmnoprstuvx,+-/().]";
      tex_conceal_frac = 1;
    };

    extraConfigLuaPre = ''
      luasnip = require("luasnip")
      vim.opt.undodir = os.getenv('HOME') .. '/.vim/undodir';
      vim.opt.backupdir = os.getenv('HOME') .. '/.vim/backup';

      -- Allows for cutting to keep stuff in the clip
      vim.keymap.set("x", "<leader>p", '"_dP')

      vim.keymap.set("n", "<leader>y", '"+y')
      vim.keymap.set("v", "<leader>y", '"+y')
      vim.keymap.set("n", "<leader>Y", '"+Y')
    '';
  };
}
