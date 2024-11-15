{...}: {
  globals.mapleader = " ";
  keymaps = [
    {
      mode = "v";
      key = "J";
      action = ":m '>+1<CR>gv=gv";
      options.silent = true;
    }

    {
      mode = "v";
      key = "K";
      action = ":m '<-2<CR>gv=gv";
      options.silent = true;
    }

    {
      key = "<C-d>";
      action = "<C-d>zz";
      options.silent = true;
    }

    {
      key = "<C-u>";
      action = "<C-u>zz";
      options.silent = true;
    }

    {
      key = "<esc>";
      mode = "n";
      action = ":nohlsearch<CR>";
      options.silent = true;
    }

    # {
    #   mode = "v";
    #   key = "<esc>";
    #   action = ":nohlsearch<CR>";
    #   options.silent = true;
    # }

    {
      key = "<tab>";
      options.silent = true;
      action = ":BufferNext<CR>";
    }

    {
      key = "<S-tab>";
      action = ":BufferPrevious<CR>";
      options.silent = true;
    }

    {
      key = "<leader>toc";
      action = ":VimtexTocToggle<CR>";
      options.desc = "Toggle TOC";
    }

    {
      key = "<leader>ll";
      action = ":VimtexCompile<CR>";
      options.desc = "Compile latex document";
    }

    {
      key = "<leader>x";
      action = ":BufferClose<CR>";
      options.desc = "Close Buffer";
      options.silent = true;
    }

    {
      key = "<C-n>";
      action = ":Neotree toggle<CR>";
      options.silent = true;
    }

    {
      key = "<leader>sp";
      action = ":split<CR>";
      options.desc = "Split horizontally";
      options.silent = true;
    }

    {
      key = "<leader>vp";
      action = ":vsplit<CR>";
      options.desc = "Split vertically";
      options.silent = true;
    }

    {
      key = "<leader>lr";
      action = ":Lspsaga rename<CR>";
      options.desc = "Rename definition";
      options.silent = true;
    }

    {
      key = "gd";
      action = ":Lspsaga goto_definition<CR>";
      options.desc = "Goto definition";
      options.silent = true;
    }

    {
      key = "<leader>gd";
      action = ":Lspsaga peek_definition<CR>";
      options.desc = "Peek definition";
      options.silent = true;
    }

    {
      key = "<leader>la";
      action = ":Lspsaga code_action<CR>";
      options.desc = "Code action";
      options.silent = true;
    }

    {
      key = "<leader>lp";
      action = ":Lspsaga preview_definition<CR>";
      options.desc = "Previews definition";
      options.silent = true;
    }

    {
      key = "<leader>gg";
      action = ":LazyGit<cr>";
      options.silent = true;
    }

    {
      key = "<leader>sh";
      action = ":split<cr>";
      options.silent = true;
    }

    {
      key = "<leader>sv";
      action = ":vsplit<cr>";
      options.silent = true;
    }

    {
      key = "n";
      action = "nzzzv";
      options.silent = true;
    }

    {
      key = "N";
      action = "Nzzzv";
      options.silent = true;
    }
  ];
}
