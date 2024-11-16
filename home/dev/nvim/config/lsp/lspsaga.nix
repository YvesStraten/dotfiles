{ ... }: {
  plugins.lspsaga.enable = true;
	keymaps = [
    {
      key = "<leader>lr";
      action = ":Lspsaga rename<CR>";
      options.desc = "Rename definition";
      options.silent = true;
    }
		
    {
      key = "<leader>lf";
      action = ":Lspsaga finder<CR>";
      options.desc = "Find defintions and implementations";
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
	];
}
