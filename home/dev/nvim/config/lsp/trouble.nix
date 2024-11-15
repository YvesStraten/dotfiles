{ pkgs, ...}:{
	plugins.trouble.enable = true;

	keymaps = [
		{
      key = "<leader>xx";
      action = "<cmd>Trouble diagnostics toggle<cr>";
      options = {
				desc = "Diagnostics (Trouble)";
				silent = true;
			};
    }

    {
      key = "<leader>xX";
      action = "<cmd>Trouble diagnostics toggle filter.buf=0<cr>";
      options = {
				desc = "Buffer Diagnostics (Trouble)";
				silent = true;
			};
    }

    {
      key = "<leader>cs";
      action = "<cmd>Trouble symbols toggle focus=false<cr>";
      options = {
				desc = "Symbols (Trouble)";
				silent = true;
    	};
		}
	];
}
