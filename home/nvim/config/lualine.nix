{ pkgs, ... }: {
  plugins.lualine = {
    enable = true;
    globalstatus = true;
    sections = {
      lualine_a = [
        {
          name.__raw = ''
            function()
            	local mode_map = {
            		n = "Normal",
            		i = "Insert",
            		v = "Visual",
            		V = "V-line",
            		c = "Command",
            		R = "Replace",
            		s = "Select",
            		S = "S-line",
            		[""] = "Empty",
            		t = "Terminal",
            	}

            	local mode = vim.fn.mode()
            	return mode_map[mode] or mode
            end
          '';
        }
      ];
      lualine_b = [ "branch" "diff" "diagnostics" ];
      lualine_c = [ "filename" ];
      lualine_x = [ "filetype" ];
      lualine_y = [ "progress" ];
      lualine_z = [ ''" " .. os.date("%R")'' ];
    };
  };
}
