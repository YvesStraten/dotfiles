{
  plugins = {
    # luasnip = {
    #   enable = true;
    #   fromVscode = [{ }];
    # };
    lspkind = {
      enable = true;
      cmp.enable = true;
    };

    nvim-cmp = {
      enable = true;
      autoEnableSources = true;
      snippet.expand = "ultisnips";

      sources = [
        {name = "nvim_lsp";}
        {name = "ultisnips";}
        {name = "nvim_lsp_document_symbol";}
        {name = "nvim_lsp_signature_help";}
        # { name = "luasnip"; }
        {name = "buffer";}
        {name = "path";}
      ];

      window = {
        completion = {
          border = "rounded";
          scrollbar = true;
        };
        documentation = {
          border = "rounded";
        };
      };
      formatting = {
        fields = ["abbr" "kind" "menu"];
      };

      mapping = {
        "<Tab>" = {
          modes = ["i" "s"];
          action = ''
            function(fallback)
              cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
            end
          '';
        };
        "<S-Tab>" = {
          modes = ["i" "s"];
          action = ''
            function(fallback)
               cmp_ultisnips_mappings.jump_backwards(fallback)
             end
          '';
        };
        "<CR>" = "cmp.mapping.confirm({ select = true })";
        "<C-Space>" = "cmp.mapping.complete()";
        "<C-e>" = "cmp.mapping.abort()";
        "<Up>" = "cmp.mapping.select_prev_item()";
        "<Down>" = "cmp.mapping.select_next_item()";
        "<C-p>" = "cmp.mapping.select_prev_item()";
        "<C-n>" = "cmp.mapping.select_next_item()";
        "<C-u>" = "cmp.mapping.scroll_docs(-4)";
        "<C-d>" = "cmp.mapping.scroll_docs(4)";
      };
    };
  };
}
