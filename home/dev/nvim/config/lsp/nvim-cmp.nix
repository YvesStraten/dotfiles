{pkgs, ...}: {
  plugins = {
    luasnip = {
      enable = true;
      extraConfig = {
        enable_autosnippets = true;
        store_selection_keys = "<Tab>";
      };
      fromVscode = [
        {
          lazyLoad = true;
          paths = "${pkgs.vimPlugins.friendly-snippets}";
        }
      ];
    };
    lspkind = {
      enable = true;
      cmp.enable = true;
    };

    cmp = {
      enable = true;
      autoEnableSources = true;

      settings = {
        snippet.expand = "function(args) require('luasnip').lsp_expand(args.body) end";
        sources = [
          {name = "nvim_lsp";}
          # {name = "ultisnips";}
          {name = "nvim_lsp_document_symbol";}
          {
            name = "nvim_lsp_signature_help";
          }
          {name = "luasnip";}
          {name = "buffer";}
          {name = "path";}
        ];

        performance = {
          max_view_entries = 15;
        };

        # window = {
        #   formatting = {fields = ["abbr" "kind" "menu"];};
        #   completion = {
        #     border = "rounded";
        #     scrollbar = true;
        #   };
        #   documentation = {border = "rounded";};
        # };
        mapping = {
          "<Tab>" = ''
                   function(fallback)
                         if cmp.visible() then
                           cmp.select_next_item()
                         elseif luasnip.expand_or_jumpable() then
                           luasnip.expand_or_jump()
                         else
                           fallback()
                         end
            end
          '';
          "<S-Tab>" = ''
            function(fallback)
                  if cmp.visible() then
                    cmp.select_prev_item()
                  elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                  else
                    fallback()
                  end
            end
          '';
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

    nvim-cmp = {};
  };
}
