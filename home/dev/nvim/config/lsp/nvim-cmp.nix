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
          max_view_entries = 30;
        };

        window = {
          formatting = {fields = ["abbr" "kind" "menu"];};
          completion = {
            border = "rounded";
            scrollbar = true;
          };
          documentation = {border = "rounded";};
        };
        mapping = {
          "<Tab>" = ''
            function(fallback)
              cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
            end
          '';
          "<S-Tab>" = ''
            function(fallback)
               cmp_ultisnips_mappings.jump_backwards(fallback)
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
