{
  pkgs,
  lib,
  helpers,
  config,
  inputs,
  ...
}: let
  inherit (lib) mkOption types;
in
  helpers.neovim-plugin.mkNeovimPlugin config {
    name = "tiny-code-action";
    originalName = "tiny-code-action.nvim";
    defaultPackage = pkgs.vimPlugins.tiny-code-action;
    maintainers = ["yves"];

    settingsOptions = {
      backend =
        helpers.defaultNullOpts.mkEnumFirstDefault ["vim" "delta"]
        ''
          The backend used by tiny-code-action
        '';

      #  signs = mkOption {
      #    type = types.submodule {
      #      freeformType = with types; attrsOf anything;
      # options = {
      #
      # };
      #    };
      # };

      # settingsExample = {
      #   backend = "delta";
      # };
    };
  }
