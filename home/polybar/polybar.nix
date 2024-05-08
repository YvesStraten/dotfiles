{ inputs, pkgs, ... }:

{
  hm = {
    services.polybar = {
      enable = true;
      config = ./config.ini;
      script = "${builtins.readFile ./launch.sh}";
    };

    xdg.configFile."polybar/pipewire-simple.sh" = {
      source = let
        script = builtins.readFile
          "${inputs.polybar-scripts}/polybar-scripts/pipewire-simple/pipewire-simple.sh";
      in pkgs.writeShellScriptBin "pipewire-simple.sh" ''
        ${script}
      '';
    };
  };
}
