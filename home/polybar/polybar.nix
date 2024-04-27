{ ... }:

{
  hm.services.polybar = {
    enable = true;
    config = ./config.ini;
    script = "${builtins.readFile ./launch.sh}";
  };
}
