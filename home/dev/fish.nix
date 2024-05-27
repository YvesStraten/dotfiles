{ pkgs, ... }: {
  programs.fish = {
    enable = true;
    shellAliases = {
      ga = "git add";
      gc = "git commit";
      ".." = "cd ..";
      "..." = "cd ../..";
    };

    shellInit = ''
      set -g fish_greeting
    '';

    plugins = with pkgs.fishPlugins; [
      {
        name = "tide";
        src = tide.src;
      }

      {
        name = "sponge";
        src = sponge.src;
      }
    ] ++ (if pkgs.stdenv.isDarwin then [{
      name = "nix-env.fish";
      src = pkgs.fetchFromGitHub {
        owner = "lilyball";
        repo = "nix-env.fish";
        rev = "master";
        sha256 = "RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
      };
    }] else
      []);
  };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      add_newline = false;
      hostname = {
        ssh_only = false;
        disabled = false;
      };
    };
  };
}
