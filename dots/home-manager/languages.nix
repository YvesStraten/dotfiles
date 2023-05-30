{ pkgs, ...}:
{
    home.packages = with pkgs; [
      nodejs
      yarn
      gcc
      omnisharp-roslyn
      python39
      python39Packages.pillow
    ];
  }
