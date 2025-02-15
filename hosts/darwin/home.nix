{
  pkgs,
  ...
}:
{
  services.alt-tab.enable = true;

  home = {
    sessionPath = [
      "$HOME/.config/emacs/bin"
      "/Applications/XAMPP/bin"
    ];
  };

  custom.general.extraPackages = with pkgs; [
    iina
    skimpdf
    xcbuild
  ];
}
