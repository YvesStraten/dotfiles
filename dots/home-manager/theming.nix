{ pkgs, ...}:
{
  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override { fonts = [
      "JetBrainsMono"
    ];})
      
    #themes here

    whitesur-gtk-theme
    whitesur-icon-theme 
  ];
}
