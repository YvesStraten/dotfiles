{ pkgs, ...}:
{
  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override { fonts = [
      "JetBrainsMono"
    ];})
      
    #themes here
    catppuccin-gtk
  ];

  gtk = {
    enable = true;
    gtk3 = {
      bookmarks = [
          
      ];
    };
  };
}
