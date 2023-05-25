{ pkgs, ...}:
{
  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override { fonts = [
      "JetBrainsMono"
    ];})
      
    #themes here
  ];

  gtk = {
    enable = true;
    gtk3 = {
      bookmarks = [
          
      ];
    };
  };
}
