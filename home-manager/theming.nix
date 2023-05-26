{ pkgs, ...}:
{
  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override { fonts = [
      "JetBrainsMono"
    ];})
      
    #themes here
    (catppuccin-gtk.override {
        accents = [ "pink" ];
        variant = "mocha";
      })

    whitesur-icon-theme 
  ];
}
