{ pkgs, ...}:
{
  home.packages = with pkgs; [
    #fonts
    (nerdfonts.override { fonts = [
      "JetBrainsMono"
    ];})
      
    #themes here

    whitesur-icon-theme 
  ];
}
