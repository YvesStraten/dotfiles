{
  stdenv,
  fetchFromGitHub,
  fetchurl,
}:
stdenv.mkDerivation rec {
  name = "sddm-sugar-dark";
  src = fetchFromGitHub {
    owner = "MarianArlt";
    repo = "sddm-sugar-dark";
    rev = "ceb2c455663429be03ba62d9f898c571650ef7fe";
    sha256 = "0153z1kylbhc9d12nxy9vpn0spxgrhgy36wy37pk6ysq7akaqlvy";
  };

  background = fetchurl {
    url = "https://gruvbox-wallpapers.pages.dev/wallpapers/anime/wall.jpg";
    sha256 = "1yyzpffr4a9iklswfvzz1k69bc0g3dbrz68qv4yfhcrrq3fl1phf";
  };

  installPhase = ''
    mkdir -p $out
    cp -R ./* $out/
    cd $out/
    rm Background.jpg
    cp ${background} $out/Background.jpg
  '';
}
