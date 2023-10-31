{
  stdenv,
  fetchFromGitHub,
  fetchurl
}:
stdenv.mkDerivation rec {
  name = "sddm-sugar-dark";
  src =
    fetchFromGitHub
    {
      owner = "MarianArlt";
      repo = "sddm-sugar-dark";
      rev = "ceb2c455663429be03ba62d9f898c571650ef7fe";
      sha256 = "0153z1kylbhc9d12nxy9vpn0spxgrhgy36wy37pk6ysq7akaqlvy";
    };

  background = fetchurl {
    url = "https://gitlab.com/exorcist365/wallpapers/-/raw/master/gruvbox/devilslaptop.jpg?inline=false";
    sha256 = "sha256-cDol8++eTua1T8ySDMipE8GoRdaZz8Cnpj/ume99huc=";
  }; 

  installPhase = ''
    mkdir -p $out
    cp -R ./* $out/
    cd $out/
    rm Background.jpg
    cp ${background} $out/Background.jpg
  '';
}
