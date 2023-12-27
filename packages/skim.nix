{ stdenv
, lib
, undmg
, fetchurl
,
}:
stdenv.mkDerivation rec {
  name = "Skim-${version}";

  src = fetchurl {
    name = "Skim-${version}.dmg";
    url = "mirror://sourceforge/project/skim-app/Skim/Skim-${version}/Skim-${version}.dmg";
    sha256 = "0SeS41ExCZfGmjiK2KvvdFpP7IVzm7xHKCaKmEUYQAY=";
  };

  nativeBuildInputs = [ undmg ];
  version = "1.6.21";

  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];

  installPhase = ''
    mkdir -p $out/Applications
    cp -R *.app $out/Applications
  '';

  meta = with lib; {
    description = "Skim is a PDF reader and note-taker for OS X";
    homepage = "https://skim-app.sourceforge.io/";
    license = licenses.bsd0;
    mainProgram = "Skim.app";
    maintainers = with maintainers; [ YvesStraten ];
    platforms = [ "x86_64-darwin" "aarch64-darwin" ];
  };
}
