{ stdenv
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
}
