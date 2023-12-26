{ stdenv, fetchurl, undmg}: 
stdenv.mkDerivation rec {
name = "Skim";
version = "1.6.21";
src = fetchurl {
url = "https://udomain.dl.sourceforge.net/project/skim-app/Skim/Skim-${version}/Skim-${version}.dmg";
};

sourceRoot = ".";
phases = ["unpackPhase" "installPhase"];
buildInputs = [ undmg ];

installPhase = ''
mkdir -p $out 
cp -R $src/* $out
'';
}

