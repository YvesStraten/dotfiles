{ stdenv
, p7zip
, fetchurl
,
}:
stdenv.mkDerivation {
  name = "Shina-fox";
  src = fetchurl {
    url = "https://github.com/Shina-SG/Shina-Fox/releases/download/release/Shina.Fox.0.1.-.Frieren.Edition.7z";
    sha256 = "uLQjluSuz8iXO9M0AQE4N1C6qHv9wvO8Dv4TjweLuRw=";
  };

  sourceRoot = ".";

  buildInputs = [ p7zip ];

  installPhase = ''
    mkdir -p $out
       7z x $src .
          cp -R * $out
  '';
}
