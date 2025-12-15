{
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  name = "whitesur-cursors";
  src = fetchFromGitHub {
    owner = "vinceliuice";
    repo = "WhiteSur-cursors";
    rev = "5c94e8c22de067282f4cf6d782afd7b75cdd08c8";
    sha256 = "03828f21sgcmpldbmqwpqbfvxrxy2zr9laipb27yy9kkfv8iwnq8";
  };
  installPhase = ''
    mkdir -p $out
    cp -R dist/* $out
  '';
}
