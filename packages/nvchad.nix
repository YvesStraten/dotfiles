{
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  name = "nvchad";
  src = fetchFromGitHub {
    owner = "Nvchad";
    repo = "Nvchad";
    rev = "13e9b0f458c3e5e95ac05a10822f26dbb1aa03cb";
    sha256 = "06k21p4w856ms405yg6qypj2sxh8jd4f606cr318qdpgrrb98n3y";
  };

  dontUnpack = true;

  installPhase = ''
    mkdir -p $out
    cp -R $src/* $out
  '';
}
