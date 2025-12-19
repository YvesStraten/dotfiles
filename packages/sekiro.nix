{
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  name = "sekiro-grub";
  src = fetchFromGitHub {
    owner = "semimqmo";
    repo = "sekiro_grub_theme";
    rev = "main";
    hash = "sha256-uXwDjb0+ViQvdesG5gefC5zFAiFs/FfDfeI5t7vP+Qc=";
  };

  installPhase = ''
    mkdir -p $out
    cp -R ./Sekiro/* $out
  '';
}
