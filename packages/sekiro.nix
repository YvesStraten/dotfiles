{
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation
{
  name = "sekiro-grub";
  src =
    fetchFromGitHub
    {
      owner = "semimqmo";
      repo = "sekiro_grub_theme";
      rev = "main";
      sha256 = "02gdihkd2w33qy86vs8g0pfljp919ah9c13cj4bh9fvvzm5zjfn1";
    };

  installPhase = ''
    mkdir -p $out
    cp -R ./Sekiro/* $out
  '';
}
