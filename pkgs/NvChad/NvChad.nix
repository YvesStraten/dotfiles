{
  lib,
  stdenv,
  pkgs,
}: let
  custom = ../home/nvim/custom;
in
  stdenv.mkDerivation {
    pname = "NvChad";
    version = "2.0.0";
    src = pkgs.fetchFromGithub {
      owner = "NvChad";
      repo = "NvChad";
      rev = "refs/heads/v2.0";
      sha256 = "sha256-u5zF9UuNt+ndeWMCpKbebYYW8TR+nLeBvcMy3blZiQw=";
    };

    installPhase = ''
      mkdir $out
      cp -r * "$out/"
      mkdir -p "$out/lua/custom"
      cp -r ${custom}/* "$out/lua/custom"
      touch "$out/lazy.lock.json"
      chmod -R 0777 "$out/lazy-lock.json"
    '';

    meta = with lib; {
      description = "NvChad";
      homepage = "https://github.com/NvChad/NvChad";
      maintainers = [maintainers.rayandrew];
      license = licenses.gpl3;
    };
  }
