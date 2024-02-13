{ stdenv
, fetchzip
,
}:
stdenv.mkDerivation rec {
  version = "6.64.0";
  pname = "AltTab";
  src =
    fetchzip
      {
        url = "https://github.com/lwouis/alt-tab-macos/releases/download/v${version}/AltTab-${version}.zip";
        sha256 = "d0BicCr/OL+8EGhiDQ82Aj0dKKI2oQILPqHHmWGQ28Y=";
      };

  installPhase = ''
    mkdir -p $out/Applications/AltTab.app
    cp -R Contents $out/Applications/AltTab.app
  '';
}
