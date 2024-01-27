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
        sha256 = "DLQ/szVeEMdpWQ9JLv0pEJ92Z1glUPV62OKBzIJF/tA=";
      };

  installPhase = ''
    mkdir -p $out/Applications/AltTab.app
    cp -R Contents $out/Applications/AltTab.app
  '';
}
