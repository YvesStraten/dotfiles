{
  stdenv,
  fetchzip,
}:
stdenv.mkDerivation rec {
  version = "0.0.4";
  pname = "win32yank";

  src = fetchzip {
    url = "https://github.com/equalsraf/win32yank/releases/download/v${version}/win32yank-x64.zip";
    sha256 = "1jzb2zabx777dpjn8bh94biakzch2ybw9bxs0sbhf67i84xxqi2n";
    stripRoot = false;
  };

  installPhase = ''
    mkdir -p $out/bin
    cp * $out/bin
    chmod +x $out/bin/*
  '';
}
