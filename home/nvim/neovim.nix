{ pkgs
, config
, inputs
, self
, ...
}: {
  home.packages =
    [
      fd
      ripgrep
    ]
    ++ (
      if pkgs.stdenv.isLinux
      then [
        self.packages."x86_64-linux".nvim
      ]
      else [
        self.packages."aarch64-darwin".nvim
      ]
    );
}
