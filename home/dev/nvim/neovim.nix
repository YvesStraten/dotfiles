{ pkgs
, config
, inputs
, self
, ...
}: {
  home.packages = with pkgs;
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

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}
