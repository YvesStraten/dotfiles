{
  pkgs,
  self,
  ...
}: {
  home.packages =
    [
      pkgs.fd
      pkgs.ripgrep
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

#  home.sessionVariables = {
#    EDITOR = "nvim";
#  };
}
