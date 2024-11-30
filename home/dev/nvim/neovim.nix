{
  pkgs,
  self,
  ...
}:
{
  home.packages =
    [
      pkgs.fd
      pkgs.ripgrep
      pkgs.google-java-format
      pkgs.nixfmt-rfc-style
      (pkgs.aspellWithDicts (
        ds: with ds; [
          en
          id
          en-computers
          en-science
        ]
      ))
    ]
    ++ (
      if pkgs.stdenv.isLinux then
        [
          self.packages."x86_64-linux".nvim
        ]
      else
        [
          self.packages."aarch64-darwin".nvim
        ]
    );

  home.sessionVariables = {
    EDITOR = "nvim";
  };
}
