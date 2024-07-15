{ pkgs, gitUser, email, ... }: {
  programs.git = {
    enable = true;
    userName = gitUser;
    userEmail = email;

    lfs.enable = true;
  };
}
