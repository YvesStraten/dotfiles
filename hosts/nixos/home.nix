{pkgs, ...}: {
  custom = {
    fish.enable = true;
    hyprland.enable = true;
    kitty.enable = true;
    emacs.enable = true;
    tmux.enable = true;
    languages.enable = true;
    office.enable = true;
    theming.enable = true;

    general.extraPackages = with pkgs; [
      spotify
      obs-studio

      filezilla
      btop
      nautilus
      teams-for-linux

      ani-cli-rofi

      qpwgraph
    ];
  };

  systemd.user.services = {
    "Rclone-onedrive-mount" = {
      Unit = {
        Description = "Mount rclone";
      };

      Service = {
        Type = "notify";
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${config.home.homeDirectory}/Onedrive";
        ExecStart = "${pkgs.rclone}/bin/rclone mount Onedrive:Uni ${config.home.homeDirectory}/Onedrive/ --vfs-cache-mode full";
        ExecStop = "${pkgs.fuse}/bin/fusermount -u ${config.home.homeDirectory}/Onedrive";
      };

      Install.WantedBy = ["default.target"];
    };
  };
}
