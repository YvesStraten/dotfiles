{ email, user, pkgs, ... }: {
  accounts.email.accounts = {
    ${user} = {
      address = email;
      primary = true;
    };
  };

  programs.thunderbird = if pkgs.stdenv.isLinux then {
    enable = true;
    profiles.${user} = { isDefault = true; };
  } else {};
}
