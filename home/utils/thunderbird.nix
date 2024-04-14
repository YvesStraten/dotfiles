{ email, user, ... }: {
  accounts.email.accounts = {
    ${user} = {
      address = email;
      primary = true;
    };
  };

  programs.thunderbird = {
    enable = true;
    profiles.${user} = { isDefault = true; };
  };
}
