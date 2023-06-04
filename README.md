# dotfiles
Just a repository with my configs

To clone do 
```
git clone --recurse-submodules -j8 https://github.com/ItsMindstorm/dotfiles
```

The hyprland dotfiles are from [ChrisTitus](https://github.com/ChrisTitusTech/hyprland-titus) which I modified for my own use.
The zathura colorscheme is from [Catpuccin](https://github.com/catppuccin/zathura).

I use nixos flakes to manage my system and home manager. To rebuild the system, you do:
```
sudo nixos-rebuild switch --flake <flake-location>#profile
```

To make home manager again, use,
```
home-manager switch --flake <flake-location>
```
