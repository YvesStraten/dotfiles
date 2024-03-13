# dotfiles
Just a repository with my configurations

To clone do 
```
git clone https://github.com/ItsMindstorm/dotfiles
```

The hyprland dotfiles are from [ChrisTitus](https://github.com/ChrisTitusTech/hyprland-titus) which I modified for my own use.
The zathura colorscheme is from [Catpuccin](https://github.com/catppuccin/zathura). Meanwhile the polybar themes are from [Polybar themes](https://github.com/adi1090x/polybar-themes) 

I use nixos flakes to manage my system and home manager. To rebuild the system (NixOS) from scratch, you do:
```
sudo nixos-rebuild switch --extra-experimental-features "nix-command flakes" --flake <flake-location>#profile
```

To apply the home manager config, use,
```
home-manager switch --flake <flake-location>
```

Meanwhile on non-NixOS, clone the repository, install the [nix ](https://nixos.org/download.html) package manager, then run: 
```shell 
nix run . --extra-experimental-features "nix-command flakes" -- switch --flake .
```
