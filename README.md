# dotfiles
Just a repository with my configs

To clone do 
```
git clone --recurse-submodules -j8 https://github.com/ItsMindstorm/dotfiles
```

The hyprland dotfiles are from [ChrisTitus](https://github.com/ChrisTitusTech/hyprland-titus) which I modified for my own use.
The zathura colorscheme is from [Catpuccin](https://github.com/catppuccin/zathura). Meanwhile the polybar themse are from [Polybar themes](https://github.com/adi1090x/polybar-themes) 

I use nixos flakes to manage my system and home manager. To rebuild the system (NixOS), you do:
```
sudo nixos-rebuild switch --flake <flake-location>#profile
```

To apply the home manager config, use,
```
home-manager switch --flake <flake-location>
```

Meanwhile on non-NixOS, clone the repository, install the [nix ](https://nixos.org/download.html) package manager, allow flakes by using the command: 
```shell 
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

Then run: 
```shell 
nix run . -- switch --flake .
```
