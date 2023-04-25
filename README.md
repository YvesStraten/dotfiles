# dotfiles
Just a repository with my configs

The hyprland dotfiles are from [ChrisTitus](https://github.com/ChrisTitusTech/hyprland-titus) which I modified for my own use.
The zathura colorscheme is from [Catpuccin](https://github.com/catppuccin/zathura).

This also uses [Home manager](https://github.com/nix-community/home-manager#installation) in combination with [Nix](https://nixos.org/download.html) package manager, to be able to use fish as default shell, add the shell to the /etc/shells file.

``` 
chsh -s /home/youruser/.nix-profile/bin/fish
```

To restore your system from package list (assuming you use paru)
```
paru -S - < ~/Git-repos/dotfiles/pkglist/pkglist.txt
```

Make a new backup from packagelist

```
rm -r ~/Git-repos/dotfiles/pkglist/pkglist.txt
```

```
pacman -Qqe > ~/Git-repos/dotfiles/pkglist/pkglist.txt
```
