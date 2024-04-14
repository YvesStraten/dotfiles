{ pkgs, ...}:{
programs.zathura = {
enable = true;
options = {
adjust-open = "best-fit";
smooth-scroll = "true";
};
};
}
