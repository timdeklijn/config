{ config, pkgs, ... }:

{
  home.username = "tim";
  home.homeDirectory = "/home/tim";

  # link the configuration file in current directory to the specified location in home directory
  # home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  home.file = { 
      ".zshrc".source = config.lib.file.mkOutOfStoreSymlink ../zsh/.zshrc;
      ".config/i3/config".source = config.lib.file.mkOutOfStoreSymlink ../i3/.config/i3/config;
      ".config/i3status-rust/config.toml".source = config.lib.file.mkOutOfStoreSymlink ../i3status-rust/.config/i3status-rust/config.toml;
      ".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink ../tmux/.tmux.conf;
  };
  home.file.".config/nvim" = {
    source = config.lib.file.mkOutOfStoreSymlink ../neovim/.config/nvim;
    recursive = true;   # link recursively
    executable = false;  # make all files executable
  };

  home.file.".config/kitty" = {
    source = ../kitty/.config/kitty;
    recursive = true;   # link recursively
    executable = true;  # make all files executable
  };

  # set cursor size and dpi for 4k monitor
  xresources.properties = {
    "Xcursor.size" = 16;
    "Xft.dpi" = 172;
  };

  # basic configuration of git, please change to your own
  programs.git = {
    enable = true;
    userName = "Tim de Klijn";
    userEmail = "timdeklijn@gmail.com";
  };

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    pavucontrol # audio control application
    neofetch # show what you are running
    wget # fetch from the internet
    bat # better less
    ripgrep # recursively searches directories for a regex pattern
    jq # A lightweight and flexible command-line JSON processor
    eza # A modern replacement for ‘ls’
    fzf # A command-line fuzzy finder
    stow # move dotfiles to locations
    gnumake  # makefiles
    neovim # terminal editor
    glow # markdown previewer in terminal
    btop  # replacement of htop/nmon
    lsof # list open ports
    zip # archives
    kubectl # kubernetes

    # Desktop apps -> this is really specific for nixOS, not nix os Mac or something
    chromium  # browser, chrome based for teams
    slack     # communicate
    spotify   # music
    vscode    # code editor
    dropbox   # file sync
    nitrogen  # desktop wallpaper
    feh       # image viewer
    vlc       # video
    
    # programming + engineering tools
    python3
    gcc
    go
    cargo
    rustc
    terraform
    azure-cli 
    azure-functions-core-tools
    nodejs  # required for azurite
  ];

  # Config programs ============================================================
  programs.starship.enable = true;
  programs.zsh.enable = true;
  programs.kitty.enable = true;
  programs.direnv.enable = true;
  programs.tmux.enable = true;

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}
