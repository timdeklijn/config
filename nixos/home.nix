{ config, pkgs, ... }:

{
  home.username = "tim";
  home.homeDirectory = "/home/tim";

  # link the configuration file in current directory to the specified location in home directory
  # home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  home.file = { 
      # ".zshrc".source = config.lib.file.mkOutOfStoreSymlink ../zsh/.zshrc;
      ".config/i3/config".source = config.lib.file.mkOutOfStoreSymlink ../i3/.config/i3/config;
      ".config/i3status-rust/config.toml".source = config.lib.file.mkOutOfStoreSymlink ../i3status-rust/.config/i3status-rust/config.toml;
      ".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink ../tmux/.tmux.conf;
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
    zsh
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
    kitty
    nitrogen  # desktop wallpaper
    feh       # image viewer
    vlc       # video
    tmux 
    delta
    
    # programming + engineering tools
    gcc
    azure-cli 
    azure-functions-core-tools
    nodejs  # required for azurite

    # language servers
    gopls
    zls
    pyright
    rust-analyzer
    lua-language-server
    terraform-ls
    vscode-langservers-extracted
  ];

  # Config programs ============================================================
  programs.starship.enable = true;
  programs.direnv.enable = true;

  programs.zsh = {
    enable = true;
    shellAliases = {
      ls = "eza --icons";
      ll = "eza -l --icons";
      update = "sudo nixos-rebuild switch --impure";  # TODO: make this 'pure'
      vim = "nvim";
      k = "kubectl";
      gd = "git diff";
      gp = "git push";
      gc = "git commit";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "zsh-users/zsh-autosuggestions"; }
      ];
    };
  };

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
