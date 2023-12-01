# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  # All packages
  nixpkgs.config.allowUnfree = true; 

  # TODO: remove after installing for real
  virtualisation.virtualbox.guest.enable = true;

  networking.hostName = "kamelot"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # Enable networking -> errors with wireless.enable
  # networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "nl_NL.UTF-8";
    LC_IDENTIFICATION = "nl_NL.UTF-8";
    LC_MEASUREMENT = "nl_NL.UTF-8";
    LC_MONETARY = "nl_NL.UTF-8";
    LC_NAME = "nl_NL.UTF-8";
    LC_NUMERIC = "nl_NL.UTF-8";
    LC_PAPER = "nl_NL.UTF-8";
    LC_TELEPHONE = "nl_NL.UTF-8";
    LC_TIME = "nl_NL.UTF-8";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # TODO: we needs to install nvidia drivers once we install
  #       on the new system

  environment.pathsToLink = [ "/libexec" ];

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";

    desktopManager = {
      xterm.enable = false;
    };

    displayManager.lightdm.enable = true;

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        rofi
        i3lock
        i3status-rust
      ];
    };
  };

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # For laptop at some point?
  services.xserver.libinput.enable = true;

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    virtualbox # TODO: remove when installing for real
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tim = {
    isNormalUser = true;
    home = "/home/tim";
    description = "tim";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      neofetch # show what you are running
      git # version control
      curl # network calls
      wget # fetch from the internet
      bat # better less
      ripgrep # recursively searches directories for a regex pattern
      jq # A lightweight and flexible command-line JSON processor
      exa # A modern replacement for ‘ls’
      fzf # A command-line fuzzy finder
      kitty # terminal emulator
      tmux # terminal multiplexer
      stow # move dotfiles to locations
      gnumake  # makefiles
      zsh # shell
      starship # prompt
      direnv # environment
      vim # terminal editor
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
      
      # programming + engineering tools
      python3
      go
      gcc
      zig
      cargo
      rustc
      terraform
      azure-cli
      azure-functions-core-tools
      nodejs  # required for azurite
      
      (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    ];
  };

  # how to enable zsh?
  programs.zsh.enable = true;
  fonts.fontconfig.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # clean up old snapshots of the system after a week
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  system.stateVersion = "23.05"; # Did you read the comment?
}
