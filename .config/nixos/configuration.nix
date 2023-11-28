# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, callPackage, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    <home-manager/nixos>
  ];
  nixpkgs.overlays = [
    (final: prev: {
      unstable = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixpkgs-unstable.tar.gz") { };
    })
    (final: prev: {
         iosevka-normal = prev.unstable.iosevka.override {
           privateBuildPlan = ''
[buildPlans.iosevka-normal]
family = "Iosevka Normal"
spacing = "normal"
serifs = "sans"
no-cv-ss = true
export-glyph-names = false

[buildPlans.iosevka-normal.variants.design]
capital-d = "more-rounded-serifless"
capital-g = "toothless-rounded-serifless-hooked"
capital-r = "standing-open-serifless"
a = "single-storey-tailed"
b = "toothless-corner-serifless"
d = "toothless-corner-serifless"
g = "single-storey-earless-rounded"
i = "tailed-serifed"
l = "tailed-serifed"
m = "short-leg-serifless"
p = "earless-rounded-serifless"
q = "earless-rounded-straight-serifless"
t = "flat-hook"
u = "toothless-rounded-serifless"
y = "cursive-serifless"
zero = "slashed-split"
four = "closed"
seven = "bend-serifless"
eight = "two-circles"
nine = "open-contour"
paren = "large-contour"
brace = "curly-flat-boundary"
at = "fourfold"
percent = "rings-continuous-slash"
lig-ltgteq = "slanted"
lig-neq = "more-slanted"
lig-equal-chain = "without-notch"
lig-hyphen-chain = "without-notch"

[buildPlans.iosevka-normal.variants.italic]
capital-j = "descending-serifed"
capital-q = "detached-bend-tailed"
capital-v = "curly-serifless"
capital-w = "curly-serifless"
capital-y = "curly-serifless"
capital-z = "cursive"
h = "tailed-serifless"
k = "cursive-bottom-right-serifed"
m = "short-leg-tailed-serifless"
n = "tailed-serifless"
q = "earless-rounded-tailed-serifless"
t = "bent-hook-short-neck"
v = "cursive-serifed"
w = "cursive-serifless"
z = "cursive"
micro-sign = "toothed-bottom-right-serifed"
'';
          set = "normal";
         };
         iosevka-extended = prev.unstable.iosevka.override {
          privateBuildPlan = ''
[buildPlans.iosevka-extended]
family = "Iosevka Extended"
spacing = "normal"
serifs = "sans"
no-cv-ss = true
export-glyph-names = false

[buildPlans.iosevka-extended.variants.design]
capital-d = "more-rounded-serifless"
capital-g = "toothless-rounded-serifless-hooked"
capital-r = "standing-open-serifless"
a = "single-storey-tailed"
b = "toothless-corner-serifless"
d = "toothless-corner-serifless"
g = "single-storey-earless-rounded"
i = "tailed-serifed"
l = "tailed-serifed"
m = "short-leg-serifless"
p = "earless-rounded-serifless"
q = "earless-rounded-straight-serifless"
t = "flat-hook"
u = "toothless-rounded-serifless"
y = "cursive-serifless"
zero = "slashed-split"
four = "closed"
seven = "bend-serifless"
eight = "two-circles"
nine = "open-contour"
paren = "large-contour"
brace = "curly-flat-boundary"
at = "fourfold"
percent = "rings-continuous-slash"
lig-ltgteq = "slanted"
lig-neq = "more-slanted"
lig-equal-chain = "without-notch"
lig-hyphen-chain = "without-notch"

[buildPlans.iosevka-extended.variants.italic]
capital-j = "descending-serifed"
capital-q = "detached-bend-tailed"
capital-v = "curly-serifless"
capital-w = "curly-serifless"
capital-y = "curly-serifless"
capital-z = "cursive"
h = "tailed-serifless"
k = "cursive-bottom-right-serifed"
m = "short-leg-tailed-serifless"
n = "tailed-serifless"
q = "earless-rounded-tailed-serifless"
t = "bent-hook-short-neck"
v = "cursive-serifed"
w = "cursive-serifless"
z = "cursive"
micro-sign = "toothed-bottom-right-serifed"

[buildPlans.iosevka-extended.widths.normal]
shape = 600
menu = 5
css = "normal"
'';
          set = "extended";
        };
        iosevka-aile = prev.unstable.iosevka.override {
          privateBuildPlan = ''
[buildPlans.iosevka-aile]
family = "Iosevka Aile"
spacing = "quasi-proportional"
serifs = "sans"
no-cv-ss = true
export-glyph-names = false

[buildPlans.iosevka-aile.variants.design]
capital-d = "more-rounded-serifless"
capital-g = "toothless-rounded-serifless-hooked"
capital-i = "serifless"
capital-r = "standing-open-serifless"
capital-w = "straight-flat-top-serifless"
a = "single-storey-earless-corner-tailed"
b = "toothless-corner-serifless"
d = "toothless-corner-serifless"
f = "flat-hook-serifless"
g = "single-storey-earless-rounded"
i = "tailed"
j = "flat-hook-serifless"
l = "flat-tailed"
m = "short-leg-serifless"
p = "earless-rounded-serifless"
q = "earless-rounded-straight-serifless"
s = "serifless"
t = "flat-hook"
u = "toothless-rounded-serifless"
y = "cursive-serifless"
zero = "slashed-split"
four = "closed"
six = "open-contour"
seven = "bend-serifless"
eight = "two-circles"
nine = "open-contour"
paren = "large-contour"
brace = "curly-flat-boundary"
ampersand = "et-toothless-rounded"
at = "fourfold"
percent = "rings-continuous-slash"
lig-ltgteq = "slanted"
lig-neq = "more-slanted"
lig-equal-chain = "without-notch"
lig-hyphen-chain = "without-notch"

[buildPlans.iosevka-aile.variants.italic]
capital-j = "descending-serifless"
capital-q = "detached-bend-tailed"
capital-v = "curly-serifless"
capital-y = "curly-serifless"
capital-z = "cursive"
a = "single-storey-earless-corner-tailed"
h = "tailed-serifless"
i = "flat-tailed"
j = "flat-hook-serifless"
k = "cursive-bottom-right-serifed"
l = "flat-tailed"
m = "short-leg-tailed-serifless"
n = "tailed-serifless"
q = "earless-rounded-tailed-serifless"
v = "cursive-serifless"
w = "cursive-serifless"
z = "cursive"
ampersand = "et-tailed"
micro-sign = "toothed-bottom-right-serifed"'';
          set = "aile";
        };
        iosevka-aile-extended = prev.unstable.iosevka.override {
          privateBuildPlan = ''
[buildPlans.iosevka-aile-extended]
family = "Iosevka Aile Extended"
spacing = "quasi-proportional"
serifs = "sans"
no-cv-ss = true
export-glyph-names = false

[buildPlans.iosevka-aile-extended.variants.design]
capital-d = "more-rounded-serifless"
capital-g = "toothless-rounded-serifless-hooked"
capital-i = "serifless"
capital-r = "standing-open-serifless"
capital-w = "straight-flat-top-serifless"
a = "single-storey-earless-corner-tailed"
b = "toothless-corner-serifless"
d = "toothless-corner-serifless"
f = "flat-hook-serifless"
g = "single-storey-earless-rounded"
i = "tailed"
j = "flat-hook-serifless"
l = "flat-tailed"
m = "short-leg-serifless"
p = "earless-rounded-serifless"
q = "earless-rounded-straight-serifless"
s = "serifless"
t = "flat-hook"
u = "toothless-rounded-serifless"
y = "cursive-serifless"
zero = "slashed-split"
four = "closed"
six = "open-contour"
seven = "bend-serifless"
eight = "two-circles"
nine = "open-contour"
paren = "large-contour"
brace = "curly-flat-boundary"
ampersand = "et-toothless-rounded"
at = "fourfold"
percent = "rings-continuous-slash"
lig-ltgteq = "slanted"
lig-neq = "more-slanted"
lig-equal-chain = "without-notch"
lig-hyphen-chain = "without-notch"

[buildPlans.iosevka-aile-extended.variants.italic]
capital-j = "descending-serifless"
capital-q = "detached-bend-tailed"
capital-v = "curly-serifless"
capital-y = "curly-serifless"
capital-z = "cursive"
a = "single-storey-earless-corner-tailed"
h = "tailed-serifless"
i = "flat-tailed"
j = "flat-hook-serifless"
k = "cursive-bottom-right-serifed"
l = "flat-tailed"
m = "short-leg-tailed-serifless"
n = "tailed-serifless"
q = "earless-rounded-tailed-serifless"
v = "cursive-serifless"
w = "cursive-serifless"
z = "cursive"
ampersand = "et-tailed"
micro-sign = "toothed-bottom-right-serifed"

[buildPlans.iosevka-aile-extended.widths.normal]
shape = 600
menu = 5
css = "normal"'';
          set = "aile-extended";
        };
        iosevka-etoile = prev.iosevka.override {
          privateBuildPlan = {
            family = "Iosevka Etoile";
            spacing = "quasi-proportional";
            serifs = "slab";
            no-cv-ss = true;
            export-glyph-name = false;
          };
          set = "Etoile";
        };
      })
  ];
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;
  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
  ];

  networking.hostName = "nixos"; # Define your hostname.
  networking.nameservers =
    [ "1.1.1.1" "8.8.8.8" "8.8.4.4" ];
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  networking.proxy.noProxy = "127.0.0.1,localhost";

  # Enable networking
  networking.networkmanager.enable = true;
  networking.networkmanager.dns = "default";
  # networking.hosts = {
  #   "192.168.0.200" = [ "home.internal" ];
  #   "192.168.0.1" = [ "tplink.internal" ];
  # };
  # Set your time zone.
  time.timeZone = "Asia/Jakarta";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "id_ID.utf8";
    LC_IDENTIFICATION = "id_ID.utf8";
    LC_MEASUREMENT = "id_ID.utf8";
    LC_MONETARY = "id_ID.utf8";
    LC_NAME = "id_ID.utf8";
    LC_NUMERIC = "id_ID.utf8";
    LC_PAPER = "id_ID.utf8";
    LC_TELEPHONE = "id_ID.utf8";
    LC_TIME = "id_ID.utf8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Stacking window manager
  # Pantheon
  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.desktopManager.pantheon.enable = true;
  # Plasma
  services.xserver.displayManager = {
    sddm.enable = true;
    defaultSession = "none+awesome";
  };
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.desktopManager.plasma5.useQtScaling = true;

  services.xserver.windowManager.awesome = {
    enable = true;
    luaModules = with pkgs.luaPackages;
      [
        luarocks
        #luadbi-mysql
      ];
  };
  # services.xserver.desktopManager.plasma5.excludePackages =
  # with pkgs.libForQt5; [
  #   elisa
  #   okular
  #   oxygen
  #   khelpcenter
  #   konsole
  # ];

  # Tiling window managers
  # services.xserver.windowManager.exwm.enable = true;

  # Configure keymap in X11
  services.xserver = {
    exportConfiguration = true;
    layout = "us,us";
    # xkbVariant = "";
    xkbVariant = "dvorak,";
    xkbOptions = "ctrl:nocaps, grp:alt_space_toggle";
  };

  # Configure console keymap
  # console.keyMap = "dvorak";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [ cnijfilter2 cnijfilter_4_00 ];

  # Enable sound with pipewire.
  sound.enable = false;
  hardware.pulseaudio.enable = false;
  hardware.bluetooth.enable = true;
  security = {
    rtkit.enable = true;
    # acme = {
    #   acceptTerms = true;
    #   email = "ricky.anderson2696@gmail.com";
    # };
  };
  services.pipewire = {
    enable = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    # jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  users.defaultUserShell = pkgs.fish;

  virtualisation = {
    libvirtd.enable = true;
    docker = {
      enable = true;
      rootless = {
        enable = true;
        setSocketVariable = true;
      };
    };
    containerd.enable = true;
    virtualbox.host.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.rickya = {
    isNormalUser = true;
    description = "Ricky Anderson";
    shell = pkgs.fish;
    extraGroups = [ "networkmanager" "wheel" "libvirtd" "video" ];
  };

  services = {
    # dnsmasq = {
    #   enable = true;
    #   settings = {
    #     server = [ "1.1.1.1" "8.8.8.8" "8.8.4.4" ];
    #     domain-needed = true;
    #     dhcp-range = [ "192.168.0.100,192.168.0.200" ];
    #     listen-address = [ "127.0.0.1,192.168.0.200" ];
    #     expand-hosts = true;
    #     bogus-priv = true;
    #     # domain = "internal.domain";
    #     cache-size = 10000;
    #   };
    # };
    syncthing = {
      enable = true;
      user = "rickya";
      dataDir = "/home/rickya/syncthing";
      configDir = "/home/rickya/.config/syncthing";
    };
    ratbagd.enable = true;
    nextdns = {
      enable = false;
      arguments = [ "-config" "2ed52e" ];
    };
    gnome.glib-networking.enable = true;
    power-profiles-daemon.enable = false;
    tlp = {
      enable = true;
      settings = {
        START_CHARGE_THRESH_BAT0 = 0;
        STOP_CHARGE_THRESH_BAT0 = 1;
      };
    };
    blueman.enable = true;
    emacs = {
      enable = false;
      defaultEditor = true;
      package = pkgs.emacs28NativeComp;
    };
    zerotierone = {
      enable = true;
      port = 9993;
      joinNetworks = [ "159924d63096e24e" ];
    };
    # caddy = {
    #   enable = true;
    #   virtualHosts = {
    #     "nextcloud.internal.domain" = {

    #     };
    #   };
    # };
  };

  home-manager.users.rickya = let
    homeDir = dir: builtins.toPath (config.users.users.rickya.home + "/" + dir);
    confDir = dir: builtins.toPath (homeDir ("config" + "/" + dir));
    mkOutOfStoreSymlink =
      config.home-manager.users.rickya.lib.file.mkOutOfStoreSymlink;
  in rec {
    imports = [
      "${
        fetchTarball
        "https://github.com/Luis-Hebendanz/nixos-chrome-pwa/tarball/master"
      }/modules/chrome-pwa/home.nix"
    ];

    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      }))
    ];

    home.stateVersion = "23.05";
    home.sessionVariables = {
      WINIT_X11_SCALE_FACTOR = 1;
      ROFI_SYSTEMD_TERM = "alacritty -e";
      DIRENV_LOG_FORMAT = "";
      DOOMPROFILE = "default";
      DOOMDIR = "~/.doom.d";
    };
    home.enableNixpkgsReleaseCheck = true;

    fonts.fontconfig.enable = true;

    editorconfig = {
      enable = true;
      settings = {
        "*" = {
          charset = "utf-8";
          end_of_line = "lf";
          insert_final_newline = true;
          trim_trailing_whitespace = true;
        };
      };
    };

    home.packages = let
      my-node-packages =
        pkgs.callPackage (import (confDir "node-packages")) { };
    in with pkgs;
    [
      yadm
      stow
      google-chrome
      firefox-devedition
      chromium
      nyxt
      wtf
      nix-output-monitor
      # pinentry
      pinentry-qt
      # pinentry-gtk2
      # pinentry-gnome
      # pinentry-emacs
      # pinentry-curses
      cmake
      prettyping
      ngrok
      rpi-imager
      scrcpy
      android-tools
      postgresql
      mongosh
      (mysql80.override { inherit (pkgs) openssl; })
      pgcli
      mycli
      pspg
      pandoc
      nix-diff
      nixfmt
      comma
      gnumake
      libvterm
      postman
      gcc
      glances
      libtool
      ripgrep
      fd
      fx
      xclip
      piper
      zoom-us
      nextdns
      gopls
      gomodifytags
      gotests
      gotools
      gofumpt
      ginkgo
      python311
      pavucontrol
      pipecontrol
      ncpamixer
      pass-git-helper
      git-imerge
      nerdctl
      gimp
      jwt-cli
      protobuf
      buf
      sysz
      redis
      nodejs
      redpanda
      lazydocker
      docker-compose
      node2nix
      sumneko-lua-language-server
      sbcl
      roswell
      lispPackages.quicklisp
      acpi
      pnmixer
      pcmanfm
      gnome.file-roller
      alsa-tools
      alsa-utils
      inetutils
      unzip
      unrar
      unar
      rpi-imager
      shellcheck
      arandr
      httpie
      brightnessctl
      xorg.xev
      zbar
      onlyoffice-bin
      libreoffice
      poppler_utils
      poppler_data
      nss
      nssTools
      openssl
      rofi-bluetooth
      rofi-systemd
      ario
      notion-app-enhanced
      mpc-cli
      nsxiv
      lsof
      procs
      tokei
      powerstat
      imagemagick
      graphviz
      colorpicker
      xdragon
      gimp
      epdfview
      texlive.combined.scheme-small
      # freecad
      entr
      zathura
      zeal-qt6
      genymotion
      steam-tui
      steamcmd
      slack
      (let
        name = "hamsket";
        version = "0.6.3";
        src = builtins.fetchurl {
          url =
            "https://github.com/TheGoddessInari/hamsket/releases/download/${version}/Hamsket-${version}.AppImage";
        };
        desktopItem = (makeDesktopItem {
          desktopName = "Hamsket";
          name = "Hamsket";
          icon = "hamsket";
          categories = [ "Network" ];
          exec = "hamsket --disable-gpu-sandbox";
        });
        appimageContents = appimageTools.extractType2 { inherit name src; };
      in appimageTools.wrapType2 rec {
        inherit name src;
        extraInstallCommands = ''
          mkdir -p $out/share/applications $out/share/icons/hicolor/256x256/apps
          install -Dm644 ${appimageContents}/usr/share/icons/hicolor/256x256/apps/hamsket*.png $out/share/icons/hicolor/256x256/apps/hamsket.png
          install -Dm644 ${desktopItem}/share/applications/* $out/share/applications
        '';
      })
      (stdenv.mkDerivation {
        name = "cdebug";
        nativeBuildInputs = [ installShellFiles ];
        src = fetchzip {
          url =
            "https://github.com/iximiuz/cdebug/releases/latest/download/cdebug_linux_amd64.tar.gz";
          sha256 = "sha256-G8NZ6FE8yrpipqtrrSrHtoKZ0PmCISBFteYfJFLxNwA=";
          stripRoot = false;
        };
        installPhase = ''
          install -D $src/cdebug $out/bin/cdebug
        '';
        postInstall = ''
          installShellCompletion --cmd cdebug \
            --bash <($out/bin/cdebug completion bash) \
            --zsh <($out/bin/cdebug completion zsh) \
            --fish <($out/bin/cdebug completion fish)
        '';
      })
      cntr
      toolbox
      ntfs3g
      krunner-pass
      tealdeer
      glab
      grobi
      cmatrix
      trash-cli
      grpcurl
      fontpreview
      imagemagick
      fontconfig
      font-manager
      difftastic
      languagetool
      fortune
      hunspell
      hunspellDicts.en-us-large
      (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
      (nerdfonts.override {
        fonts = [
          "JetBrainsMono"
          "IBMPlexMono"
          "SourceCodePro"
          "Overpass"
          "CodeNewRoman"
        ];
      })
      alegreya
      merriweather
      nix-prefetch-scripts
      gore
      sqlc
      ldns
      dig
      (buildGoModule rec {
        name = "tern";
        src = fetchFromGitHub {
          owner = "jackc";
          repo = "tern";
          rev = "v2.0.1";
          sha256 = "sha256-DzVp3UvoiCSg5IKowKujZ+mtER5Be2sSUJ8UgkmGOvs=";
        };
        doCheck = false;
        vendorHash = "sha256-Yv3DrW3TRafa8rjVRmFDpQpN5Bc8kAc5VdEnNA0aAII=";
      })
      (buildGoModule rec {
        name = "godoctor";
        src = fetchFromGitHub {
          owner = "godoctor";
          repo = "godoctor";
          rev = "b665b8ff3f3519eb1debee2397663ec54f0327dc";
          sha256 = "sha256-jajAMVNElLU3y2GxFOoepdhlMMNpEtRUDWSrhfhfUS0=";
        };
        doCheck = false;
        vendorHash = null;
      })
      (buildGoModule rec {
        name = "goose";
        src = fetchFromGitHub {
          owner = "pressly";
          repo = "goose";
          rev = "master";
          sha256 = "sha256-DQyq/7qVvdI4uVaq75KNtsUWF6EZZBTBpoCtlgPAKus=";
        };
        doCheck = false;
        vendorHash = "sha256-nSMhz5cHPDEj7FtNBBnOhVDe9dxj9ybF30y0NpADGLo=";
      })
      (buildGoModule rec {
        name = "godocdash";
        src = fetchFromGitHub {
          owner = "wuudjac";
          repo = "godocdash";
          rev = "b80fdd9dcadcc28eb89af60f0472b7bd404386be";
          sha256 = "sha256-8cXy9/QBKukwPMH9FQUyGRxT666VoCc6hU2O6SD1Wz0=";
        };
        vendorHash = "sha256-GFc472QB4ts7fDOeG2pfWzChkMOJnEVR/8StZ9N2u4Y=";
      })
      golangci-lint

    ] ++ (with pkgs.fishPlugins; [ bass fzf-fish autopair-fish ])
    ++ (with pkgs.nodePackages; [
      npm
      yarn
      pyright
      typescript
      typescript-language-server
      create-react-app
      pkgs.nodePackages."@tailwindcss/language-server"
      mermaid-cli
      dockerfile-language-server-nodejs
    ]) ++ (with pkgs.python311Packages; [ grip pandas ]);
    programs = {
      home-manager.enable = true;
      browserpass.enable = true;
      gpg.enable = true;
      htop.enable = true;
      jq.enable = true;
      nix-index.enable = true;
      exa.enable = true;
      lsd.enable = true;
      dircolors.enable = true;
      feh.enable = true;
      obs-studio = {
        enable = true;
        plugins = with pkgs.obs-studio-plugins; [
          obs-pipewire-audio-capture
        ];
      };
      autorandr = {
        enable = true;
        profiles = let
          laptop =
            "00ffffffffffff0030e49c0600000000001e0104a51f117803adf5985e598b261c5054000000010101010101010101010101010101012e3680a070381f403020350035ae1000001a000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503134305746482d535050310018";
          home-monitor =
            "00ffffffffffff0010aca4a04c443731301b010380351e78ee7e75a755529c270f5054a54b00714f8180a9c0a940d1c0010101010101023a801871382d40582c45000f282100001e000000ff005450444b483742553137444c0a000000fc0044454c4c205532343134480a20000000fd00384c1e5311000a202020202020012502031ff14c9005040302071601141f12132309070765030c00100083010000023a801871382d40582c45000f282100001e011d8018711c1620582c25000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f282100001800000000000000000000000000000000000000000000000037";
        in {
          home = {
            fingerprint = {
              eDP-1 = laptop;
              HDMI-1 = home-monitor;
            };
            config = {
              HDMI-1 = {
                enable = true;
                mode = "1920x1080";
                crtc = 1;
                position = "0x0";
                rate = "60.00";
              };
              eDP-1 = {
                enable = true;
                crtc = 0;
                mode = "1600x900";
                position = "160x1080";
                primary = true;
                rate = "60.00";
              };
            };
          };
          on-the-go = {
            fingerprint = { "eDP-1" = laptop; };
            config = {
              eDP-1 = {
                enable = true;
                mode = "1600x900";
              };
            };
          };
        };
      };
      go = {
        enable = true;
        package = pkgs.go;
        goPath = "go";
      };
      starship = {
        enable = true;
        settings = { kubernetes.disabled = false; };
      };
      alacritty = import (confDir "alacritty") { inherit pkgs; };
      bat = {
        enable = true;
        config = {
          pager = "less --raw-control-chars";
          italic-text = "always";
          theme = "Solarized (light)";
        };
        extraPackages = with pkgs.bat-extras; [
          batman
          batpipe
          batgrep
          batdiff
          batwatch
        ];
      };
      direnv = {
        enable = true;
        nix-direnv.enable = true;
        config = {
          global = {
            strict_env = true;
            load_dotenv = true;
          };
        };
      };
      fzf = {
        enable = true;
        enableFishIntegration = false;
        defaultOptions = [
          "--cycle"
          "--layout=reverse"
          "--border"
          "--height=90%"
          "--preview-window=wrap"
          "--marker='*'"
          "--color=fg:-1,bg:-1,hl:#b58900,fg+:-1,bg+:#fdf6e3,hl+:#b58900,info:#93a1a1,prompt:#dc322f,pointer:#2aa198,marker:#859900,spinner:#6c71c4,header:#cb4b16"
        ];
        defaultCommand = "fd --type f";
      };
      fish = {
        enable = true;
        shellAliases = {
          # exa = pkgs.lib.mkForce "exa --icons";
          doom = "~/.emacs.d/bin/doom";
        };
        shellAbbrs = {
          ls = "exa";
          ll = "exa -l";
          la = "exa -a";
          lla = "exa -la";
          lt = "exa -laT";
          grep = "rg";
          mux = "tmuxinator";
        };
        interactiveShellInit = ''
          set -g fish_greetings ""
        '';
      };
      git = {
        enable = true;
        userEmail = "ricky.anderson2696@gmail.com";
        userName = "Ricky Anderson";
        signing.key = null;
        signing.signByDefault = true;
        extraConfig = {
          gitlab.user = "rickyson";
          github.user = "rickyson96";
          credential = {
            helper = "!pass-git-helper $@";
            useHttpPath = true;
          };
          http.postBuffer = 1524288000;
          branch.autoSetupMerge = true;
          init.defaultBranch = "main";
          url = { "git@bitbucket.org:".insteadOf = "https://bitbucket.org/"; };
          rerere.enabled = true;
          diff.tool = "difftastic";
          difftool.prompt = false;
          difftool."difftastic".cmd = ''
            ${pkgs.difftastic}/bin/difft --display side-by-side --background light --color auto "$LOCAL" "$REMOTE"'';
        };
        includes = [{
          condition = "gitdir:~/stockbitgroup/";
          contents = {
            user.email = "ricky.anderson@stockbit.com";
            gitlab = {
              user = "ricky.anderson";
              "gitlab.com/api".user = "ricky.anderson";
            };
          };
        }];
        ignores = [ ".dir-locals.el" ".direnv/" ".projectile" ];
        difftastic = {
          enable = false;
          display = "side-by-side-show-both";
          background = "light";
        };
      };
      emacs = {
        enable = true;
        package = pkgs.emacs29;
        extraPackages = (epkgs: [ epkgs.vterm epkgs.pdf-tools]);
      };
      topgrade = {
        enable = true;
        settings = {
          assume_yes = true;
          skip_notify = true;
        };
      };
      rofi = {
        enable = true;
        plugins = with pkgs; [ rofi-emoji rofi-calc ];
        cycle = true;
        font = "JetBrainsMono Nerd Font 10";
        terminal = "${pkgs.alacritty}/bin/alacritty";
        extraConfig = {
          modes = "drun,calc,ssh,emoji";
          show = "drun";
          show-icons = true;
        };
        pass = {
          enable = true;
          extraConfig = ''
            help_color="#4872FF"
            auto_enter='false'
            clip=clipboard
            default_do=copyMenu
          '';
        };
        theme = confDir "rofi/my-theme.rasi";
      };
      password-store = {
        enable = true;
        package = pkgs.pass.withExtensions (exts: [
          exts.pass-otp
          exts.pass-audit
          exts.pass-checkup
          exts.pass-update
        ]);
      };
      tmux = {
        enable = true;
        clock24 = true;
        keyMode = "vi";
        newSession = true;
        prefix = "C-a";
        terminal = "xterm-256color";
        plugins = with pkgs.tmuxPlugins; [
          cpu
          tmux-thumbs
          better-mouse-mode
          power-theme
        ];
        tmuxinator.enable = true;
      };
      mpv = { enable = true; };
      ncmpcpp.enable = true;
      z-lua = {
        enable = true;
        enableAliases = true;
        options = [ "enchanced" "once" "fzf" ];
      };
      qutebrowser = {
        enable = true;
        package = pkgs.qutebrowser-qt6;
        searchEngines = {
          w =
            "https://en.wikipedia.org/wiki/Special:Search?search={}&go=Go&ns0=1";
          n = "https://search.nixos.org/packages?channel=23.05&query={}";
          no = "https://search.nixos.org/options?channel=23.05&query={}";
          nw = "https://nixos.wiki/index.php?search={}";
          hm = "https://mipmip.github.io/home-manager-option-search/?query={}";
          g = "https://www.google.com/search?hl=en&q={}";
          d = "https://duckduckgo.com/?q={}";
          akops = "https://gamepress.gg/arknights/search?query={}";
          gmeet = "https://meet.google.com/{}";
          gmeetbit = "https://meet.google.com/{}?authuser=1";
          genius = "https://genius.com/search?q={}";
          shopee = "https://shopee.co.id/search?keyword={}";
        };
        quickmarks = {
          hm = "https://rycee.gitlab.io/home-manager/options.html";
          akrec = "https://aceship.github.io/AN-EN-Tags/akhr.html";
          akhr = "https://aceship.github.io/AN-EN-Tags/akhr.html";
          akplan = "https://penguin-stats.io/planner";
          aklog = "https://ark-nights.com/table";
          akfarm = "https://ark-nights.com/farming";
          sb-portal =
            "https://sites.google.com/stockbit.com/employeeportal/compensation-benefit/outpatient?pli=1&authuser=1";
        };
        keyBindings = {
          normal = {
            "J" = "tab-next";
            "K" = "tab-prev";
            "D" = "tab-close";
            "U" = "undo";
            # "d" = "run-with-count 10 scroll down";
            # "u" = "run-with-count 10 scroll up";
            "d" = "scroll-page 0 0.5";
            "u" = "scroll-page 0 -0.5";
            "tt" = "config-cycle -t tabs.show switching always";
            "tp" =
              "config-cycle -t -p content.proxy system http://192.168.195.167:8181";
            "<ctrl+d>" = "scroll-page 0 0.25";
            "<ctrl+u>" = "scroll-page 0 -0.25";
            "zll" = "spawn -u qute-pass";
            "zlL" = "spawn -u qute-pass --unfiltered";
            "zlu" = "spawn -u qute-pass --username-only";
            "zlU" = "spawn -u qute-pass --unfiltered --username-only";
            "zlp" = "spawn -u qute-pass --password-only";
            "zlP" = "spawn -u qute-pass --unfiltered --password-only";
            "zlo" = "spawn -u qute-pass --otp-only";
            "zlO" = "spawn -u qute-pass --unfiltered --otp-only";
            "zm" = "spawn umpv {url}";
            "zM" = "hint links spawn umpv {hint-url}";
          };
          command = {
            "<Ctrl+j>" = "completion-item-focus next";
            "<Ctrl+k>" = "completion-item-focus prev";
          };
        };
        extraConfig = ''
          c.tabs.padding = ${
            builtins.toJSON {
              bottom = 5;
              top = 5;
              left = 0;
              right = 5;
            }
          }
        '';
        settings = let
          base00 = "#fdf6e3";
          base01 = "#eee8d5";
          base02 = "#93a1a1";
          base03 = "#839496";
          base04 = "#657b83";
          base05 = "#586e75";
          base06 = "#073642";
          base07 = "#002b36";
          base08 = "#dc322f";
          base09 = "#cb4b16";
          base0A = "#b58900";
          base0B = "#859900";
          base0C = "#2aa198";
          base0D = "#268bd2";
          base0E = "#6c71c4";
          base0F = "#d33682";
        in {
          # workaround for https://github.com/qutebrowser/qutebrowser/issues/7489
          qt.force_software_rendering = "chromium";
          # qt.chromium.low_end_device_mode = "never";

          completion.shrink = true;
          scrolling.smooth = true;
          content.javascript.clipboard = "access";

          fonts.default_family = "CodeNewRoman Nerd Font";
          fonts.default_size = "12pt";

          fonts.tabs.selected = "default_size default_family";
          fonts.tabs.unselected = "default_size default_family";

          tabs.title.format = "{audio}{current_title}";
          tabs.title.format_pinned = "{audio}{current_title}";
          tabs.show = "switching";
          tabs.position = "left";

          tabs.favicons.scale = 1.0;
          colors.completion.fg = base05;
          colors.completion.odd.bg = base00;
          colors.completion.even.bg = base00;
          colors.completion.category.fg = base0D;
          colors.completion.category.bg = base00;
          colors.completion.category.border.top = base00;
          colors.completion.category.border.bottom = base00;
          colors.completion.item.selected.fg = base05;
          colors.completion.item.selected.bg = base01;
          colors.completion.item.selected.border.top = base01;
          colors.completion.item.selected.border.bottom = base01;
          colors.completion.item.selected.match.fg = base0C;
          colors.completion.match.fg = base0C;
          colors.completion.scrollbar.fg = base05;
          colors.completion.scrollbar.bg = base00;
          colors.contextmenu.disabled.bg = base01;
          colors.contextmenu.disabled.fg = base04;
          colors.contextmenu.menu.bg = base00;
          colors.contextmenu.menu.fg = base05;
          colors.contextmenu.selected.bg = base02;
          colors.contextmenu.selected.fg = base05;
          colors.downloads.bar.bg = base00;
          colors.downloads.start.fg = base00;
          colors.downloads.start.bg = base0D;
          colors.downloads.stop.fg = base00;
          colors.downloads.stop.bg = base0C;
          colors.downloads.error.fg = base08;
          colors.hints.fg = base00;
          colors.hints.bg = base0A;
          colors.hints.match.fg = base05;
          colors.keyhint.fg = base05;
          colors.keyhint.suffix.fg = base05;
          colors.keyhint.bg = base00;
          colors.messages.error.fg = base00;
          colors.messages.error.bg = base08;
          colors.messages.error.border = base08;
          colors.messages.warning.fg = base00;
          colors.messages.warning.bg = base0E;
          colors.messages.warning.border = base0E;
          colors.messages.info.fg = base05;
          colors.messages.info.bg = base00;
          colors.messages.info.border = base00;
          colors.prompts.fg = base05;
          colors.prompts.border = base00;
          colors.prompts.bg = base00;
          colors.prompts.selected.bg = base02;
          colors.prompts.selected.fg = base05;
          colors.statusbar.normal.fg = base0B;
          colors.statusbar.normal.bg = base00;
          colors.statusbar.insert.fg = base00;
          colors.statusbar.insert.bg = base0D;
          colors.statusbar.passthrough.fg = base00;
          colors.statusbar.passthrough.bg = base0C;
          colors.statusbar.private.fg = base00;
          colors.statusbar.private.bg = base01;
          colors.statusbar.command.fg = base04;
          colors.statusbar.command.bg = base01;
          colors.statusbar.command.private.fg = base0E;
          colors.statusbar.command.private.bg = base01;
          colors.statusbar.caret.fg = base00;
          colors.statusbar.caret.bg = base0E;
          colors.statusbar.caret.selection.fg = base00;
          colors.statusbar.caret.selection.bg = base0D;
          colors.statusbar.progress.bg = base0D;
          colors.statusbar.url.fg = base05;
          colors.statusbar.url.error.fg = base08;
          colors.statusbar.url.hover.fg = base05;
          colors.statusbar.url.success.http.fg = base0F;
          colors.statusbar.url.success.https.fg = base0B;
          colors.statusbar.url.warn.fg = base08;
          colors.tabs.bar.bg = base00;
          colors.tabs.indicator.start = base0D;
          colors.tabs.indicator.stop = base0C;
          colors.tabs.indicator.error = base08;
          colors.tabs.odd.fg = base05;
          colors.tabs.odd.bg = base00;
          colors.tabs.even.fg = base05;
          colors.tabs.even.bg = base00;
          colors.tabs.pinned.even.bg = base0B;
          colors.tabs.pinned.even.fg = base00;
          colors.tabs.pinned.odd.bg = base0B;
          colors.tabs.pinned.odd.fg = base00;
          colors.tabs.pinned.selected.even.bg = base02;
          colors.tabs.pinned.selected.even.fg = base05;
          colors.tabs.pinned.selected.odd.bg = base02;
          colors.tabs.pinned.selected.odd.fg = base05;
          colors.tabs.selected.odd.fg = base05;
          colors.tabs.selected.odd.bg = base01;
          colors.tabs.selected.even.fg = base05;
          colors.tabs.selected.even.bg = base01;
          colors.webpage.bg = "#FFFFFF";
        };
      };
    };
    services = {
      blueman-applet.enable = true;
      chrome-pwa.enable = true;
      network-manager-applet.enable = true;
      dunst = {
        enable = true;
        settings = {
          global = {
            width = 300;
            height = 300;
            offset = "30x50";
            origin = "top-right";
            transparency = 10;
            frame_color = "#eceff1";
            font = "Droid Sans 9";
          };

          urgency_normal = {
            background = "#37474f";
            foreground = "#eceff1";
            timeout = 5;
          };
        };
      };
      # grobi = {
      #   enable = true;
      #   rules = [
      #     {
      #       name = "home";
      #       outputs_connected = [ "HDMI-1" ];
      #       configure_column = [ "HDMI-1" "eDP-1@1600x900" ];
      #     }
      #     {
      #       name = "on-the-go";
      #       outputs_disconnected = [ "HDMI-1" ];
      #       configure_single = "eDP-1@1600x900";
      #     }
      #     {
      #       name = "fallback";
      #       configure_single = "eDP-1";
      #     }
      #   ];
      # };
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        pinentryFlavor = "qt";
        extraConfig = ''
          allow-loopback-pinentry
        '';
      };
      emacs = {
        enable = true;
        package = pkgs.emacs28NativeComp;
        client.enable = true;
        defaultEditor = true;
      };
      picom = {
        enable = true;
        backend = "glx";
        activeOpacity = 1;
        inactiveOpacity = 1;
        shadow = true;
        shadowOpacity = 0.9;
        fade = true;
        fadeDelta = 4;
        settings = {
          glx-no-stencil = true;
          glx-copy-from-front = false;
          glx-no-rebind-pixmap = true;
          use-damage = true;
          corner-radius = 10;
          frame.opacity = 0.8;
          # blur-background = true;
          blur-method = "dual_kawase";
          blur-strength = 6;
          blur-size = 12;
        };
      };
      flameshot = {
        enable = true;
        settings = {
          General = {
            uiColor = "#2aa198";
            contrastUiColor = "#073642";
            drawColor = "#2aa198";
          };
        };
      };
      mopidy = {
        enable = true;
        extensionPackages = with pkgs; [
          mopidy-iris
          mopidy-ytmusic
          mopidy-mpd
        ];
        settings = {
          ytmusic = {
            enabled = true;
            auth_json = "/home/rickya/.password-secret/ytmusic/auth.json";
          };
          mpd = {
            hostname = "::";
            port = "6600";
          };
        };
      };
      xcape = {
        enable = true;
        mapExpression = { Control_L = "Escape"; };
      };
      screen-locker = {
        enable = true;
        inactiveInterval = 1;
        lockCmd = "\${pkgs.i3lock}/bin/i3lock -n -c 000000";
      };
      pass-secret-service.enable = true;
    };
    xdg = {
      configFile = {
        "wtf".source = confDir "wtf";
        "pass-git-helper".source = confDir "pass-git-helper";
        "awesome/awesome-wm-widgets".source = pkgs.fetchFromGitHub {
          owner = "streetturtle";
          repo = "awesome-wm-widgets";
          rev = "master";
          sha256 = "sha256-zXAiEeAJibdmF5lpSJc7zZrgUJP/+l/dKcBR1HdUf9k=";
        };
        "awesome/rc.lua".source =
          mkOutOfStoreSymlink (confDir "awesome/rc.lua");
        "awesome/theme.lua".source =
          mkOutOfStoreSymlink (confDir "awesome/theme.lua");
        "pgcli/config".source = confDir "pgcli/config";
        "mycli/config".source = confDir "mycli/config";
      };
      dataFile = {
        "Zeal/Zeal/docsets/Git.docset".source = "${
            pkgs.fetchFromGitHub {
              owner = "iamaziz";
              repo = "git-dash";
              rev = "master";
              sha256 = "sha256-ZYuX6dWSyCFJTM6wxd7NQ3tExp5voG47ubjKLXbVovM=";
            }
          }/Git.docset";
      };
      desktopEntries = {
        org-protocol = {
          name = "org-protocol";
          comment =
            "Intercept calls from emacsclient to trigger custom actions";
          categories = [ "X-Other" ];
          icon = "emacs";
          type = "Application";
          exec = "emacsclient -- %u";
          terminal = false;
          mimeType = [ "x-scheme-handler/org-protocol" ];
          settings = {
            Keywords = "org-protocol";
            StartupWMClass = "Emacs";
          };
        };
        emacs-wasteland = {
          name = "Emacs Wasteland";
          genericName = "Text Editor";
          exec = "emacs --init-directory=\"~/.wasteland.emacs.d\"";
          icon = "emacs";
        };
        rofi-systemd = {
          name = "Rofi Systemd";
          exec = "rofi-systemd";
          icon = "rofi";
        };
        rofi-bluetooth = {
          name = "Rofi Bluetooth";
          genericName = "Bluetooth Manager";
          exec = "rofi-bluetooth";
          icon = "rofi";
        };
        rofi-pass = {
          name = "Rofi Pass";
          genericName = "Password Manager";
          exec = "rofi-pass";
          icon = "rofi";
        };
        sysz = {
          name = "sysz";
          genericName = "Systemd";
          exec = "sysz";
          terminal = true;
        };
      };
    };
    xsession = {
      enable = true;
      initExtra = ''
        export PASSWORD_STORE_DIR="${(homeDir ".local/share/password-store")}"
      '';
      # windowManager = { bspwm = { enable = true; }; };
    };
  };

  fonts.fonts = with pkgs;
    [
      (nerdfonts.override {
        fonts = [
          "JetBrainsMono"
          "IBMPlexMono"
          "SourceCodePro"
          "Overpass"
          "CodeNewRoman"
        ];
      })
      # (google-fonts.override { fonts = [ "PlusJakartaSans" "Quicksand" ]; })
      iosevka-normal
      iosevka-extended
      iosevka-aile
      iosevka-aile-extended
      iosevka-etoile
    ];

  programs = {
    fish.enable = true;
    dconf.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall =
        true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall =
        true; # Open ports in the firewall for Source Dedicated Server
    };
  };
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
      vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      virt-manager
      #  wget
    ];
  environment.pathsToLink = [ "/share/fish" ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.cores = 2;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 53 ];
    allowedUDPPorts = [ 53 ];
  };
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
