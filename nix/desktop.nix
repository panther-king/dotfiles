{ pkgs, ... }: {
  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };

  # デスクトップ環境のみで利用するパッケージ
  home.packages = with pkgs; [
    brightnessctl
    dbeaver-bin
    firefox
    fuzzel # アプリケーションランチャー
    imv # 画像ビューワー
    iw
    libreoffice-fresh
    mako # デスクトップ通知
    meld
    nemo
    networkmanagerapplet
    nwg-look # GTK ルック & フィール設定
    podman-compose
    skktools
    swaybg # 壁紙管理
    swaylock # スクリーンロック
    vivaldi
    vlc
    xdg-desktop-portal
    xfsprogs
    xwayland-satellite
    waybar
    waynaptics # overlay
    zola # ブログ用
  ];

  home.file."Pictures/wallpaper-catppuccin.png".source = ./wallpaper-catppuccin.png;

  services.xremap = {
    enable = true;
    withNiri = true;
    yamlConfig = builtins.readFile ./xdg-config/xremap/config.yml;
  };

  # swaybg は systemd のユーザーサービスで管理する
  systemd.user.services.swaybg = {
    Install = {
      WantedBy = [
        "graphical-session.target"
      ];
    };
    Service = {
      ExecStart = "${pkgs.swaybg}/bin/swaybg -m fill -i \"%h/Pictures/wallpaper-catppuccin.png\"";
      Restart = "on-failure";
    };
    Unit = {
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
      Requisite = [ "graphical-session.target" ];
    };
  };

  # waybar は systemd のユーザーサービスで管理する
  systemd.user.services.waybar = {
    Install = {
      WantedBy = [
        "graphical-session.target"
      ];
    };
    Service = {
      ExecStart = "${pkgs.waybar}/bin/waybar";
      Restart = "on-failure";
    };
    Unit = {
      After = [
        "graphical-session.target"
      ];
      PartOf = [ "graphical-session.target" ];
    };
  };

  # fuzzel
  xdg.configFile."fuzzel/fuzzel.ini".source = ./xdg-config/fuzzel/fuzzel.ini;
  xdg.configFile."fuzzel/catppuccin-mocha.ini".source = pkgs.catppuccin-fuzzel-blue;

  # mako
  xdg.configFile."mako/config".source = ./xdg-config/mako/config;

  # niri
  xdg.configFile."niri/config.kdl".source = ./xdg-config/niri/config.kdl;

  # waybar
  xdg.configFile."waybar/config.jsonc".source = ./xdg-config/waybar/config.jsonc;
  xdg.configFile."waybar/style.css".source = ./xdg-config/waybar/style.css;
  xdg.configFile."waybar/mocha.css".source = pkgs.catppuccin-waybar;
}
