{ pkgs, ... }: {
  # デスクトップ環境のみで利用するパッケージ
  home.packages = with pkgs; [
    brightnessctl
    dbeaver-bin
    fcitx5
    fcitx5-gtk
    fcitx5-skk
    firefox
    fuzzel # アプリケーションランチャー
    iw
    libreoffice-fresh
    libsForQt5.fcitx5-qt
    libsForQt5.qt5ct
    mako # デスクトップ通知
    meld
    networkmanagerapplet
    niri
    nwg-look # GTK ルック & フィール設定
    podman-compose
    qt6Packages.fcitx5-qt
    skkDictionaries.assoc
    skkDictionaries.emoji
    skkDictionaries.fullname
    skkDictionaries.geo
    skkDictionaries.itaiji
    skkDictionaries.itaiji_jis3_4
    skkDictionaries.jinmei
    skkDictionaries.jis2
    skkDictionaries.jis2004
    skkDictionaries.l
    skkDictionaries.law
    skkDictionaries.mazegaki
    skkDictionaries.station
    skkDictionaries.zipcode
    skktools
    swaybg # 壁紙管理
    swaylock # スクリーンロック
    vivaldi
    vlc
    xdg-desktop-portal
    xfsprogs
    xremap
    xwayland-satellite
    waybar
    waynaptics # overlay
    zola # ブログ用
  ];

  # xremap は systemd のユーザーサービスで起動する
  systemd.user.services.xremap = {
    Install = {
      WantedBy = [
        "graphical-session.target"
      ];
    };
    Service = {
      ExecStart = "${pkgs.xremap}/bin/xremap %h/.config/xremap/config.yml";
      ExecStop = "${pkgs.killall}/bin/killall xremap";
      KillMode = "process";
      Restart = "always";
    };
    Unit = {
      After = [
        "graphical-session.target"
      ];
      Description = "xremap";
    };
  };

  # fuzzel
  xdg.configFile."fuzzel/fuzzel.ini".source = ./xdg-config/fuzzel/fuzzel.ini;
  xdg.configFile."fuzzel/cappuccin-mocha.ini".source = pkgs.catppuccin-fuzzel-blue;

  # mako
  xdg.configFile."mako/config".source = ./xdg-config/mako/config;

  # niri
  xdg.configFile."niri/config.kdl".source = ./xdg-config/niri/config.kdl;

  # waybar
  xdg.configFile."waybar/config.jsonc".source = ./xdg-config/waybar/config.jsonc;
  xdg.configFile."waybar/style.css".source = ./xdg-config/waybar/style.css;
  xdg.configFile."waybar/mocha.css".source = pkgs.catppuccin-waybar;

  # xremap
  xdg.configFile."xremap/config.yml".source = ./xdg-config/xremap/config.yml;
}
