{ pkgs, ... }: {
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
    networkmanagerapplet
    nwg-look # GTK ルック & フィール設定
    podman-compose
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
    Service = {
      ExecStart = "${pkgs.swaybg}/bin/swaybg -m fill -i \"%h/Pictures/wallpaper-catppuccin.png\"";
      Restart = "on-failure";
    };
    Unit = {
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
      Requisuite = [ "graphical-session.target" ];
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
}
