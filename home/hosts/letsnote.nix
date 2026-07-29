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
    meld
    nemo
    networkmanagerapplet
    nwg-look # GTK ルック & フィール設定
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

  home.file."Pictures/wallpaper-catppuccin.png".source = ../config/wallpaper-catppuccin.png;

  # Wayland 用の Emacs を利用する
  my.emacs.package = pkgs.emacs31-pgtk;

  # デスクトップ通知
  services.mako = {
    enable = true;
    settings = {
      background-color = "#89b4fa";
      border-color = "#89b4fa";
      default-timeout = 10000;
      font = "BIZ UDPGothic 10";
      padding = 8;
      progress-color = "over #313244";
      text-color = "#1e1e2e";
      "urgency=high" = {
        background-color = "#fab387";
        border-color = "#fab387";
        text-color = "#1e1e2e";
      };
    };
  };

  services.xremap = {
    enable = true;
    withNiri = true;
    yamlConfig = builtins.readFile ../config/xremap/config.yml;
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
  xdg.configFile."fuzzel/fuzzel.ini".source = ../config/fuzzel/fuzzel.ini;
  xdg.configFile."fuzzel/catppuccin-mocha.ini".source = pkgs.catppuccin-fuzzel-blue;

  # niri
  xdg.configFile."niri/config.kdl".source = ../config/niri/config.kdl;

  # waybar
  xdg.configFile."waybar/config.jsonc".source = ../config/waybar/config.jsonc;
  xdg.configFile."waybar/style.css".source = ../config/waybar/style.css;
  xdg.configFile."waybar/mocha.css".source = pkgs.catppuccin-waybar;
}
